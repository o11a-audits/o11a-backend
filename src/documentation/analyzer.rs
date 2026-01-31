use crate::core;
use crate::core::topic;
use crate::core::{
  AST, AuditData, DataContext, Node, Scope, TopicMetadata, UnnamedTopicKind,
};
use crate::documentation::parser::{self, DocumentationAST, DocumentationNode};
use std::path::Path;

/// Scope context passed through the recursive processing
/// For documentation: container = file, component = section, member = paragraph, semantic_block = sentence
struct ScopeContext {
  container: core::ProjectPath,
  component: Option<topic::Topic>,      // Section
  member: Option<topic::Topic>,         // Paragraph
  semantic_block: Option<topic::Topic>, // Sentence
}

impl ScopeContext {
  fn new(container: core::ProjectPath) -> Self {
    Self {
      container,
      component: None,
      member: None,
      semantic_block: None,
    }
  }

  fn with_component(&self, component: topic::Topic) -> Self {
    Self {
      container: self.container.clone(),
      component: Some(component),
      member: None,
      semantic_block: None,
    }
  }

  fn with_member(&self, member: topic::Topic) -> Self {
    Self {
      container: self.container.clone(),
      component: self.component.clone(),
      member: Some(member),
      semantic_block: None,
    }
  }

  fn with_semantic_block(&self, semantic_block: topic::Topic) -> Self {
    Self {
      container: self.container.clone(),
      component: self.component.clone(),
      member: self.member.clone(),
      semantic_block: Some(semantic_block),
    }
  }

  /// Build the appropriate Scope based on current context
  fn to_scope(&self) -> Scope {
    match (&self.component, &self.member, &self.semantic_block) {
      (None, _, _) => Scope::Container {
        container: self.container.clone(),
      },
      (Some(component), None, _) => Scope::Component {
        container: self.container.clone(),
        component: component.clone(),
      },
      (Some(component), Some(member), None) => Scope::Member {
        container: self.container.clone(),
        component: component.clone(),
        member: member.clone(),
      },
      (Some(component), Some(member), Some(semantic_block)) => {
        Scope::SemanticBlock {
          container: self.container.clone(),
          component: component.clone(),
          member: member.clone(),
          semantic_block: semantic_block.clone(),
        }
      }
    }
  }
}

/// Analyzes documentation files and integrates them with the solidity DataContext
/// This MUST be called after solidity analysis completes, as it needs the solidity
/// declarations to resolve inline code references
pub fn analyze(
  project_root: &Path,
  audit_id: &str,
  data_context: &mut DataContext,
) -> Result<(), String> {
  // Get the audit data
  let audit_data = data_context
    .get_audit_mut(audit_id)
    .ok_or_else(|| format!("Audit '{}' not found", audit_id))?;

  // Parse all markdown files (passing audit_data for inline code resolution)
  let ast_map = parser::process(project_root, &audit_data)?;

  // Process each markdown file and add nodes/declarations to the audit data
  for (project_path, asts) in &ast_map {
    for ast in asts {
      // Add to in_scope_files
      audit_data.in_scope_files.insert(project_path.clone());

      // Add to asts map with stubbed nodes
      let stubbed_ast = DocumentationAST {
        nodes: ast
          .nodes
          .iter()
          .map(|n| parser::children_to_stubs(n.clone()))
          .collect(),
        project_path: project_path.clone(),
        source_content: ast.source_content.clone(),
      };
      audit_data
        .asts
        .insert(project_path.clone(), AST::Documentation(stubbed_ast));

      process_documentation_ast(ast, project_path, audit_data)?;
    }
  }

  Ok(())
}

fn process_documentation_ast(
  ast: &DocumentationAST,
  project_path: &core::ProjectPath,
  audit_data: &mut AuditData,
) -> Result<(), String> {
  let scope_ctx = ScopeContext::new(project_path.clone());

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(node, &scope_ctx, audit_data)?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  scope_ctx: &ScopeContext,
  audit_data: &mut AuditData,
) -> Result<(), String> {
  let topic = topic::new_documentation_topic(node.node_id());

  match node {
    DocumentationNode::Root { children, .. } => {
      // Root is scoped at container level
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationRoot,
          scope,
        },
      );

      // Add the Root node to the audit data
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(child, scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::Section {
      header, children, ..
    } => {
      // Section uses the header's topic as the component
      let header_topic = topic::new_documentation_topic(header.node_id());

      // Process the header node first (which creates the section declaration)
      process_documentation_node(header, scope_ctx, audit_data)?;

      // Create new scope context with this section as the component
      let section_scope_ctx = scope_ctx.with_component(header_topic);

      // Process section children with the section as component
      for child in children {
        process_documentation_node(child, &section_scope_ctx, audit_data)?;
      }

      // Add the section node itself with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );
    }

    DocumentationNode::Heading { children, .. } => {
      // Headings create section declarations (components) - scoped at container level
      let scope = Scope::Container {
        container: scope_ctx.container.clone(),
      };

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationSection,
          scope,
        },
      );

      // Add the heading node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process heading children (inline formatting nodes)
      for child in children {
        process_documentation_node(child, scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      // Paragraphs are members - scoped at component level (within a section)
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationParagraph,
          scope,
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Create new scope context with this paragraph as the member
      let paragraph_scope_ctx = scope_ctx.with_member(topic.clone());

      // Process children with this paragraph as the member
      for child in children {
        process_documentation_node(child, &paragraph_scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::Sentence { children, .. } => {
      // Sentences are semantic blocks - scoped at member level (within a paragraph)
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationSentence,
          scope,
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Create new scope context with this sentence as the semantic block
      let sentence_scope_ctx = scope_ctx.with_semantic_block(topic.clone());

      // Process children with this sentence as the semantic block
      for child in children {
        process_documentation_node(child, &sentence_scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::CodeBlock { .. } => {
      // Code blocks are scoped at current level
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationCodeBlock,
          scope,
        },
      );

      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
    }

    DocumentationNode::List { children, .. } => {
      // Lists are scoped at current level
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationList,
          scope,
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(child, scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::BlockQuote { children, .. } => {
      // Block quotes are scoped at current level
      let scope = scope_ctx.to_scope();

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationBlockQuote,
          scope,
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(child, scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::InlineCode { .. } => {
      // Add the inline code node (no topic_metadata, just the node)
      // Documentation references are tracked via the referenced_declaration field
      audit_data
        .nodes
        .insert(topic.clone(), Node::Documentation(node.clone()));
    }

    // For all other node types, just add them to the nodes map
    DocumentationNode::Text { .. }
    | DocumentationNode::ThematicBreak { .. } => {
      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
    }

    // For nodes with children that don't create topic_metadata, add the node and recurse
    DocumentationNode::ListItem { children, .. }
    | DocumentationNode::Emphasis { children, .. }
    | DocumentationNode::Strong { children, .. }
    | DocumentationNode::Link { children, .. } => {
      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(child, scope_ctx, audit_data)?;
      }
    }

    DocumentationNode::Stub { .. } => {
      // Stubs are already processed, skip
    }
  }

  Ok(())
}
