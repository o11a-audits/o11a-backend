use crate::core;
use crate::core::topic;
use crate::core::{
  AST, AuditData, DataContext, Node, Scope, TopicMetadata, UnnamedTopicKind,
};
use crate::documentation::parser::{self, DocumentationAST, DocumentationNode};
use std::path::Path;

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
  let scope = Scope::Container {
    container: project_path.clone(),
  };

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(node, &scope, audit_data)?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  scope: &Scope,
  audit_data: &mut AuditData,
) -> Result<(), String> {
  let topic = topic::new_documentation_topic(node.node_id());

  match node {
    DocumentationNode::Root { children, .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationRoot,
          scope: scope.clone(),
        },
      );

      // Add the Root node to the audit data
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(child, scope, audit_data)?;
      }
    }

    DocumentationNode::Section {
      header, children, ..
    } => {
      // Add topic metadata for the section block itself
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationSection,
          scope: scope.clone(),
        },
      );

      // Process the header node first (which creates the heading declaration)
      process_documentation_node(header, scope, audit_data)?;

      // Create nested scope with section (heading) as the component
      let header_topic = topic::new_documentation_topic(header.node_id());
      let section_scope = core::add_to_scope(scope, header_topic);

      // Process section children with the nested scope
      for child in children {
        process_documentation_node(child, &section_scope, audit_data)?;
      }

      // Add the section node itself with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );
    }

    DocumentationNode::Heading { children, .. } => {
      // Headings create section declarations - scoped at current level
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationHeading,
          scope: scope.clone(),
        },
      );

      // Add the heading node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process heading children (inline formatting nodes)
      for child in children {
        process_documentation_node(child, scope, audit_data)?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationParagraph,
          scope: scope.clone(),
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Create nested scope with paragraph as the member
      let paragraph_scope = core::add_to_scope(scope, topic.clone());

      // Process children with nested scope
      for child in children {
        process_documentation_node(child, &paragraph_scope, audit_data)?;
      }
    }

    DocumentationNode::Sentence { children, .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationSentence,
          scope: scope.clone(),
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Create nested scope with sentence as the semantic block
      let sentence_scope = core::add_to_scope(scope, topic.clone());

      // Process children with nested scope
      for child in children {
        process_documentation_node(child, &sentence_scope, audit_data)?;
      }
    }

    DocumentationNode::CodeBlock { .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationCodeBlock,
          scope: scope.clone(),
        },
      );

      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
    }

    DocumentationNode::List { children, .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationList,
          scope: scope.clone(),
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(child, scope, audit_data)?;
      }
    }

    DocumentationNode::BlockQuote { children, .. } => {
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationBlockQuote,
          scope: scope.clone(),
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(child, scope, audit_data)?;
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

      // Process children with the same scope
      for child in children {
        process_documentation_node(child, scope, audit_data)?;
      }
    }

    DocumentationNode::Stub { .. } => {
      // Stubs are already processed, skip
    }
  }

  Ok(())
}
