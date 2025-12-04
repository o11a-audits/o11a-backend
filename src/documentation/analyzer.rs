use crate::core;
use crate::core::topic;
use crate::core::{
  AST, AuditData, DataContext, Declaration, DeclarationKind, Node, Scope,
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
  // Store source content for this file
  audit_data
    .source_content
    .insert(project_path.clone(), ast.source_content.clone());

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(
      node,
      project_path,
      None, // No section context initially
      None, // No paragraph context initially
      audit_data,
    )?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  project_path: &core::ProjectPath,
  current_section: Option<&topic::Topic>,
  current_paragraph: Option<&topic::Topic>,
  audit_data: &mut AuditData,
) -> Result<(), String> {
  let node_id = node.node_id();
  let topic = topic::new_node_topic(node_id);

  match node {
    DocumentationNode::Root { children, .. } => {
      // Add the Root node to the audit data
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(
          child,
          project_path,
          current_section,
          current_paragraph,
          audit_data,
        )?;
      }
    }

    DocumentationNode::Section {
      header, children, ..
    } => {
      // Extract the heading topic ID from the header
      let header_topic = topic::new_node_topic(header.node_id());

      // Process the header node (which is a Heading)
      process_documentation_node(
        header,
        project_path,
        current_section,
        current_paragraph,
        audit_data,
      )?;

      // Process section children with this section as the current section
      for child in children {
        process_documentation_node(
          child,
          project_path,
          Some(&header_topic),
          None, // Reset paragraph context when entering new section
          audit_data,
        )?;
      }

      // Add the section node itself with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );
    }

    DocumentationNode::Heading { children, .. } => {
      // Headings create section declarations (components in scope)
      let scope = Scope::Container {
        container: project_path.clone(),
      };

      // Extract heading text from children for the name
      let name = format!("Header {}", topic.id);

      audit_data.declarations.insert(
        topic.clone(),
        Declaration {
          topic: topic.clone(),
          declaration_kind: DeclarationKind::DocumentationSection,
          name,
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
        process_documentation_node(
          child,
          project_path,
          current_section,
          current_paragraph,
          audit_data,
        )?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      let name = format!("Paragraph {}", topic.id);
      // Paragraphs become paragraph declarations (members in scope)
      let scope = Scope::Component {
        container: project_path.clone(),
        component: topic.clone(),
      };

      audit_data.declarations.insert(
        topic.clone(),
        Declaration {
          topic: topic.clone(),
          declaration_kind: DeclarationKind::DocumentationParagraph,
          name,
          scope,
        },
      );

      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with this paragraph as the current paragraph
      for child in children {
        process_documentation_node(
          child,
          project_path,
          current_section,
          Some(&topic),
          audit_data,
        )?;
      }
    }

    DocumentationNode::InlineCode {
      referenced_declaration,
      ..
    } => {
      // Add the inline code node
      audit_data
        .nodes
        .insert(topic.clone(), Node::Documentation(node.clone()));

      // If there's a referenced declaration, create a reference
      if let Some(solidity_topic) = referenced_declaration {
        // Create a reference from the solidity declaration to this inline code
        let references = audit_data
          .references
          .entry(solidity_topic.clone())
          .or_insert_with(Vec::new);

        // Add this inline code as a reference to the declaration
        if !references.contains(&topic) {
          references.push(topic.clone());
        }
      }
    }

    // For all other node types, just add them to the nodes map
    DocumentationNode::Text { .. }
    | DocumentationNode::CodeBlock { .. }
    | DocumentationNode::ThematicBreak { .. } => {
      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
    }

    // For nodes with children, add the node and recurse
    DocumentationNode::Sentence { children, .. }
    | DocumentationNode::List { children, .. }
    | DocumentationNode::ListItem { children, .. }
    | DocumentationNode::Emphasis { children, .. }
    | DocumentationNode::Strong { children, .. }
    | DocumentationNode::Link { children, .. }
    | DocumentationNode::BlockQuote { children, .. } => {
      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(
          child,
          project_path,
          current_section,
          current_paragraph,
          audit_data,
        )?;
      }
    }

    DocumentationNode::Stub { .. } => {
      // Stubs are already processed, skip
    }
  }

  Ok(())
}
