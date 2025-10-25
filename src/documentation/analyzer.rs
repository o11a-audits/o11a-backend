use crate::data_context::{
  DataContext, Declaration, DeclarationKind, Node, Scope,
};
use crate::documentation::collaborator;
use crate::documentation::parser::{self, DocumentationAST, DocumentationNode};
use std::path::Path;

/// Analyzes documentation files and integrates them with the solidity DataContext
/// This MUST be called after solidity analysis completes, as it needs the solidity
/// declarations to resolve inline code references
pub fn analyze(
  project_root: &Path,
  mut data_context: DataContext,
) -> Result<DataContext, String> {
  // Parse all markdown files (passing data_context for inline code resolution)
  let ast_map = parser::process(project_root, &data_context)?;

  // Process each markdown file and add nodes/declarations to the data context
  for (file_path, asts) in &ast_map {
    for ast in asts {
      process_documentation_ast(ast, file_path, &mut data_context)?;
    }
  }

  Ok(data_context)
}

fn process_documentation_ast(
  ast: &DocumentationAST,
  file_path: &str,
  data_context: &mut DataContext,
) -> Result<(), String> {
  // Store source content for this file
  data_context
    .source_content
    .insert(file_path.to_string(), ast.source_content.clone());

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(
      node,
      file_path,
      None, // No section context initially
      None, // No paragraph context initially
      data_context,
    )?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  file_path: &str,
  current_section: Option<&str>,
  current_paragraph: Option<&str>,
  data_context: &mut DataContext,
) -> Result<(), String> {
  let node_id = node.node_id();
  let topic_id = collaborator::node_id_to_topic_id(node_id);

  match node {
    DocumentationNode::Root { children, .. } => {
      // Add the Root node to the data context
      data_context.nodes.insert(
        topic_id.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(
          child,
          file_path,
          current_section,
          current_paragraph,
          data_context,
        )?;
      }
    }

    DocumentationNode::Section {
      header, children, ..
    } => {
      // Extract the heading topic ID from the header
      let header_topic_id = collaborator::node_id_to_topic_id(header.node_id());

      // Process the header node (which is a Heading)
      process_documentation_node(
        header,
        file_path,
        current_section,
        current_paragraph,
        data_context,
      )?;

      // Process section children with this section as the current section
      for child in children {
        process_documentation_node(
          child,
          file_path,
          Some(&header_topic_id),
          None, // Reset paragraph context when entering new section
          data_context,
        )?;
      }

      // Add the section node itself with children converted to stubs
      data_context.nodes.insert(
        topic_id,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );
    }

    DocumentationNode::Heading { children, .. } => {
      // Headings create section declarations (components in scope)
      let scope = Scope {
        container: file_path.to_string(),
        component: None,
        member: None,
      };

      // Extract heading text from children for the name
      let name = format!("Header {}", topic_id);

      data_context.declarations.insert(
        topic_id.clone(),
        Declaration {
          topic_id: topic_id.clone(),
          declaration_kind: DeclarationKind::DocumentationSection,
          name,
          scope,
        },
      );

      // Add the heading node with children converted to stubs
      data_context.nodes.insert(
        topic_id.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process heading children (inline formatting nodes)
      for child in children {
        process_documentation_node(
          child,
          file_path,
          current_section,
          current_paragraph,
          data_context,
        )?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      let name = format!("Paragraph {}", topic_id);
      // Paragraphs become paragraph declarations (members in scope)
      let scope = Scope {
        container: file_path.to_string(),
        component: Some(name.clone()),
        member: None,
      };

      data_context.declarations.insert(
        topic_id.clone(),
        Declaration {
          topic_id: topic_id.clone(),
          declaration_kind: DeclarationKind::DocumentationParagraph,
          name,
          scope,
        },
      );

      // Add the node with children converted to stubs
      data_context.nodes.insert(
        topic_id.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with this paragraph as the current paragraph
      for child in children {
        process_documentation_node(
          child,
          file_path,
          current_section,
          Some(&topic_id),
          data_context,
        )?;
      }
    }

    DocumentationNode::InlineCode {
      referenced_declaration,
      ..
    } => {
      // Add the inline code node
      data_context
        .nodes
        .insert(topic_id.clone(), Node::Documentation(node.clone()));

      // If there's a referenced declaration, create a reference
      if let Some(solidity_topic_id) = referenced_declaration {
        // Create a reference from the solidity declaration to this inline code
        let references = data_context
          .references
          .entry(solidity_topic_id.clone())
          .or_insert_with(Vec::new);

        // Add this inline code as a reference to the declaration
        if !references.contains(&topic_id) {
          references.push(topic_id.clone());
        }
      }
    }

    // For all other node types, just add them to the nodes map
    DocumentationNode::Text { .. }
    | DocumentationNode::CodeBlock { .. }
    | DocumentationNode::ThematicBreak { .. } => {
      data_context
        .nodes
        .insert(topic_id, Node::Documentation(node.clone()));
    }

    // For nodes with children, add the node and recurse
    DocumentationNode::List { children, .. }
    | DocumentationNode::ListItem { children, .. }
    | DocumentationNode::Emphasis { children, .. }
    | DocumentationNode::Strong { children, .. }
    | DocumentationNode::Link { children, .. }
    | DocumentationNode::BlockQuote { children, .. } => {
      // Add the node with children converted to stubs
      data_context.nodes.insert(
        topic_id,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children
      for child in children {
        process_documentation_node(
          child,
          file_path,
          current_section,
          current_paragraph,
          data_context,
        )?;
      }
    }

    DocumentationNode::Stub { .. } => {
      // Stubs are already processed, skip
    }
  }

  Ok(())
}

/// Extracts plain text content from a list of documentation nodes
fn extract_text_content(nodes: &[DocumentationNode]) -> String {
  let mut text = String::new();
  for node in nodes {
    match node {
      DocumentationNode::Text { value, .. } => {
        text.push_str(value);
      }
      DocumentationNode::InlineCode { value, .. } => {
        text.push_str(value);
      }
      DocumentationNode::Emphasis { children, .. }
      | DocumentationNode::Strong { children, .. }
      | DocumentationNode::Link { children, .. } => {
        text.push_str(&extract_text_content(children));
      }
      _ => {}
    }
  }
  text
}
