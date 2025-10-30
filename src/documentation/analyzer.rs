use crate::core;
use crate::core::topic;
use crate::core::{
  AST, DataContext, Declaration, DeclarationKind, Node, Scope,
};
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
  for (project_path, asts) in &ast_map {
    for ast in asts {
      // Add to in_scope_files
      data_context.in_scope_files.insert(project_path.clone());

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
      data_context
        .asts
        .insert(project_path.clone(), AST::Documentation(stubbed_ast));

      process_documentation_ast(ast, project_path, &mut data_context)?;
    }
  }

  Ok(data_context)
}

fn process_documentation_ast(
  ast: &DocumentationAST,
  project_path: &core::ProjectPath,
  data_context: &mut DataContext,
) -> Result<(), String> {
  // Store source content for this file
  data_context
    .source_content
    .insert(project_path.clone(), ast.source_content.clone());

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(
      node,
      project_path,
      None, // No section context initially
      None, // No paragraph context initially
      data_context,
    )?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  project_path: &core::ProjectPath,
  current_section: Option<&topic::Topic>,
  current_paragraph: Option<&topic::Topic>,
  data_context: &mut DataContext,
) -> Result<(), String> {
  let node_id = node.node_id();
  let topic = topic::new_node_topic(node_id);

  match node {
    DocumentationNode::Root { children, .. } => {
      // Add the Root node to the data context
      data_context.nodes.insert(
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
          data_context,
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
        data_context,
      )?;

      // Process section children with this section as the current section
      for child in children {
        process_documentation_node(
          child,
          project_path,
          Some(&header_topic),
          None, // Reset paragraph context when entering new section
          data_context,
        )?;
      }

      // Add the section node itself with children converted to stubs
      data_context.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );
    }

    DocumentationNode::Heading { children, .. } => {
      // Headings create section declarations (components in scope)
      let scope = Scope {
        container: project_path.clone(),
        component: None,
        member: None,
      };

      // Extract heading text from children for the name
      let name = format!("Header {}", topic.id);

      data_context.declarations.insert(
        topic.clone(),
        Declaration {
          topic: topic.clone(),
          declaration_kind: DeclarationKind::DocumentationSection,
          name,
          scope,
        },
      );

      // Add the heading node with children converted to stubs
      data_context.nodes.insert(
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
          data_context,
        )?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      let name = format!("Paragraph {}", topic.id);
      // Paragraphs become paragraph declarations (members in scope)
      let scope = Scope {
        container: project_path.clone(),
        component: Some(topic.clone()),
        member: None,
      };

      data_context.declarations.insert(
        topic.clone(),
        Declaration {
          topic: topic.clone(),
          declaration_kind: DeclarationKind::DocumentationParagraph,
          name,
          scope,
        },
      );

      // Add the node with children converted to stubs
      data_context.nodes.insert(
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
        .insert(topic.clone(), Node::Documentation(node.clone()));

      // If there's a referenced declaration, create a reference
      if let Some(solidity_topic) = referenced_declaration {
        // Create a reference from the solidity declaration to this inline code
        let references = data_context
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
      data_context
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
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
