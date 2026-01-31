use crate::core;
use crate::core::topic;
use markdown::ParseOptions;
use markdown::mdast::Node as MdNode;
use std::path::Path;
use std::sync::atomic::{AtomicI32, Ordering};

/// Global counter for documentation node IDs
/// This can be used later when processing user-submitted docs
static NEXT_DOC_NODE_ID: AtomicI32 = AtomicI32::new(1);

/// Gets the next documentation node ID
pub fn next_node_id() -> i32 {
  NEXT_DOC_NODE_ID.fetch_add(1, Ordering::SeqCst)
}

/// Processes markdown files from src/ and docs/ directories
pub fn process(
  project_path: &Path,
  audit_data: &core::AuditData,
) -> Result<
  std::collections::BTreeMap<core::ProjectPath, Vec<DocumentationAST>>,
  String,
> {
  let mut ast_map = std::collections::BTreeMap::new();

  // Process both src/ and docs/ directories
  let src_dir = project_path.join("src");
  let docs_dir = project_path.join("docs");

  if src_dir.exists() && src_dir.is_dir() {
    traverse_directory(&src_dir, &project_path, &mut ast_map, audit_data)?;
  }

  if docs_dir.exists() && docs_dir.is_dir() {
    traverse_directory(&docs_dir, &project_path, &mut ast_map, audit_data)?;
  }

  Ok(ast_map)
}

fn traverse_directory(
  dir: &Path,
  project_root: &Path,
  ast_map: &mut std::collections::BTreeMap<
    core::ProjectPath,
    Vec<DocumentationAST>,
  >,
  audit_data: &core::AuditData,
) -> Result<(), String> {
  let entries = std::fs::read_dir(dir)
    .map_err(|e| format!("Failed to read directory {:?}: {}", dir, e))?;

  for entry in entries {
    let entry =
      entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
    let path = entry.path();

    if path.is_dir() {
      // Recursively traverse subdirectories
      traverse_directory(&path, &project_root, ast_map, audit_data)?;
    } else if path.is_file() {
      if let Some(extension) = path.extension() {
        if extension == "md" {
          let content = std::fs::read_to_string(&path).map_err(|e| {
            format!("Failed to read markdown file {:?}: {}", path, e)
          })?;

          let project_path =
            core::new_project_path_from_path(&path, project_root);

          let ast = ast_from_markdown(&content, &project_path, audit_data)?;

          ast_map
            .entry(project_path)
            .or_insert_with(Vec::new)
            .push(ast);
        }
      }
    }
  }

  Ok(())
}

pub fn ast_from_markdown(
  content: &str,
  project_path: &core::ProjectPath,
  audit_data: &core::AuditData,
) -> Result<DocumentationAST, String> {
  // Parse markdown to mdast
  let md_ast = markdown::to_mdast(content, &ParseOptions::default())
    .map_err(|e| format!("Failed to parse markdown: {}", e))?;

  // Convert mdast to our DocumentationNode format
  let nodes = convert_mdast_node(&md_ast, audit_data)?;

  Ok(DocumentationAST {
    nodes: vec![nodes],
    project_path: project_path.clone(),
    source_content: content.to_string(),
  })
}

pub struct DocumentationAST {
  pub nodes: Vec<DocumentationNode>,
  pub project_path: core::ProjectPath,
  pub source_content: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocumentationNode {
  Root {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  // Section: groups a heading with all content until the next heading
  // This node is created by the parser, not present in markdown AST
  Section {
    node_id: i32,
    header: Box<DocumentationNode>, // The heading node
    children: Vec<DocumentationNode>, // Content until next heading
  },

  Heading {
    node_id: i32,
    level: u8,
    children: Vec<DocumentationNode>,
  },

  Paragraph {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  // Sentence
  Sentence {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  Text {
    node_id: i32,
    value: String,
  },

  // Inline code (potential references to source code declarations)
  InlineCode {
    node_id: i32,
    value: String,
    referenced_declaration: Option<topic::Topic>, // Solidity topic ID if found
  },

  CodeBlock {
    node_id: i32,
    lang: Option<String>,
    value: String,
  },

  List {
    node_id: i32,
    ordered: bool,
    children: Vec<DocumentationNode>,
  },

  ListItem {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  Emphasis {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  Strong {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  Link {
    node_id: i32,
    url: String,
    title: Option<String>,
    children: Vec<DocumentationNode>,
  },

  BlockQuote {
    node_id: i32,
    children: Vec<DocumentationNode>,
  },

  ThematicBreak {
    node_id: i32,
  },

  // Placeholder for a node (similar to Solidity's Stub)
  Stub {
    node_id: i32,
    topic: topic::Topic,
  },
}

impl DocumentationNode {
  pub fn node_id(&self) -> i32 {
    match self {
      DocumentationNode::Root { node_id, .. } => *node_id,
      DocumentationNode::Section { node_id, .. } => *node_id,
      DocumentationNode::Heading { node_id, .. } => *node_id,
      DocumentationNode::Paragraph { node_id, .. } => *node_id,
      DocumentationNode::Sentence { node_id, .. } => *node_id,
      DocumentationNode::Text { node_id, .. } => *node_id,
      DocumentationNode::InlineCode { node_id, .. } => *node_id,
      DocumentationNode::CodeBlock { node_id, .. } => *node_id,
      DocumentationNode::List { node_id, .. } => *node_id,
      DocumentationNode::ListItem { node_id, .. } => *node_id,
      DocumentationNode::Emphasis { node_id, .. } => *node_id,
      DocumentationNode::Strong { node_id, .. } => *node_id,
      DocumentationNode::Link { node_id, .. } => *node_id,
      DocumentationNode::BlockQuote { node_id, .. } => *node_id,
      DocumentationNode::ThematicBreak { node_id, .. } => *node_id,
      DocumentationNode::Stub { node_id, .. } => *node_id,
    }
  }

  pub fn children(&self) -> Vec<&DocumentationNode> {
    match self {
      DocumentationNode::Root { children, .. }
      | DocumentationNode::Section { children, .. }
      | DocumentationNode::Heading { children, .. }
      | DocumentationNode::Paragraph { children, .. }
      | DocumentationNode::Sentence { children, .. }
      | DocumentationNode::List { children, .. }
      | DocumentationNode::ListItem { children, .. }
      | DocumentationNode::Emphasis { children, .. }
      | DocumentationNode::Strong { children, .. }
      | DocumentationNode::Link { children, .. }
      | DocumentationNode::BlockQuote { children, .. } => {
        children.iter().collect()
      }
      _ => vec![],
    }
  }

  /// Resolves a node, looking up Stub nodes from the nodes_map
  pub fn resolve<'a>(
    &'a self,
    nodes_map: &'a std::collections::BTreeMap<
      crate::core::topic::Topic,
      crate::core::Node,
    >,
  ) -> &'a DocumentationNode {
    match self {
      DocumentationNode::Stub { topic, .. } => {
        if let Some(crate::core::Node::Documentation(doc_node)) =
          nodes_map.get(topic)
        {
          doc_node
        } else {
          self
        }
      }
      _ => self,
    }
  }
}

/// Splits paragraph children into sentence nodes based on periods
/// Each sentence contains all inline nodes (Text, InlineCode, Emphasis, Strong, Link) until a period
fn split_into_sentences(
  children: Vec<DocumentationNode>,
) -> Vec<DocumentationNode> {
  let mut sentences = Vec::new();
  let mut current_sentence_nodes = Vec::new();

  for node in children {
    match &node {
      DocumentationNode::Text { value, .. } => {
        // Split text by periods, keeping track of which nodes go into which sentence
        let mut remaining_text = value.as_str();

        loop {
          if let Some(period_idx) = remaining_text.find('.') {
            // Found a period
            let before_period = &remaining_text[..=period_idx]; // Include the period

            if !before_period.trim().is_empty() {
              // Add text up to and including the period
              current_sentence_nodes.push(DocumentationNode::Text {
                node_id: next_node_id(),
                value: before_period.to_string(),
              });

              // Complete this sentence
              if !current_sentence_nodes.is_empty() {
                sentences.push(DocumentationNode::Sentence {
                  node_id: next_node_id(),
                  children: current_sentence_nodes.drain(..).collect(),
                });
              }
            }

            // Move past the period
            remaining_text = &remaining_text[period_idx + 1..];
          } else {
            // No more periods in this text node
            if !remaining_text.trim().is_empty() {
              current_sentence_nodes.push(DocumentationNode::Text {
                node_id: next_node_id(),
                value: remaining_text.to_string(),
              });
            }
            break;
          }
        }
      }

      // For non-text inline nodes, add them to the current sentence
      DocumentationNode::InlineCode { .. }
      | DocumentationNode::Emphasis { .. }
      | DocumentationNode::Strong { .. }
      | DocumentationNode::Link { .. } => {
        current_sentence_nodes.push(node);
      }

      // Other node types shouldn't appear as direct children of paragraphs,
      // but handle them gracefully by ending the current sentence
      _ => {
        // End the current sentence if there is one
        if !current_sentence_nodes.is_empty() {
          sentences.push(DocumentationNode::Sentence {
            node_id: next_node_id(),
            children: current_sentence_nodes.drain(..).collect(),
          });
        }
        // The unexpected node is not added to any sentence
      }
    }
  }

  // Add any remaining nodes as the final sentence
  if !current_sentence_nodes.is_empty() {
    sentences.push(DocumentationNode::Sentence {
      node_id: next_node_id(),
      children: current_sentence_nodes,
    });
  }

  sentences
}

/// Groups nodes into sections based on headings
/// Each heading and all content until the next heading (or end) becomes a Section
fn group_into_sections(
  nodes: Vec<DocumentationNode>,
) -> Vec<DocumentationNode> {
  let mut sections = Vec::new();
  let mut current_heading: Option<DocumentationNode> = None;
  let mut current_content = Vec::new();

  for node in nodes {
    match &node {
      DocumentationNode::Heading { .. } => {
        // If we have a previous heading, create a section for it
        if let Some(heading) = current_heading.take() {
          let section_node_id = next_node_id();
          sections.push(DocumentationNode::Section {
            node_id: section_node_id,
            header: Box::new(heading),
            children: current_content.drain(..).collect(),
          });
        }
        // Store this heading for the next section
        current_heading = Some(node);
      }
      _ => {
        // Non-heading content
        if current_heading.is_some() {
          // We're inside a section, add to current content
          current_content.push(node);
        } else {
          // No heading yet, add node directly to sections
          sections.push(node);
        }
      }
    }
  }

  // Handle the last section if there is one
  if let Some(heading) = current_heading {
    let section_node_id = next_node_id();
    sections.push(DocumentationNode::Section {
      node_id: section_node_id,
      header: Box::new(heading),
      children: current_content,
    });
  }

  sections
}

fn convert_mdast_node(
  node: &MdNode,
  audit_data: &core::AuditData,
) -> Result<DocumentationNode, String> {
  let node_id = next_node_id();

  match node {
    MdNode::Root(root) => {
      // Convert all children first
      let children = root
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      // Group children into sections
      let sections = group_into_sections(children);

      Ok(DocumentationNode::Root {
        node_id,
        children: sections,
      })
    }

    MdNode::Heading(heading) => {
      let children = heading
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::Heading {
        node_id,
        level: heading.depth,
        children,
      })
    }

    MdNode::Paragraph(paragraph) => {
      let children = paragraph
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      // Split the paragraph's children into sentences
      let sentences = split_into_sentences(children);

      Ok(DocumentationNode::Paragraph {
        node_id,
        children: sentences,
      })
    }

    MdNode::Text(text) => Ok(DocumentationNode::Text {
      node_id,
      value: text.value.clone(),
    }),

    MdNode::InlineCode(code) => {
      // Try to find a matching declaration in the solidity context
      let referenced_declaration =
        find_declaration_by_name(audit_data, &code.value)
          .map(|decl| decl.topic().clone());

      Ok(DocumentationNode::InlineCode {
        node_id,
        value: code.value.clone(),
        referenced_declaration,
      })
    }

    MdNode::Code(code) => Ok(DocumentationNode::CodeBlock {
      node_id,
      lang: code.lang.clone(),
      value: code.value.clone(),
    }),

    MdNode::List(list) => {
      let children = list
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::List {
        node_id,
        ordered: list.ordered,
        children,
      })
    }

    MdNode::ListItem(item) => {
      let children = item
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::ListItem { node_id, children })
    }

    MdNode::Emphasis(emphasis) => {
      let children = emphasis
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::Emphasis { node_id, children })
    }

    MdNode::Strong(strong) => {
      let children = strong
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::Strong { node_id, children })
    }

    MdNode::Link(link) => {
      let children = link
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::Link {
        node_id,
        url: link.url.clone(),
        title: link.title.clone(),
        children,
      })
    }

    MdNode::Blockquote(quote) => {
      let children = quote
        .children
        .iter()
        .map(|child| convert_mdast_node(child, audit_data))
        .collect::<Result<Vec<_>, _>>()?;

      Ok(DocumentationNode::BlockQuote { node_id, children })
    }

    MdNode::ThematicBreak(_) => {
      Ok(DocumentationNode::ThematicBreak { node_id })
    }

    // For unsupported node types, we'll create a text node with a placeholder
    _ => Ok(DocumentationNode::Text {
      node_id,
      value: format!("[UNSUPPORTED {:?}]", node),
    }),
  }
}

/// Searches the AuditData for a declaration with the given value
/// Search order: topic ID, qualified name, then simple name
/// This is used to resolve inline code references to solidity declarations
fn find_declaration_by_name<'a>(
  audit_data: &'a core::AuditData,
  value: &str,
) -> Option<&'a crate::core::TopicMetadata> {
  // First try to find by topic ID (most specific)
  audit_data
    .topic_metadata
    .get(&topic::new_topic(value))
    .or_else(|| {
      // If not found by topic ID, try to find by qualified name
      audit_data
        .topic_metadata
        .values()
        .find(|decl| decl.qualified_name(audit_data) == value)
        .or_else(|| {
          // If not found by qualified name, try to find by simple name
          audit_data
            .topic_metadata
            .values()
            .find(|decl| decl.name() == value)
        })
    })
}

/// Converts children nodes to stubs for storage optimization
pub fn children_to_stubs(node: DocumentationNode) -> DocumentationNode {
  match node {
    DocumentationNode::Root { node_id, children } => DocumentationNode::Root {
      node_id,
      children: children.into_iter().map(node_to_stub).collect(),
    },
    DocumentationNode::Section {
      node_id,
      header,
      children,
    } => DocumentationNode::Section {
      node_id,
      header: Box::new(node_to_stub(*header)),
      children: children.into_iter().map(node_to_stub).collect(),
    },
    DocumentationNode::Heading {
      node_id,
      level,
      children,
    } => DocumentationNode::Heading {
      node_id,
      level,
      children: children.into_iter().map(node_to_stub).collect(),
    },
    DocumentationNode::Paragraph { node_id, children } => {
      DocumentationNode::Paragraph {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    DocumentationNode::Sentence { node_id, children } => {
      DocumentationNode::Sentence {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    DocumentationNode::List {
      node_id,
      ordered,
      children,
    } => DocumentationNode::List {
      node_id,
      ordered,
      children: children.into_iter().map(node_to_stub).collect(),
    },
    DocumentationNode::ListItem { node_id, children } => {
      DocumentationNode::ListItem {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    DocumentationNode::Emphasis { node_id, children } => {
      DocumentationNode::Emphasis {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    DocumentationNode::Strong { node_id, children } => {
      DocumentationNode::Strong {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    DocumentationNode::Link {
      node_id,
      url,
      title,
      children,
    } => DocumentationNode::Link {
      node_id,
      url,
      title,
      children: children.into_iter().map(node_to_stub).collect(),
    },
    DocumentationNode::BlockQuote { node_id, children } => {
      DocumentationNode::BlockQuote {
        node_id,
        children: children.into_iter().map(node_to_stub).collect(),
      }
    }
    // Leaf nodes remain unchanged
    other => other,
  }
}

fn node_to_stub(node: DocumentationNode) -> DocumentationNode {
  DocumentationNode::Stub {
    node_id: node.node_id(),
    topic: topic::new_documentation_topic(node.node_id()),
  }
}
