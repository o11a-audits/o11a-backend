use crate::core;
use crate::core::topic;
use crate::documentation::parser::DocumentationNode;
use crate::formatting;
use std::collections::BTreeMap;

/// Converts a documentation node to formatted HTML string
pub fn node_to_html(
  node: &DocumentationNode,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  do_node_to_html(node, 0, nodes_map)
}

fn do_node_to_html(
  node: &DocumentationNode,
  indent_level: usize,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  match node.resolve(nodes_map) {
    DocumentationNode::Root { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");
      formatting::format_block(&content, "documentation-root")
    }

    // Section: renders just the content (no header, since header is parent)
    DocumentationNode::Section { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");

      formatting::format_block(&content, "section")
    }

    // Heading: renders the heading text and its section child (if any)
    DocumentationNode::Heading {
      level,
      children,
      node_id,
      section,
      ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let heading_content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      let heading_html =
        formatting::format_heading(*level, &topic_id, &heading_content);

      // If there's a section child, render it after the heading
      match section {
        Some(sec) => {
          let section_html = do_node_to_html(sec, indent_level, nodes_map);
          format!("{}\n{}", heading_html, section_html)
        }
        None => heading_html,
      }
    }

    DocumentationNode::Paragraph {
      children, node_id, ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_topic_block(
        &topic_id,
        &content,
        "paragraph",
        &topic_id,
      )
    }

    DocumentationNode::Sentence {
      children, node_id, ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_topic_token(&topic_id, &content, "sentence", &topic_id)
    }

    DocumentationNode::Text { value, .. } => formatting::html_escape(value),

    DocumentationNode::InlineCode { children, .. } => {
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map))
        .collect();
      formatting::format_inline_code(&inner)
    }

    DocumentationNode::CodeBlock {
      lang,
      children,
      node_id,
      ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map))
        .collect();

      formatting::format_code_block(&topic_id, lang.as_deref(), &inner)
    }

    DocumentationNode::CodeKeyword { value, .. } => {
      formatting::format_keyword(&formatting::html_escape(value))
    }

    DocumentationNode::CodeOperator { value, .. } => {
      // format_operator already escapes HTML, so don't double-escape
      formatting::format_operator(value)
    }

    DocumentationNode::CodeIdentifier {
      node_id,
      value,
      referenced_topic,
      kind,
      ..
    } => {
      let class = kind
        .as_ref()
        .map(formatting::named_topic_kind_to_class)
        .unwrap_or("identifier");

      match referenced_topic {
        Some(ref_topic) => {
          let node_topic = topic::new_documentation_topic(*node_id);
          formatting::format_topic_token(
            &node_topic,
            &formatting::html_escape(value),
            class,
            ref_topic,
          )
        }
        None => {
          formatting::format_token(&formatting::html_escape(value), class)
        }
      }
    }

    DocumentationNode::CodeText { value, .. } => formatting::html_escape(value),

    DocumentationNode::List {
      node_id,
      ordered,
      children,
      ..
    } => {
      let topic = topic::new_documentation_topic(*node_id);

      let list_items = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");

      formatting::format_list(&topic, *ordered, &list_items)
    }

    DocumentationNode::ListItem { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_list_item(&content)
    }

    DocumentationNode::Emphasis { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_emphasis(&content)
    }

    DocumentationNode::Strong { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_strong(&content)
    }

    DocumentationNode::Link {
      url,
      title,
      children,
      ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_link(url, title.as_deref(), &content)
    }

    DocumentationNode::BlockQuote {
      children, node_id, ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");

      formatting::format_topic_block(
        &topic_id,
        &content,
        "blockquote",
        &topic_id,
      )
    }

    DocumentationNode::ThematicBreak { .. } => {
      formatting::format_thematic_break()
    }

    DocumentationNode::Stub { topic, .. } => formatting::format_stub(topic),
  }
}
