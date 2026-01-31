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

    DocumentationNode::Section {
      header, children, ..
    } => {
      let header_html = do_node_to_html(header, indent_level, nodes_map);
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");

      let section_str = format!("\n{}\n{}\n", header_html, content);

      formatting::format_block(&section_str, "section")
    }

    DocumentationNode::Heading {
      level,
      children,
      node_id,
      ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let heading_content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<h{}>{}</h{}>",
        level,
        formatting::format_topic_token(
          &topic_id,
          &heading_content,
          "heading",
          &topic_id
        ),
        level
      )
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

    DocumentationNode::InlineCode {
      value,
      referenced_declaration,
      ..
    } => {
      // Add reference class and data attribute if this links to a declaration
      let reference_attrs = if let Some(ref_topic) = referenced_declaration {
        format!(" data-ref-topic=\"{}\"", ref_topic.id)
      } else {
        String::new()
      };

      format!(
        "<code class=\"inline-code reference\"{}>{}</code>",
        reference_attrs,
        formatting::html_escape(value)
      )
    }

    DocumentationNode::CodeBlock {
      lang,
      value,
      node_id,
      ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let lang_class = lang
        .as_ref()
        .map(|l| format!(" language-{}", l))
        .unwrap_or_default();

      format!(
        "<pre id=\"{}\" class=\"topic-token code-block\" data-topic=\"{}\" tabindex=\"0\"><code class=\"{}\">{}</code></pre>",
        topic_id.id,
        topic_id.id,
        lang_class,
        formatting::html_escape(value)
      )
    }

    DocumentationNode::List {
      ordered, children, ..
    } => {
      let list_items = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");

      if *ordered {
        format!(
          "<ol class=\"topic-token list ordered\" tabindex=\"0\">\n{}\n</ol>",
          list_items
        )
      } else {
        format!(
          "<ul class=\"topic-token list unordered\" tabindex=\"0\">\n{}\n</ul>",
          list_items
        )
      }
    }

    DocumentationNode::ListItem { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      format!("<li class=\"list-item\">{}</li>", content)
    }

    DocumentationNode::Emphasis { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      format!("<em class=\"emphasis\">{}</em>", content)
    }

    DocumentationNode::Strong { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("");

      format!("<strong class=\"strong\">{}</strong>", content)
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

      let title_attr = title
        .as_ref()
        .map(|t| format!(" title=\"{}\"", formatting::html_escape(t)))
        .unwrap_or_default();

      format!(
        "<a href=\"{}\" class=\"link\"{}>{}</a>",
        formatting::html_escape(url),
        title_attr,
        content
      )
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
      "<hr class=\"thematic-break\">".to_string()
    }

    DocumentationNode::Stub { topic, .. } => {
      format!(
        "<span class=\"stub\" data-topic=\"{}\">Stub</span>",
        topic.id
      )
    }
  }
}
