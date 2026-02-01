use crate::core;
use crate::core::topic;
use crate::core::{NamedTopicKind, VariableMutability};
use crate::documentation::parser::DocumentationNode;
use crate::formatting;
use std::collections::BTreeMap;

/// Converts a NamedTopicKind to a CSS class for syntax highlighting
fn named_topic_kind_to_class(kind: &NamedTopicKind) -> &'static str {
  match kind {
    NamedTopicKind::Contract(_) => "contract",
    NamedTopicKind::Function(_) => "function",
    NamedTopicKind::Modifier => "modifier",
    NamedTopicKind::Event => "event",
    NamedTopicKind::Error => "error",
    NamedTopicKind::Struct => "struct",
    NamedTopicKind::Enum => "enum",
    NamedTopicKind::EnumMember => "enum-member",
    NamedTopicKind::StateVariable(VariableMutability::Mutable) => {
      "mutable-state-variable"
    }
    NamedTopicKind::StateVariable(VariableMutability::Immutable) => {
      "immutable-state-variable"
    }
    NamedTopicKind::StateVariable(VariableMutability::Constant) => "constant",
    NamedTopicKind::LocalVariable => "local-variable",
    NamedTopicKind::Builtin => "builtin",
  }
}

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

    DocumentationNode::InlineCode { children, .. } => {
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map))
        .collect();
      format!("<code class=\"inline-code\">{}</code>", inner)
    }

    DocumentationNode::CodeBlock {
      lang,
      children,
      node_id,
      ..
    } => {
      let topic_id = topic::new_documentation_topic(*node_id);
      let lang_class = lang
        .as_ref()
        .map(|l| format!(" language-{}", l))
        .unwrap_or_default();
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map))
        .collect();

      format!(
        "<pre id=\"{}\" class=\"topic-token code-block{}\" data-topic=\"{}\" tabindex=\"0\"><code>{}</code></pre>",
        topic_id.id, lang_class, topic_id.id, inner
      )
    }

    DocumentationNode::CodeKeyword { value, .. } => {
      formatting::format_token(&formatting::html_escape(value), "keyword")
    }

    DocumentationNode::CodeOperator { value, .. } => {
      formatting::format_token(&formatting::html_escape(value), "operator")
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
        .map(named_topic_kind_to_class)
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
