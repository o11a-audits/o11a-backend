use crate::core;
use crate::core::topic;
use crate::documentation::parser::DocumentationNode;
use crate::formatting;
use std::collections::BTreeMap;

/// Controls how documentation nodes are rendered to HTML.
pub struct FormatContext {
  /// When true, omits `id` and `data-topic` attributes on all nodes
  /// except code identifier references.
  pub comment_formatting: bool,
  pub target_topic: topic::Topic,
}

/// Converts a documentation node to formatted HTML string
pub fn node_to_html(
  node: &DocumentationNode,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
  ctx: &FormatContext,
) -> String {
  do_node_to_html(node, 0, nodes_map, ctx)
}

fn do_node_to_html(
  node: &DocumentationNode,
  indent_level: usize,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
  ctx: &FormatContext,
) -> String {
  match node.resolve(nodes_map) {
    DocumentationNode::Root { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("\n");

      if ctx.comment_formatting {
        formatting::format_topic_block(
          &ctx.target_topic,
          &content,
          "comment-root target-topic",
          &ctx.target_topic,
        )
      } else {
        formatting::format_block(&content, "documentation-root")
      }
    }

    // Section: renders just the content (no header, since header is parent)
    DocumentationNode::Section { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
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
      let heading_content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      let heading_html = if ctx.comment_formatting {
        formatting::format_heading(*level, &heading_content)
      } else {
        let topic_id = topic::new_documentation_topic(*node_id);
        formatting::format_topic_heading(*level, &topic_id, &heading_content)
      };

      // If there's a section child, render it after the heading
      match section {
        Some(sec) => {
          let section_html = do_node_to_html(sec, indent_level, nodes_map, ctx);
          format!("{}\n{}", heading_html, section_html)
        }
        None => heading_html,
      }
    }

    DocumentationNode::Paragraph {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      if ctx.comment_formatting {
        formatting::format_block(&content, "paragraph")
      } else {
        let topic_id = topic::new_documentation_topic(*node_id);
        formatting::format_topic_block(
          &topic_id,
          &content,
          "paragraph",
          &topic_id,
        )
      }
    }

    DocumentationNode::Sentence {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      if ctx.comment_formatting {
        content
      } else {
        let topic_id = topic::new_documentation_topic(*node_id);
        formatting::format_topic_token(
          &topic_id, &content, "sentence", &topic_id,
        )
      }
    }

    DocumentationNode::Text { value, .. } => formatting::html_escape(value),

    DocumentationNode::InlineCode { children, .. } => {
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map, ctx))
        .collect();
      formatting::format_inline_code(&inner)
    }

    DocumentationNode::CodeBlock {
      lang,
      children,
      node_id,
      ..
    } => {
      let inner: String = children
        .iter()
        .map(|c| do_node_to_html(c, indent_level, nodes_map, ctx))
        .collect();

      if ctx.comment_formatting {
        formatting::format_code_block(lang.as_deref(), &inner)
      } else {
        let topic_id = topic::new_documentation_topic(*node_id);
        formatting::format_topic_code_block(&topic_id, lang.as_deref(), &inner)
      }
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
      referenced_name,
      ..
    } => {
      let display_value = referenced_name.as_deref().unwrap_or(value);
      let class = kind
        .as_ref()
        .map(formatting::named_topic_kind_to_class)
        .unwrap_or("identifier");

      match referenced_topic {
        // Code references always keep topic attributes
        Some(ref_topic) => {
          let node_topic = topic::new_documentation_topic(*node_id);
          formatting::format_topic_token(
            &node_topic,
            &formatting::html_escape(display_value),
            class,
            ref_topic,
          )
        }
        None => formatting::format_token(
          &formatting::html_escape(display_value),
          class,
        ),
      }
    }

    DocumentationNode::CodeText { value, .. } => formatting::html_escape(value),

    DocumentationNode::List {
      node_id,
      ordered,
      children,
      ..
    } => {
      let list_items = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("\n");

      if ctx.comment_formatting {
        formatting::format_list(*ordered, &list_items)
      } else {
        let topic = topic::new_documentation_topic(*node_id);
        formatting::format_topic_list(&topic, *ordered, &list_items)
      }
    }

    DocumentationNode::ListItem { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_list_item(&content)
    }

    DocumentationNode::Emphasis { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_emphasis(&content)
    }

    DocumentationNode::Strong { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
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
        .map(|child| do_node_to_html(child, indent_level, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("");

      formatting::format_link(url, title.as_deref(), &content)
    }

    DocumentationNode::BlockQuote {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1, nodes_map, ctx))
        .collect::<Vec<_>>()
        .join("\n");

      if ctx.comment_formatting {
        formatting::format_block(&content, "blockquote")
      } else {
        let topic_id = topic::new_documentation_topic(*node_id);
        formatting::format_topic_block(
          &topic_id,
          &content,
          "blockquote",
          &topic_id,
        )
      }
    }

    DocumentationNode::ThematicBreak { .. } => {
      formatting::format_thematic_break()
    }

    DocumentationNode::Stub { topic, .. } => {
      if ctx.comment_formatting {
        String::new()
      } else {
        formatting::format_stub(topic)
      }
    }
  }
}

/// Converts a documentation node to plain text (raw markdown without semantics).
pub fn node_to_plain_text(
  node: &DocumentationNode,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  do_node_to_plain_text(node, nodes_map)
}

fn do_node_to_plain_text(
  node: &DocumentationNode,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  let node = node.resolve(nodes_map);
  match node {
    DocumentationNode::Root { children, .. } => children
      .iter()
      .map(|c| do_node_to_plain_text(c, nodes_map))
      .collect::<Vec<_>>()
      .join("\n"),

    DocumentationNode::Section { children, .. } => children
      .iter()
      .map(|c| do_node_to_plain_text(c, nodes_map))
      .collect::<Vec<_>>()
      .join("\n"),

    DocumentationNode::Heading {
      level,
      children,
      section,
      ..
    } => {
      let prefix = "#".repeat(*level as usize);
      let text: String = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect();
      let heading = format!("{} {}", prefix, text);
      match section {
        Some(sec) => {
          let section_text = do_node_to_plain_text(sec, nodes_map);
          format!("{}\n{}", heading, section_text)
        }
        None => heading,
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      let content: String = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect();
      format!("{}\n", content)
    }

    DocumentationNode::Sentence { children, .. } => children
      .iter()
      .map(|c| do_node_to_plain_text(c, nodes_map))
      .collect(),

    DocumentationNode::Text { value, .. } => value.clone(),

    DocumentationNode::InlineCode { value, .. } => format!("`{}`", value),

    DocumentationNode::CodeBlock { lang, value, .. } => {
      let fence_lang = lang.as_deref().unwrap_or("");
      format!("```{}\n{}\n```", fence_lang, value)
    }

    DocumentationNode::CodeKeyword { value, .. }
    | DocumentationNode::CodeOperator { value, .. }
    | DocumentationNode::CodeIdentifier { value, .. }
    | DocumentationNode::CodeText { value, .. } => value.clone(),

    DocumentationNode::List {
      ordered, children, ..
    } => {
      let items: Vec<String> = children
        .iter()
        .enumerate()
        .map(|(i, c)| {
          let inner = do_node_to_plain_text(c, nodes_map);
          if *ordered {
            format!("{}. {}", i + 1, inner)
          } else {
            format!("- {}", inner)
          }
        })
        .collect();
      items.join("\n")
    }

    DocumentationNode::ListItem { children, .. } => children
      .iter()
      .map(|c| do_node_to_plain_text(c, nodes_map))
      .collect(),

    DocumentationNode::Emphasis { children, .. } => {
      let content: String = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect();
      format!("*{}*", content)
    }

    DocumentationNode::Strong { children, .. } => {
      let content: String = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect();
      format!("**{}**", content)
    }

    DocumentationNode::Link {
      url, children, ..
    } => {
      let content: String = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect();
      format!("[{}]({})", content, url)
    }

    DocumentationNode::BlockQuote { children, .. } => {
      let content = children
        .iter()
        .map(|c| do_node_to_plain_text(c, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");
      content
        .lines()
        .map(|line| format!("> {}", line))
        .collect::<Vec<_>>()
        .join("\n")
    }

    DocumentationNode::ThematicBreak { .. } => "---".to_string(),

    DocumentationNode::Stub { .. } => String::new(),
  }
}
