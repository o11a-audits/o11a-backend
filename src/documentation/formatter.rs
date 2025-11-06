use crate::documentation::parser::DocumentationNode;

/// Converts a documentation node to formatted HTML string
pub fn node_to_html(node: &DocumentationNode) -> String {
  do_node_to_html(node, 0)
}

fn do_node_to_html(node: &DocumentationNode, indent_level: usize) -> String {
  match node {
    DocumentationNode::Root { children, .. } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      format!("<div class=\"documentation-root\">{}</div>", content)
    }

    DocumentationNode::Section {
      header,
      children,
      node_id,
      ..
    } => {
      let header_html = do_node_to_html(header, indent_level);
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("\n");

      format!(
        "<section class=\"section node-{}\" id=\"node-{}\">\n{}\n{}\n</section>",
        node_id, node_id, header_html, content
      )
    }

    DocumentationNode::Heading {
      level,
      children,
      node_id,
      ..
    } => {
      let heading_content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<h{} class=\"heading node-{}\" id=\"node-{}\">{}</h{}>",
        level, node_id, node_id, heading_content, level
      )
    }

    DocumentationNode::Paragraph {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<p class=\"paragraph node-{}\" id=\"node-{}\">{}</p>",
        node_id, node_id, content
      )
    }

    DocumentationNode::Sentence {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<span class=\"sentence node-{}\" id=\"node-{}\">{}</span>",
        node_id, node_id, content
      )
    }

    DocumentationNode::Text { value, .. } => html_escape(value),

    DocumentationNode::InlineCode {
      value,
      node_id,
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
        "<code class=\"inline-code node-{} reference\" id=\"node-{}\"{}>{}</code>",
        node_id,
        node_id,
        reference_attrs,
        html_escape(value)
      )
    }

    DocumentationNode::CodeBlock {
      lang,
      value,
      node_id,
      ..
    } => {
      let lang_class = lang
        .as_ref()
        .map(|l| format!(" language-{}", l))
        .unwrap_or_default();

      format!(
        "<pre class=\"code-block node-{}\" id=\"node-{}\"><code class=\"{}\">{}</code></pre>",
        node_id,
        node_id,
        lang_class,
        html_escape(value)
      )
    }

    DocumentationNode::List {
      ordered,
      children,
      node_id,
      ..
    } => {
      let list_items = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1))
        .collect::<Vec<_>>()
        .join("\n");

      if *ordered {
        format!(
          "<ol class=\"list ordered node-{}\" id=\"node-{}\">\n{}\n</ol>",
          node_id, node_id, list_items
        )
      } else {
        format!(
          "<ul class=\"list unordered node-{}\" id=\"node-{}\">\n{}\n</ul>",
          node_id, node_id, list_items
        )
      }
    }

    DocumentationNode::ListItem {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<li class=\"list-item node-{}\" id=\"node-{}\">{}</li>",
        node_id, node_id, content
      )
    }

    DocumentationNode::Emphasis {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<em class=\"emphasis node-{}\" id=\"node-{}\">{}</em>",
        node_id, node_id, content
      )
    }

    DocumentationNode::Strong {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      format!(
        "<strong class=\"strong node-{}\" id=\"node-{}\">{}</strong>",
        node_id, node_id, content
      )
    }

    DocumentationNode::Link {
      url,
      title,
      children,
      node_id,
      ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level))
        .collect::<Vec<_>>()
        .join("");

      let title_attr = title
        .as_ref()
        .map(|t| format!(" title=\"{}\"", html_escape(t)))
        .unwrap_or_default();

      format!(
        "<a href=\"{}\" class=\"link node-{}\" id=\"node-{}\"{}>{}</a>",
        html_escape(url),
        node_id,
        node_id,
        title_attr,
        content
      )
    }

    DocumentationNode::BlockQuote {
      children, node_id, ..
    } => {
      let content = children
        .iter()
        .map(|child| do_node_to_html(child, indent_level + 1))
        .collect::<Vec<_>>()
        .join("\n");

      format!(
        "<blockquote class=\"blockquote node-{}\" id=\"node-{}\">\n{}\n</blockquote>",
        node_id, node_id, content
      )
    }

    DocumentationNode::ThematicBreak { node_id, .. } => {
      format!(
        "<hr class=\"thematic-break node-{}\" id=\"node-{}\">",
        node_id, node_id
      )
    }

    DocumentationNode::Stub { topic, .. } => {
      format!(
        "<span class=\"stub\" data-topic-id=\"{}\">Stub</span>",
        topic.id
      )
    }
  }
}

/// Escapes HTML special characters
fn html_escape(s: &str) -> String {
  s.replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}
