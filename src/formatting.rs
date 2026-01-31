use crate::core::topic;

/// Formats a simple token with a class
pub fn format_token(token: &str, class: &str) -> String {
  format!("<span class=\"{}\">{}</span>", class, token)
}

/// Formats a block with a class
pub fn format_block(content: &str, class: &str) -> String {
  format!("<div class=\"{}\">{}</div>", class, content)
}

/// Formats a topic token with data-topic attribute for focusable elements
pub fn format_topic_token(
  node_topic: &topic::Topic,
  token: &str,
  class: &str,
  data_topic: &topic::Topic,
) -> String {
  format!(
    "<span id=\"{}\" class=\"topic-token {}\" data-topic=\"{}\" tabindex=\"0\">{}</span>",
    node_topic.id(),
    class,
    data_topic.id(),
    token
  )
}

/// Formats a topic block (div) with data-topic attribute for focusable elements
pub fn format_topic_block(
  node_topic: &topic::Topic,
  content: &str,
  class: &str,
  data_topic: &topic::Topic,
) -> String {
  format!(
    "<div id=\"{}\" class=\"topic-token {}\" data-topic=\"{}\" tabindex=\"0\">{}</div>",
    node_topic.id(),
    class,
    data_topic.id(),
    content
  )
}

/// Escapes HTML special characters
pub fn html_escape(s: &str) -> String {
  s.replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}
