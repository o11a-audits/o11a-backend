use crate::core::{NamedMutableTopicKind, NamedTopicKind, topic};

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

pub fn format_named_identifier(
  topic: &topic::Topic,
  name: &String,
  ref_topic: &topic::Topic,
  kind: &NamedTopicKind,
) -> String {
  // Prefer metadata-based classification (single source of truth)
  let css_class = match kind {
    core::NamedTopicKind::Contract(_) => "contract",
    core::NamedTopicKind::Struct => "struct",
    core::NamedTopicKind::Enum => "enum",
    core::NamedTopicKind::EnumMember => "enum-value",
    core::NamedTopicKind::Error => "error",
    core::NamedTopicKind::Event => "event",
    core::NamedTopicKind::StateVariable(core::VariableMutability::Constant) => {
      "constant"
    }
    core::NamedTopicKind::StateVariable(core::VariableMutability::Mutable) => {
      "mutable-state-variable"
    }
    core::NamedTopicKind::StateVariable(
      core::VariableMutability::Immutable,
    ) => "immutable-state-variable",
    core::NamedTopicKind::LocalVariable => "local-variable",
    core::NamedTopicKind::Function(_) => "function",
    core::NamedTopicKind::Modifier => "modifier",
    core::NamedTopicKind::Builtin => "global",
  };
  format_topic_token(&topic, name, css_class, ref_topic)
}

pub fn format_named_mutable_identifier(
  topic: &topic::Topic,
  name: &String,
  ref_topic: &topic::Topic,
  kind: &NamedMutableTopicKind,
) -> String {
  let css_class = match kind {
    core::NamedMutableTopicKind::StateVariable => "mutable-state-variable",
    core::NamedMutableTopicKind::LocalVariable => "mutable-local-variable",
  };
  format_topic_token(&topic, name, css_class, ref_topic)
}

/// Escapes HTML special characters
pub fn html_escape(s: &str) -> String {
  s.replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}
