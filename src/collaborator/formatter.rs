use crate::core;
use crate::documentation::formatter as doc_formatter;
use crate::documentation::formatter::FormatContext;
use crate::documentation::parser::DocumentationAST;
use std::collections::BTreeMap;

/// Renders a comment AST to HTML by delegating to the documentation formatter
/// with comment formatting enabled (no topic IDs except on code references).
pub fn render_comment_html(
  ast: &DocumentationAST,
  comment_topic: &core::topic::Topic,
  nodes_map: &BTreeMap<core::topic::Topic, core::Node>,
) -> String {
  let ctx = FormatContext {
    comment_formatting: true,
    target_topic: comment_topic.clone(),
  };
  ast
    .nodes
    .iter()
    .map(|node| doc_formatter::node_to_html(node, nodes_map, &ctx))
    .collect::<Vec<_>>()
    .join("")
}
