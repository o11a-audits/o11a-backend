use crate::core::topic::Topic;
use crate::core::{self, ProjectPath};
use crate::documentation::parser as doc_parser;
use crate::documentation::parser::DocumentationNode;

/// Parses comment markdown and extracts mentions.
/// Uses the documentation parser to parse markdown into an AST, then
/// collects CodeIdentifier nodes with resolved referenced_topic values.
/// This ensures only identifiers inside code spans (backticks) are matched.
pub fn parse_comment(
  content: &str,
  audit_data: &core::AuditData,
) -> Vec<Topic> {
  // Use a dummy project path — it's only stored on the AST, not used during parsing
  let dummy_path = ProjectPath {
    file_path: String::new(),
  };

  let ast =
    match doc_parser::ast_from_markdown(content, &dummy_path, audit_data) {
      Ok(ast) => ast,
      Err(_) => return vec![],
    };

  let mut mentions = Vec::new();
  for node in &ast.nodes {
    collect_referenced_topics(node, &mut mentions);
  }

  mentions.sort_unstable();
  mentions.dedup();
  mentions
}

/// Recursively walks a DocumentationNode tree and collects referenced_topic
/// from CodeIdentifier nodes (identifiers inside code spans that resolved
/// to known topics in the audit data).
fn collect_referenced_topics(node: &DocumentationNode, out: &mut Vec<Topic>) {
  match node {
    DocumentationNode::CodeIdentifier {
      referenced_topic: Some(topic),
      ..
    } => {
      out.push(topic.clone());
    }
    _ => {
      for child in node.children() {
        collect_referenced_topics(child, out);
      }
    }
  }
}
