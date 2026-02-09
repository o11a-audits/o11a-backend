use crate::core::topic::Topic;
use crate::core::{self, ProjectPath};
use crate::documentation::parser as doc_parser;
use crate::documentation::parser::DocumentationNode;
use std::sync::atomic::{AtomicI32, Ordering};

/// Parses comment markdown and extracts mentions and the AST.
/// Uses the documentation parser to parse markdown into an AST, then
/// collects CodeIdentifier nodes with resolved referenced_topic values.
/// This ensures only identifiers inside code spans (backticks) are matched.
/// Returns both the mentions and the AST so callers can format without re-parsing.
pub fn parse_comment(
  content: &str,
  audit_data: &core::AuditData,
) -> (Vec<Topic>, doc_parser::DocumentationAST) {
  // Use a dummy project path — it's only stored on the AST, not used during parsing
  let dummy_path = ProjectPath {
    file_path: String::new(),
  };

  // Use a local throwaway counter — comment AST node IDs are ephemeral
  // and must not consume IDs from the global documentation counter
  let counter = AtomicI32::new(0);
  let next_id = || counter.fetch_add(1, Ordering::SeqCst);

  let ast = match doc_parser::ast_from_markdown(
    content,
    &dummy_path,
    audit_data,
    &next_id,
  ) {
    Ok(ast) => ast,
    Err(_) => {
      return (
        vec![],
        doc_parser::DocumentationAST {
          nodes: vec![],
          project_path: dummy_path,
          source_content: content.to_string(),
        },
      );
    }
  };

  let mut mentions = Vec::new();
  for node in &ast.nodes {
    collect_referenced_topics(node, &mut mentions);
  }

  mentions.sort_unstable();
  mentions.dedup();
  (mentions, ast)
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
