use serde::Serialize;

use crate::core::{
  self, AuditData, BlockAnnotationKind, ControlFlowStatementKind,
  NamedTopicKind, NamedTopicVisibility, Node, Reference, Scope, SourceChild,
  SourceContext, TitledTopicKind, TopicMetadata, UnnamedTopicKind, topic,
};
use crate::documentation::FormatContext;
use crate::solidity::parser::ASTNode;

// ============================================================================
// Response Types
// ============================================================================

#[derive(Debug, Serialize)]
pub struct AgentTopicContext {
  pub name: String,
  pub kind: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub sub_kind: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub visibility: Option<String>,
  pub scope: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_mutable: Option<bool>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub ancestors: Option<Vec<String>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub descendants: Option<Vec<String>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub relatives: Option<Vec<String>>,
  pub context: Vec<AgentSourceGroup>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub expanded_context: Option<Vec<AgentSourceGroup>>,
  pub mentions: Vec<AgentSourceGroup>,
}

#[derive(Debug, Serialize)]
pub struct AgentSourceGroup {
  pub scope: String,
  pub in_scope: bool,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub scope_references: Vec<AgentSourceChild>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub nested_references: Vec<AgentNestedGroup>,
}

#[derive(Debug, Serialize)]
pub struct AgentNestedGroup {
  pub subscope: String,
  pub children: Vec<AgentSourceChild>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum AgentSourceChild {
  #[serde(rename = "reference")]
  Reference {
    code: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    comments: Option<Vec<AgentComment>>,
  },
  #[serde(rename = "annotated_block")]
  AnnotatedBlock {
    annotation: String,
    kind: String,
    children: Vec<AgentSourceChild>,
  },
}

#[derive(Debug, Serialize)]
pub struct AgentComment {
  pub comment_type: String,
  pub content: String,
}

// ============================================================================
// Utility: HTML to plain text conversion
// ============================================================================

/// Convert formatter HTML to plain text, preserving newlines and indentation.
///
/// Indentation is preserved via literal space characters baked into the
/// content by `formatting::inline_indent()`. Indent span tags are stripped
/// like any other HTML tag.
///
/// Handles special HTML patterns from the formatter:
/// - Placeholder `<div>` elements → skipped (empty, no content)
/// - All other tags (including indent spans) → stripped, content preserved
///
/// Also unescapes HTML entities and preserves literal `\n` characters.
///
/// Note: Inline comment injection happens at a higher level in
/// `convert_reference`, which prepends `// [type] comment` lines from
/// the `mention_topics` on each `Reference`. The placeholder divs in the
/// formatter HTML point to declarations (not comments) and are skipped.
fn html_to_plain_text(html: &str) -> String {
  let mut output = String::with_capacity(html.len());
  let mut pos = 0;
  let bytes = html.as_bytes();

  while pos < bytes.len() {
    if bytes[pos] == b'<' {
      // Find the end of this tag
      let tag_start = pos;
      let tag_end = match html[pos..].find('>') {
        Some(offset) => pos + offset + 1,
        None => break,
      };
      let tag_content = &html[tag_start + 1..tag_end - 1]; // between < and >

      // Skip placeholder divs (empty elements, content injected elsewhere)
      if is_placeholder_div(tag_content) {
        // Skip past the closing </div>
        if !tag_content.ends_with('/') {
          if let Some(close_offset) = html[tag_end..].find("</div>") {
            pos = tag_end + close_offset + 6;
          } else {
            pos = tag_end;
          }
        } else {
          pos = tag_end;
        }
        continue;
      }

      // All other tags (including indent spans): skip tag, keep content
      pos = tag_end;
    } else if bytes[pos] == b'&' {
      // Unescape HTML entities
      if html[pos..].starts_with("&amp;") {
        output.push('&');
        pos += 5;
      } else if html[pos..].starts_with("&lt;") {
        output.push('<');
        pos += 4;
      } else if html[pos..].starts_with("&gt;") {
        output.push('>');
        pos += 4;
      } else if html[pos..].starts_with("&quot;") {
        output.push('"');
        pos += 6;
      } else if html[pos..].starts_with("&#39;") {
        output.push('\'');
        pos += 5;
      } else {
        output.push('&');
        pos += 1;
      }
    } else {
      output.push(bytes[pos] as char);
      pos += 1;
    }
  }

  output.trim_end().to_string()
}

/// Check if a tag is a placeholder div (inline-comment placeholder).
fn is_placeholder_div(tag: &str) -> bool {
  tag.starts_with("div ") && tag.contains("inline-comment")
}

/// Simple HTML tag stripping for contexts where we don't need indent/placeholder
/// handling (e.g., comment content, documentation, names).
fn strip_html_tags(html: &str) -> String {
  let mut result = String::with_capacity(html.len());
  let mut in_tag = false;

  for ch in html.chars() {
    match ch {
      '<' => in_tag = true,
      '>' => in_tag = false,
      _ if !in_tag => result.push(ch),
      _ => {}
    }
  }

  // Collapse runs of whitespace into single spaces
  let mut collapsed = String::with_capacity(result.len());
  let mut last_was_space = false;
  for ch in result.chars() {
    if ch.is_whitespace() {
      if !last_was_space {
        collapsed.push(' ');
        last_was_space = true;
      }
    } else {
      collapsed.push(ch);
      last_was_space = false;
    }
  }

  collapsed.trim().to_string()
}

// ============================================================================
// Utility: Source text as plain text
// ============================================================================

/// Get source text for a topic as plain text by rendering via the formatter
/// and converting HTML to plain text. Preserves indentation and newlines,
/// and resolves inline comment placeholders.
fn get_plain_text(
  topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  // Check cache first (comments and pre-rendered entries)
  if let Some(html) = source_text_cache.get(topic.id()) {
    return html_to_plain_text(html);
  }

  // Check for global builtins
  if let Some(html) =
    crate::solidity::formatter::global_to_source_text(topic)
  {
    return html_to_plain_text(&html);
  }

  // Render from AST node and convert to plain text
  match audit_data.nodes.get(topic) {
    Some(Node::Solidity(solidity_node)) => {
      let html = crate::solidity::formatter::node_to_source_text(
        solidity_node,
        &audit_data.nodes,
        &audit_data.topic_metadata,
      );
      html_to_plain_text(&html)
    }
    Some(Node::Documentation(doc_node)) => {
      let html = crate::documentation::formatter::node_to_html(
        doc_node,
        &audit_data.nodes,
        &FormatContext {
          comment_formatting: false,
          target_topic: topic.clone(),
        },
      );
      strip_html_tags(&html)
    }
    None => topic.id().to_string(),
  }
}

// ============================================================================
// Utility: Topic name resolution
// ============================================================================

/// Resolve a topic to its display name (plain text, no HTML).
fn resolve_topic_name(
  topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  match audit_data.topic_metadata.get(topic) {
    Some(TopicMetadata::NamedTopic { name, .. }) => name.clone(),
    Some(TopicMetadata::TitledTopic { title, .. }) => title.clone(),
    Some(TopicMetadata::UnnamedTopic { kind, .. }) => {
      unnamed_kind_to_string(kind)
    }
    Some(TopicMetadata::ControlFlow {
      kind, condition, ..
    }) => {
      let keyword = control_flow_kind_to_string(kind);
      let cond_source =
        get_plain_text(condition, audit_data, source_text_cache);
      format!("{} ({})", keyword, cond_source)
    }
    Some(TopicMetadata::CommentTopic { comment_type, .. }) => {
      comment_type.clone()
    }
    None => topic.id().to_string(),
  }
}

fn unnamed_kind_to_string(kind: &UnnamedTopicKind) -> String {
  format!("{:?}", kind)
}

fn control_flow_kind_to_string(
  kind: &ControlFlowStatementKind,
) -> &'static str {
  match kind {
    ControlFlowStatementKind::If => "if",
    ControlFlowStatementKind::For => "for",
    ControlFlowStatementKind::While => "while",
    ControlFlowStatementKind::DoWhile => "do-while",
  }
}

// ============================================================================
// Utility: Scope breadcrumb
// ============================================================================

/// Build a scope breadcrumb string like "src/ERC20.sol > ERC20 > transfer".
fn scope_to_breadcrumb(
  scope: &Scope,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  match scope {
    Scope::Global => "Global".to_string(),
    Scope::Container { container } => container.file_path.clone(),
    Scope::Component {
      container,
      component,
    } => {
      let component_name =
        resolve_topic_name(component, audit_data, source_text_cache);
      format!("{} > {}", container.file_path, component_name)
    }
    Scope::Member {
      container,
      component,
      member,
      ..
    } => {
      let component_name =
        resolve_topic_name(component, audit_data, source_text_cache);
      let member_name =
        resolve_topic_name(member, audit_data, source_text_cache);
      format!(
        "{} > {} > {}",
        container.file_path, component_name, member_name
      )
    }
    Scope::ContainingBlock {
      container,
      component,
      member,
      ..
    } => {
      let component_name =
        resolve_topic_name(component, audit_data, source_text_cache);
      let member_name =
        resolve_topic_name(member, audit_data, source_text_cache);
      format!(
        "{} > {} > {}",
        container.file_path, component_name, member_name
      )
    }
  }
}

// ============================================================================
// Utility: Kind/visibility formatting
// ============================================================================

fn named_kind_to_string(kind: &NamedTopicKind) -> (String, Option<String>) {
  match kind {
    NamedTopicKind::Contract(contract_kind) => {
      ("Contract".to_string(), Some(format!("{:?}", contract_kind)))
    }
    NamedTopicKind::Function(function_kind) => {
      ("Function".to_string(), Some(format!("{:?}", function_kind)))
    }
    NamedTopicKind::StateVariable(mutability) => {
      ("StateVariable".to_string(), Some(format!("{:?}", mutability)))
    }
    kind => (format!("{:?}", kind), None),
  }
}

fn visibility_to_string(visibility: &NamedTopicVisibility) -> String {
  format!("{:?}", visibility)
}

// ============================================================================
// Utility: Control flow annotation rendering
// ============================================================================

/// Render an annotation string for an annotated block.
fn render_annotation(
  annotation_topic: &topic::Topic,
  annotation_kind: &BlockAnnotationKind,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  match annotation_kind {
    BlockAnnotationKind::If(_) => render_control_flow_condition(
      annotation_topic,
      "if",
      audit_data,
      source_text_cache,
    ),
    BlockAnnotationKind::While => render_control_flow_condition(
      annotation_topic,
      "while",
      audit_data,
      source_text_cache,
    ),
    BlockAnnotationKind::DoWhile => render_control_flow_condition(
      annotation_topic,
      "do-while",
      audit_data,
      source_text_cache,
    ),
    BlockAnnotationKind::For => render_control_flow_condition(
      annotation_topic,
      "for",
      audit_data,
      source_text_cache,
    ),
    BlockAnnotationKind::Unchecked => "unchecked".to_string(),
    BlockAnnotationKind::InlineAssembly => "assembly".to_string(),
  }
}

/// Extract the condition source for a control flow statement.
fn render_control_flow_condition(
  statement_topic: &topic::Topic,
  keyword: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  let condition_topic = match audit_data.nodes.get(statement_topic) {
    Some(Node::Solidity(ast_node)) => get_condition_topic(ast_node),
    _ => None,
  };

  match condition_topic {
    Some(cond_topic) => {
      let cond_source =
        get_plain_text(&cond_topic, audit_data, source_text_cache);
      format!("{} ({})", keyword, cond_source)
    }
    None => keyword.to_string(),
  }
}

/// Get the condition topic from a control flow AST node.
fn get_condition_topic(node: &ASTNode) -> Option<topic::Topic> {
  match node {
    ASTNode::IfStatement { condition, .. }
    | ASTNode::WhileStatement { condition, .. }
    | ASTNode::DoWhileStatement { condition, .. } => {
      Some(topic::new_node_topic(&condition.node_id()))
    }
    ASTNode::ForStatement { condition, .. } => {
      Some(topic::new_node_topic(&condition.node_id()))
    }
    _ => None,
  }
}

fn annotation_kind_to_string(kind: &BlockAnnotationKind) -> &'static str {
  match kind {
    BlockAnnotationKind::If(core::ControlFlowBranch::True) => "if_true",
    BlockAnnotationKind::If(core::ControlFlowBranch::False) => "if_false",
    BlockAnnotationKind::For => "for",
    BlockAnnotationKind::While => "while",
    BlockAnnotationKind::DoWhile => "do_while",
    BlockAnnotationKind::Unchecked => "unchecked",
    BlockAnnotationKind::InlineAssembly => "assembly",
  }
}

// ============================================================================
// Source Context Conversion
// ============================================================================

/// Convert a list of SourceContext groups to AgentSourceGroup entries.
fn convert_source_groups(
  groups: &[SourceContext],
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Vec<AgentSourceGroup> {
  groups
    .iter()
    .map(|group| convert_source_group(group, audit_data, source_text_cache))
    .collect()
}

fn convert_source_group(
  group: &SourceContext,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> AgentSourceGroup {
  let scope_name =
    resolve_topic_name(group.scope(), audit_data, source_text_cache);

  let scope_references = group
    .scope_references()
    .iter()
    .map(|r| convert_reference(r, audit_data, source_text_cache))
    .collect();

  let nested_references = group
    .nested_references()
    .iter()
    .map(|nested| {
      let subscope_name =
        resolve_topic_name(nested.subscope(), audit_data, source_text_cache);
      let children = convert_source_children(
        nested.children(),
        audit_data,
        source_text_cache,
      );
      AgentNestedGroup {
        subscope: subscope_name,
        children,
      }
    })
    .collect();

  AgentSourceGroup {
    scope: scope_name,
    in_scope: group.is_in_scope(),
    scope_references,
    nested_references,
  }
}

/// Recursively convert SourceChild entries to AgentSourceChild entries.
fn convert_source_children(
  children: &[SourceChild],
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Vec<AgentSourceChild> {
  children
    .iter()
    .map(|child| match child {
      SourceChild::Reference(reference) => {
        convert_reference(reference, audit_data, source_text_cache)
      }
      SourceChild::AnnotatedBlock(block) => {
        let annotation = block.annotation();
        let annotation_str = render_annotation(
          &annotation.topic,
          &annotation.kind,
          audit_data,
          source_text_cache,
        );
        let kind = annotation_kind_to_string(&annotation.kind).to_string();
        let children = convert_source_children(
          block.children(),
          audit_data,
          source_text_cache,
        );
        AgentSourceChild::AnnotatedBlock {
          annotation: annotation_str,
          kind,
          children,
        }
      }
    })
    .collect()
}

/// Convert a single Reference to an AgentSourceChild.
/// Mention comments are injected inline as `// [type] comment` lines
/// prepended before the code, mirroring how the topic view renders
/// comments above their associated references.
fn convert_reference(
  reference: &Reference,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> AgentSourceChild {
  let ref_topic = reference.reference_topic();

  // Get source text by rendering via formatter and converting to plain text
  let code = get_plain_text(ref_topic, audit_data, source_text_cache);

  // Resolve mention comments and inject inline before the code
  let mut comment_lines = Vec::new();
  let mut comments = Vec::new();

  if let Some(mention_topics) = reference.mention_topics() {
    for mention_topic in mention_topics {
      let content =
        get_plain_text(mention_topic, audit_data, source_text_cache);
      let content = content.trim().to_string();
      if content.is_empty() {
        continue;
      }

      let comment_type = match audit_data.topic_metadata.get(mention_topic) {
        Some(TopicMetadata::CommentTopic { comment_type, .. }) => {
          comment_type.clone()
        }
        _ => "note".to_string(),
      };

      // Build inline comment line: // [issue] comment text
      comment_lines.push(format!("// [{}] {}", comment_type, content));

      comments.push(AgentComment {
        comment_type,
        content,
      });
    }
  }

  // Prepend inline comment lines to the code
  let code = if comment_lines.is_empty() {
    code
  } else {
    comment_lines.push(code);
    comment_lines.join("\n")
  };

  let comments = if comments.is_empty() {
    None
  } else {
    Some(comments)
  };

  AgentSourceChild::Reference { code, comments }
}

// ============================================================================
// Public API: Build Agent Context
// ============================================================================

/// Build the agent context for a given topic.
///
/// Returns a resolved JSON-serializable structure where all topic IDs
/// are replaced with human-readable values (source code, names, etc.).
///
/// `source_text_cache` should be the audit's cached rendered HTML, used
/// to extract source text and comment content (HTML-stripped to plain text).
pub fn build_agent_context(
  topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
  include_expanded_context: bool,
) -> Option<AgentTopicContext> {
  let topic = topic::new_topic(topic_id);
  let metadata = audit_data.topic_metadata.get(&topic)?;

  let name = resolve_topic_name(&topic, audit_data, source_text_cache);
  let scope =
    scope_to_breadcrumb(metadata.scope(), audit_data, source_text_cache);

  let context =
    convert_source_groups(metadata.context(), audit_data, source_text_cache);
  let mentions =
    convert_source_groups(metadata.mentions(), audit_data, source_text_cache);

  match metadata {
    TopicMetadata::NamedTopic {
      kind,
      visibility,
      is_mutable,
      ancestors,
      descendants,
      relatives,
      expanded_context,
      ..
    } => {
      let (kind_str, sub_kind) = named_kind_to_string(kind);
      let visibility_str = visibility_to_string(visibility);

      let resolved_ancestors = if ancestors.is_empty() {
        None
      } else {
        Some(
          ancestors
            .iter()
            .map(|t| resolve_topic_name(t, audit_data, source_text_cache))
            .collect(),
        )
      };

      let resolved_descendants = if descendants.is_empty() {
        None
      } else {
        Some(
          descendants
            .iter()
            .map(|t| resolve_topic_name(t, audit_data, source_text_cache))
            .collect(),
        )
      };

      let resolved_relatives = if relatives.is_empty() {
        None
      } else {
        Some(
          relatives
            .iter()
            .map(|t| resolve_topic_name(t, audit_data, source_text_cache))
            .collect(),
        )
      };

      let expanded = if include_expanded_context {
        Some(convert_source_groups(
          expanded_context,
          audit_data,
          source_text_cache,
        ))
      } else {
        None
      };

      Some(AgentTopicContext {
        name,
        kind: kind_str,
        sub_kind,
        visibility: Some(visibility_str),
        scope,
        is_mutable: Some(*is_mutable),
        ancestors: resolved_ancestors,
        descendants: resolved_descendants,
        relatives: resolved_relatives,
        context,
        expanded_context: expanded,
        mentions,
      })
    }

    TopicMetadata::UnnamedTopic { kind, .. } => Some(AgentTopicContext {
      name,
      kind: unnamed_kind_to_string(kind),
      sub_kind: None,
      visibility: None,
      scope,
      is_mutable: None,
      ancestors: None,
      descendants: None,
      relatives: None,
      context,
      expanded_context: None,
      mentions,
    }),

    TopicMetadata::ControlFlow { kind, .. } => Some(AgentTopicContext {
      name,
      kind: control_flow_kind_to_string(kind).to_string(),
      sub_kind: None,
      visibility: None,
      scope,
      is_mutable: None,
      ancestors: None,
      descendants: None,
      relatives: None,
      context,
      expanded_context: None,
      mentions,
    }),

    TopicMetadata::TitledTopic { kind, .. } => {
      let kind_str = match kind {
        TitledTopicKind::DocumentationSection => "DocumentationSection",
      };

      Some(AgentTopicContext {
        name,
        kind: kind_str.to_string(),
        sub_kind: None,
        visibility: None,
        scope,
        is_mutable: None,
        ancestors: None,
        descendants: None,
        relatives: None,
        context,
        expanded_context: None,
        mentions,
      })
    }

    TopicMetadata::CommentTopic {
      comment_type,
      target_topic,
      ..
    } => {
      let target_name =
        resolve_topic_name(target_topic, audit_data, source_text_cache);

      Some(AgentTopicContext {
        name,
        kind: "Comment".to_string(),
        sub_kind: Some(comment_type.clone()),
        visibility: None,
        scope: format!("{} (on: {})", scope, target_name),
        is_mutable: None,
        ancestors: None,
        descendants: None,
        relatives: None,
        context,
        expanded_context: None,
        mentions,
      })
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::core::{
    self, NamedTopicKind, NamedTopicVisibility, Reference,
    insert_into_context, topic,
  };
  use crate::solidity::parser::{ASTNode, SourceLocation};
  use std::collections::{HashMap, HashSet};
  use std::path::PathBuf;

  fn dummy_src_location() -> SourceLocation {
    SourceLocation {
      start: None,
      length: None,
      index: None,
    }
  }

  fn test_project_path() -> core::ProjectPath {
    core::new_project_path(
      &"src/Test.sol".to_string(),
      &PathBuf::from("/project"),
    )
  }

  /// Build a minimal AuditData with:
  /// - A contract topic (scope)
  /// - A function topic (subscope)
  /// - A variable topic (the target of build_agent_context)
  /// - A reference to the variable inside the function, with a mention comment
  /// - A comment topic for the mention
  fn build_test_data() -> (AuditData, HashMap<String, String>) {
    let mut audit_data =
      core::new_audit_data("test".to_string(), HashSet::new());
    let mut source_text_cache = HashMap::new();

    let project_path = test_project_path();

    // Topics
    let contract_topic = topic::new_node_topic(&100); // contract MyContract
    let function_topic = topic::new_node_topic(&200); // function transfer
    let variable_topic = topic::new_node_topic(&300); // balanceOf
    let ref_node_topic = topic::new_node_topic(&301); // reference to balanceOf in transfer
    let comment_topic = topic::new_comment_topic(1); // an issue comment

    // AST nodes — minimal nodes so the formatter can render them
    // The variable declaration renders as "uint256"
    audit_data.nodes.insert(
      ref_node_topic.clone(),
      Node::Solidity(ASTNode::ElementaryTypeName {
        node_id: 301,
        src_location: dummy_src_location(),
        name: "uint256".to_string(),
      }),
    );

    // Put rendered HTML for the reference in the cache
    // (simulating what node_to_source_text would produce for a variable reference)
    source_text_cache.insert(
      ref_node_topic.id().to_string(),
      "<pre><code><span class=\"keyword\">pub</span> <span class=\"topic-token mutable-state-variable\">balanceOf</span>: <span class=\"type\">uint256</span></code></pre>".to_string(),
    );

    // Put rendered HTML for the comment in the cache
    source_text_cache.insert(
      comment_topic.id().to_string(),
      "<div class=\"comment-root\"><div class=\"paragraph\">This variable is vulnerable to overflow</div></div>".to_string(),
    );

    // Contract metadata (scope for the variable)
    audit_data.topic_metadata.insert(
      contract_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: contract_topic.clone(),
        scope: Scope::Container {
          container: project_path.clone(),
        },
        kind: NamedTopicKind::Contract(core::ContractKind::Contract),
        name: "MyContract".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: vec![],
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: false,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    // Function metadata (subscope)
    audit_data.topic_metadata.insert(
      function_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: function_topic.clone(),
        scope: Scope::Component {
          container: project_path.clone(),
          component: contract_topic.clone(),
        },
        kind: NamedTopicKind::Function(core::FunctionKind::Function),
        name: "transfer".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: vec![],
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: false,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    // Comment metadata
    audit_data.topic_metadata.insert(
      comment_topic.clone(),
      TopicMetadata::CommentTopic {
        topic: comment_topic.clone(),
        author_id: 1,
        comment_type: "issue".to_string(),
        target_topic: variable_topic.clone(),
        created_at: "2025-01-01".to_string(),
        scope: Scope::Global,
        mentioned_topics: vec![variable_topic.clone()],
        context: vec![],
        mentions: vec![],
      },
    );

    // Build context for the variable: a reference with mention in a function
    let reference = Reference::ProjectReferenceWithMentions {
      reference_topic: ref_node_topic.clone(),
      mention_topics: vec![comment_topic.clone()],
      sort_key: Some(0),
    };

    let mut context_groups: Vec<SourceContext> = vec![];
    insert_into_context(
      &mut context_groups,
      contract_topic.clone(),
      Some(0),
      true,
      Some((function_topic.clone(), Some(10))),
      &[],
      reference,
    );

    // Variable metadata (the target topic)
    audit_data.topic_metadata.insert(
      variable_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: variable_topic.clone(),
        scope: Scope::Component {
          container: project_path.clone(),
          component: contract_topic.clone(),
        },
        kind: NamedTopicKind::StateVariable(core::VariableMutability::Mutable),
        name: "balanceOf".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: context_groups,
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: true,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    (audit_data, source_text_cache)
  }

  #[test]
  fn test_comments_injected_inline_in_code() {
    let (audit_data, source_text_cache) = build_test_data();

    let result = build_agent_context(
      "N300",
      &audit_data,
      &source_text_cache,
      false,
    );

    let context = result.expect("should return agent context");

    // Find the reference in the context groups
    assert!(!context.context.is_empty(), "context should have groups");
    let group = &context.context[0];
    assert!(
      !group.nested_references.is_empty(),
      "group should have nested references"
    );
    let nested = &group.nested_references[0];
    assert!(!nested.children.is_empty(), "nested should have children");

    match &nested.children[0] {
      AgentSourceChild::Reference { code, comments } => {
        // The code should contain the inline comment
        assert!(
          code.contains("// [issue]"),
          "code should contain inline comment marker, got: {}",
          code
        );
        assert!(
          code.contains("vulnerable to overflow"),
          "code should contain comment text, got: {}",
          code
        );
        // The code should also contain the source text
        assert!(
          code.contains("balanceOf"),
          "code should contain source text, got: {}",
          code
        );

        // Structured comments should also be present
        let comments = comments.as_ref().expect("should have comments");
        assert_eq!(comments.len(), 1);
        assert_eq!(comments[0].comment_type, "issue");
        assert!(comments[0].content.contains("vulnerable to overflow"));
      }
      other => panic!(
        "expected Reference variant, got: {:?}",
        std::mem::discriminant(other)
      ),
    }
  }

  #[test]
  fn test_no_comments_when_no_mentions() {
    let mut audit_data =
      core::new_audit_data("test".to_string(), HashSet::new());
    let source_text_cache = HashMap::new();

    let project_path = test_project_path();
    let contract_topic = topic::new_node_topic(&100);
    let function_topic = topic::new_node_topic(&200);
    let variable_topic = topic::new_node_topic(&300);
    let ref_node_topic = topic::new_node_topic(&301);

    // Minimal AST node for the reference
    audit_data.nodes.insert(
      ref_node_topic.clone(),
      Node::Solidity(ASTNode::ElementaryTypeName {
        node_id: 301,
        src_location: dummy_src_location(),
        name: "uint256".to_string(),
      }),
    );

    // Contract + function metadata (for name resolution)
    audit_data.topic_metadata.insert(
      contract_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: contract_topic.clone(),
        scope: Scope::Container {
          container: project_path.clone(),
        },
        kind: NamedTopicKind::Contract(core::ContractKind::Contract),
        name: "MyContract".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: vec![],
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: false,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    audit_data.topic_metadata.insert(
      function_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: function_topic.clone(),
        scope: Scope::Component {
          container: project_path.clone(),
          component: contract_topic.clone(),
        },
        kind: NamedTopicKind::Function(core::FunctionKind::Function),
        name: "transfer".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: vec![],
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: false,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    // A plain reference with NO mentions
    let reference = Reference::ProjectReference {
      reference_topic: ref_node_topic.clone(),
      sort_key: Some(0),
    };

    let mut context_groups: Vec<SourceContext> = vec![];
    insert_into_context(
      &mut context_groups,
      contract_topic.clone(),
      Some(0),
      true,
      Some((function_topic.clone(), Some(10))),
      &[],
      reference,
    );

    audit_data.topic_metadata.insert(
      variable_topic.clone(),
      TopicMetadata::NamedTopic {
        topic: variable_topic.clone(),
        scope: Scope::Component {
          container: project_path.clone(),
          component: contract_topic.clone(),
        },
        kind: NamedTopicKind::StateVariable(core::VariableMutability::Mutable),
        name: "balanceOf".to_string(),
        visibility: NamedTopicVisibility::Public,
        context: context_groups,
        expanded_context: vec![],
        ancestry: vec![],
        is_mutable: true,
        mutations: vec![],
        ancestors: vec![],
        descendants: vec![],
        relatives: vec![],
        mentions: vec![],
      },
    );

    let result = build_agent_context(
      "N300",
      &audit_data,
      &source_text_cache,
      false,
    );

    let context = result.expect("should return agent context");
    let group = &context.context[0];
    let nested = &group.nested_references[0];

    match &nested.children[0] {
      AgentSourceChild::Reference { code, comments } => {
        // No comment markers should appear in the code
        assert!(
          !code.contains("//"),
          "code should not contain comment markers, got: {}",
          code
        );
        // No structured comments
        assert!(
          comments.is_none(),
          "should have no comments"
        );
      }
      other => panic!(
        "expected Reference variant, got: {:?}",
        std::mem::discriminant(other)
      ),
    }
  }

  #[test]
  fn test_html_to_plain_text_preserves_indentation() {
    use crate::formatting;

    // Build HTML using actual formatting functions so literal spaces match
    let inner = formatting::inline_indent("== 0", 1);
    let body = format!("x\n{}", inner);
    let indented = formatting::indent(&body, 1);
    let html = format!("<pre><code>if ({}\n)</code></pre>", indented);

    let result = html_to_plain_text(&html);
    // inner inline_indent adds 2 spaces to "== 0", outer indent adds 2 more
    assert_eq!(result, "if (\n  x\n    == 0\n)");
  }

  #[test]
  fn test_html_to_plain_text_nested_indentation() {
    use crate::formatting;

    // Simulates nested indent spans (contract body > block body)
    // Each indent() call adds 2 literal spaces per line
    let inner_body = "stmt1\n\nstmt2";
    let inner = formatting::indent(inner_body, 1);
    let outer_body = format!("function foo() {{{}\n}}", inner);
    let html = formatting::inline_indent(&outer_body, 0);

    let result = html_to_plain_text(&html);
    assert_eq!(
      result,
      "  function foo() {\n    stmt1\n\n    stmt2\n  }"
    );
  }

  #[test]
  fn test_html_to_plain_text_unescapes_entities() {
    let html = "<span>x &gt; 0 &amp;&amp; y &lt; 10</span>";
    let result = html_to_plain_text(html);
    assert_eq!(result, "x > 0 && y < 10");
  }

  #[test]
  fn test_html_to_plain_text_skips_placeholder_divs() {
    let html = "<div class=\"identifier-inline-comment\" data-placeholder-topic=\"N123\"></div><span>balanceOf</span>";
    let result = html_to_plain_text(html);
    assert_eq!(result, "balanceOf");
  }

  #[test]
  fn test_strip_html_tags_collapses_whitespace() {
    let html =
      "<div>  some  <span>  text  </span>  here  </div>";
    let result = strip_html_tags(html);
    assert_eq!(result, "some text here");
  }
}
