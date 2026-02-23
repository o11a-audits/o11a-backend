use serde::Serialize;

use crate::core::{
  self, AuditData, BlockAnnotationKind, ContractKind, ControlFlowBranch,
  ControlFlowStatementKind, FunctionKind, NamedTopicKind, NamedTopicVisibility,
  Node, Reference, Scope, SourceChild, SourceContext, TitledTopicKind,
  TopicMetadata, UnnamedTopicKind, VariableMutability, topic,
};
use crate::documentation::FormatContext;
use crate::formatting::html_escape;

// ============================================================================
// Response Types
// ============================================================================

#[derive(Debug, Serialize)]
pub struct TopicViewResponse {
  pub topic_panel_html: String,
  pub expanded_references_panel_html: String,
  pub breadcrumb_html: String,
  pub highlight_css: String,
}

#[derive(Debug, Serialize)]
pub struct MentionsPanelResponse {
  pub mentions_panel_html: String,
}

// ============================================================================
// CSS Constants (replicated from Gleam frontend)
// ============================================================================

const COMBINED_PANEL_STYLE: &str = "border-color: var(--color-body-border); border-right-width: 1px; border-right-style: solid; border-left-width: 1px; border-left-style: solid; border-bottom-width: 1px; border-bottom-style: dashed; padding: 0.5rem; background: var(--color-code-bg); max-height: 100%;";

const COMBINED_PANEL_FIRST_STYLE: &str = "border-top-width: 1px; border-top-style: solid; border-top-right-radius: 8px; border-top-left-radius: 8px;";

const COMBINED_PANEL_LAST_STYLE: &str = "border-bottom-right-radius: 8px; border-bottom-left-radius: 8px; border-bottom-width: 1px; border-bottom-style: solid;";

const COMBINED_PANEL_MEMBER_TITLE_STYLE: &str = "outline: 1px solid var(--color-body-border); border-radius: 4px; margin-bottom: 0.5rem; background: var(--color-body-bg); padding-left: 0.5rem;";

const SCOPE_STYLE: &str = "position: relative; display: inline-flex; align-items: center; gap: 0.25rem; margin-bottom: 0.5rem; padding-right: 0.5rem; direction: rtl; overflow: hidden;";

const SCOPE_ITEM_STYLE: &str =
  "color: var(--color-body-text); white-space: nowrap;";

const SCOPE_CHEVRON_STYLE: &str = "display: inline-flex; align-items: center; opacity: 0.6; width: 0.75em; height: 0.75em; line-height: 1; flex-shrink: 0;";

const SCOPE_OVERFLOW_GRADIENT_STYLE: &str = "display: none;";

const OUT_OF_SCOPE_BORDER: &str =
  "border-color: var(--color-body-out-of-scope-bg)";

const CHEVRON_RIGHT_SVG: &str = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" style=\"display: block;\"><path d=\"m9 18 6-6-6-6\"/></svg>";

// ============================================================================
// Highlighted Name
// ============================================================================

fn kw(text: &str) -> String {
  format!("<span class=\"keyword\">{}</span>", text)
}

fn visibility_kw(visibility: &NamedTopicVisibility) -> String {
  match visibility {
    NamedTopicVisibility::Public => format!("{} ", kw("pub")),
    NamedTopicVisibility::Private => format!("{} ", kw("priv")),
    NamedTopicVisibility::Internal => format!("{} ", kw("int")),
    NamedTopicVisibility::External => format!("{} ", kw("ext")),
  }
}

fn contract_kind_to_keyword(kind: &ContractKind) -> &'static str {
  match kind {
    ContractKind::Contract => "contract",
    ContractKind::Interface => "interface",
    ContractKind::Library => "library",
    ContractKind::Abstract => "abstract",
  }
}

/// Produces `<code>...</code>` HTML for a topic's highlighted name.
/// Mirrors the Gleam `topic_metadata_highlighted_name` function.
pub fn highlighted_name(metadata: &TopicMetadata) -> String {
  let inner = match metadata {
    TopicMetadata::NamedTopic {
      name,
      kind,
      visibility,
      is_mutable,
      ..
    } => match (kind, *is_mutable) {
      (NamedTopicKind::Contract(contract_kind), _) => {
        format!(
          "{} <span class=\"contract\">{}</span>",
          kw(contract_kind_to_keyword(contract_kind)),
          html_escape(name)
        )
      }
      (NamedTopicKind::Function(FunctionKind::Function), _)
      | (NamedTopicKind::Function(FunctionKind::FreeFunction), _) => {
        format!(
          "{}{} <span class=\"function\">{}</span>",
          visibility_kw(visibility),
          kw("fn"),
          html_escape(name)
        )
      }
      (NamedTopicKind::Function(FunctionKind::Receive), _) => {
        format!("{}{}", visibility_kw(visibility), kw("receive"))
      }
      (NamedTopicKind::Function(FunctionKind::Fallback), _) => {
        format!("{}{}", visibility_kw(visibility), kw("fallback"))
      }
      (NamedTopicKind::Function(FunctionKind::Constructor), _) => {
        kw("constructor")
      }
      (NamedTopicKind::Modifier, _) => {
        format!(
          "{} <span class=\"modifier\">{}</span>",
          kw("mod"),
          html_escape(name)
        )
      }
      (NamedTopicKind::Event, _) => {
        format!(
          "{}{} <span class=\"event\">{}</span>",
          visibility_kw(visibility),
          kw("event"),
          html_escape(name)
        )
      }
      (NamedTopicKind::Error, _) => {
        format!(
          "{}{} <span class=\"error\">{}</span>",
          visibility_kw(visibility),
          kw("error"),
          html_escape(name)
        )
      }
      (NamedTopicKind::Struct, _) => {
        format!(
          "{}{} <span class=\"struct\">{}</span>",
          visibility_kw(visibility),
          kw("struct"),
          html_escape(name)
        )
      }
      (NamedTopicKind::Enum, _) => {
        format!(
          "{}{} <span class=\"enum\">{}</span>",
          visibility_kw(visibility),
          kw("enum"),
          html_escape(name)
        )
      }
      (NamedTopicKind::EnumMember, _) => {
        format!("<span class=\"enum-value\">{}</span>", html_escape(name))
      }
      (NamedTopicKind::StateVariable(_), true)
      | (NamedTopicKind::StateVariable(VariableMutability::Mutable), _) => {
        format!(
          "{}<span class=\"mutable-state-variable\">{}</span>",
          visibility_kw(visibility),
          html_escape(name)
        )
      }
      (NamedTopicKind::StateVariable(VariableMutability::Constant), false) => {
        format!(
          "{}{} <span class=\"constant\">{}</span>",
          visibility_kw(visibility),
          kw("const"),
          html_escape(name)
        )
      }
      (NamedTopicKind::StateVariable(VariableMutability::Immutable), false) => {
        format!(
          "{}{} <span class=\"immutable-state-variable\">{}</span>",
          visibility_kw(visibility),
          kw("immutable"),
          html_escape(name)
        )
      }
      (NamedTopicKind::LocalVariable, true) => {
        format!(
          "<span class=\"mutable-local-variable\">{}</span>",
          html_escape(name)
        )
      }
      (NamedTopicKind::LocalVariable, false) => {
        format!(
          "<span class=\"local-variable\">{}</span>",
          html_escape(name)
        )
      }
      (NamedTopicKind::Builtin, _) => {
        format!("<span class=\"global\">{}</span>", html_escape(name))
      }
    },
    TopicMetadata::TitledTopic { title, kind, .. } => match kind {
      TitledTopicKind::DocumentationSection => {
        format!("<span>{}</span>", html_escape(title))
      }
    },
    TopicMetadata::UnnamedTopic { kind, .. } => match kind {
      UnnamedTopicKind::VariableMutation => {
        "<span class=\"keyword\">MutationStatement</span>".to_string()
      }
      UnnamedTopicKind::Arithmetic => {
        "<span class=\"operator\">ArithmeticExpression</span>".to_string()
      }
      UnnamedTopicKind::Comparison => {
        "<span class=\"operator\">ComparisonExpression</span>".to_string()
      }
      UnnamedTopicKind::Logical => {
        "<span class=\"operator\">BooleanExpression</span>".to_string()
      }
      UnnamedTopicKind::Bitwise => {
        "<span class=\"operator\">BitwiseExpression</span>".to_string()
      }
      UnnamedTopicKind::Conditional => {
        "<span class=\"keyword\">ConditionalStatement</span>".to_string()
      }
      UnnamedTopicKind::FunctionCall => {
        "<span class=\"function\">FunctionCall</span>".to_string()
      }
      UnnamedTopicKind::TypeConversion => {
        "<span class=\"operator\">TypeConversion</span>".to_string()
      }
      UnnamedTopicKind::StructConstruction => {
        "<span class=\"struct\">StructConstruction</span>".to_string()
      }
      UnnamedTopicKind::NewExpression => {
        "<span class=\"keyword\">NewExpression</span>".to_string()
      }
      UnnamedTopicKind::SemanticBlock => {
        "<span class=\"block\">ContainingBlock</span>".to_string()
      }
      UnnamedTopicKind::Break => {
        "<span class=\"keyword\">BreakStatement</span>".to_string()
      }
      UnnamedTopicKind::Continue => {
        "<span class=\"keyword\">ContinueStatement</span>".to_string()
      }
      UnnamedTopicKind::Emit => {
        "<span class=\"keyword\">EmitStatement</span>".to_string()
      }
      UnnamedTopicKind::InlineAssembly => {
        "<span class=\"keyword\">InlineAssembly</span>".to_string()
      }
      UnnamedTopicKind::Placeholder => {
        "<span class=\"keyword\">PlaceholderStatement</span>".to_string()
      }
      UnnamedTopicKind::Return => {
        "<span class=\"keyword\">ReturnStatement</span>".to_string()
      }
      UnnamedTopicKind::Revert => {
        "<span class=\"keyword\">RevertStatement</span>".to_string()
      }
      UnnamedTopicKind::Try => {
        "<span class=\"keyword\">TryStatement</span>".to_string()
      }
      UnnamedTopicKind::UncheckedBlock => {
        "<span class=\"keyword\">UncheckedBlock</span>".to_string()
      }
      UnnamedTopicKind::Reference => {
        "<span class=\"identifier\">Reference</span>".to_string()
      }
      UnnamedTopicKind::MutableReference => {
        "<span class=\"identifier\">MutableReference</span>".to_string()
      }
      UnnamedTopicKind::Signature => {
        "<span class=\"identifier\">Signature</span>".to_string()
      }
      UnnamedTopicKind::DocumentationRoot => {
        "<span>Documentation</span>".to_string()
      }
      UnnamedTopicKind::DocumentationHeading => {
        "<span>DocumentationHeading</span>".to_string()
      }
      UnnamedTopicKind::DocumentationParagraph => {
        "<span>DocumentationParagraph</span>".to_string()
      }
      UnnamedTopicKind::DocumentationSentence => {
        "<span>DocumentationSentence</span>".to_string()
      }
      UnnamedTopicKind::DocumentationCodeBlock => {
        "<span>DocumentationCodeBlock</span>".to_string()
      }
      UnnamedTopicKind::DocumentationList => {
        "<span>DocumentationList</span>".to_string()
      }
      UnnamedTopicKind::DocumentationBlockQuote => {
        "<span>DocumentationBlockQuote</span>".to_string()
      }
      UnnamedTopicKind::DocumentationInlineCode => {
        "<span>DocumentationInlineCode</span>".to_string()
      }
      UnnamedTopicKind::Literal => {
        "<span class=\"literal\">Literal</span>".to_string()
      }
      UnnamedTopicKind::LoopExpression => {
        "<span class=\"keyword\">LoopExpression</span>".to_string()
      }
      UnnamedTopicKind::Other => "<span>Other</span>".to_string(),
    },
    TopicMetadata::ControlFlow { kind, .. } => match kind {
      ControlFlowStatementKind::If => {
        "<span class=\"keyword\">IfStatement</span>".to_string()
      }
      ControlFlowStatementKind::For => {
        "<span class=\"keyword\">ForStatement</span>".to_string()
      }
      ControlFlowStatementKind::While => {
        "<span class=\"keyword\">WhileStatement</span>".to_string()
      }
      ControlFlowStatementKind::DoWhile => {
        "<span class=\"keyword\">DoWhileStatement</span>".to_string()
      }
    },
    TopicMetadata::CommentTopic { .. } => "<span>Comment</span>".to_string(),
  };

  format!("<code>{}</code>", inner)
}

// ============================================================================
// Breadcrumb Rendering
// ============================================================================

/// A part of a breadcrumb - either a file name string or a topic with metadata
enum BreadcrumbPart<'a> {
  Text(&'a str),
  Topic(&'a topic::Topic),
}

/// Gets the fully-qualified breadcrumb parts for a topic based on its scope.
/// Returns parts in display order (leftmost first).
fn get_breadcrumb_parts<'a>(
  metadata: &'a TopicMetadata,
) -> Vec<BreadcrumbPart<'a>> {
  match metadata.scope() {
    Scope::Global => {
      vec![
        BreadcrumbPart::Text("global"),
        BreadcrumbPart::Topic(metadata.topic()),
      ]
    }
    Scope::Container { container } => {
      vec![
        BreadcrumbPart::Text(&container.file_path),
        BreadcrumbPart::Topic(metadata.topic()),
      ]
    }
    Scope::Component {
      container,
      component,
      ..
    } => {
      vec![
        BreadcrumbPart::Text(&container.file_path),
        BreadcrumbPart::Topic(component),
        BreadcrumbPart::Topic(metadata.topic()),
      ]
    }
    Scope::Member {
      container,
      component,
      member,
      ..
    }
    | Scope::ContainingBlock {
      container,
      component,
      member,
      ..
    } => {
      vec![
        BreadcrumbPart::Text(&container.file_path),
        BreadcrumbPart::Topic(component),
        BreadcrumbPart::Topic(member),
        BreadcrumbPart::Topic(metadata.topic()),
      ]
    }
  }
}

/// Render just the highlighted name of a scope topic (without the container path).
fn render_scope_name(
  scope_topic: &topic::Topic,
  audit_data: &AuditData,
) -> String {
  let name_html = match audit_data.topic_metadata.get(scope_topic) {
    Some(metadata) => highlighted_name(metadata),
    None => "<code>?</code>".to_string(),
  };
  format!("<span style=\"{}\">{}</span>", SCOPE_ITEM_STYLE, name_html)
}

/// Render a breadcrumb for a scope topic.
/// Produces the same HTML structure as the Gleam `mount_breadcrumb_parts` function.
fn render_breadcrumb(
  scope_topic: &topic::Topic,
  audit_data: &AuditData,
) -> String {
  let parts = match audit_data.topic_metadata.get(scope_topic) {
    Some(metadata) => get_breadcrumb_parts(metadata),
    None => vec![BreadcrumbPart::Text("?")],
  };

  render_breadcrumb_parts(&parts, audit_data)
}

/// Render the fully-qualified breadcrumb for the history bar (uses the active topic's metadata).
pub fn render_history_breadcrumb(
  metadata: &TopicMetadata,
  audit_data: &AuditData,
) -> String {
  let parts = get_breadcrumb_parts(metadata);
  render_breadcrumb_parts(&parts, audit_data)
}

/// Render breadcrumb parts into HTML.
/// The container has `direction: rtl` so parts are reversed in the HTML.
fn render_breadcrumb_parts(
  parts: &[BreadcrumbPart],
  audit_data: &AuditData,
) -> String {
  let mut html = String::new();

  // Gradient overlay (hidden by default, frontend can show it on overflow)
  html.push_str(&format!(
    "<div style=\"{}\"></div>",
    SCOPE_OVERFLOW_GRADIENT_STYLE
  ));

  // Reverse because container has direction: rtl
  for (index, part) in parts.iter().rev().enumerate() {
    // Add chevron delimiter before each item except the first
    if index > 0 {
      html.push_str(&format!(
        "<span style=\"{}\">{}</span>",
        SCOPE_CHEVRON_STYLE, CHEVRON_RIGHT_SVG
      ));
    }

    match part {
      BreadcrumbPart::Text(name) => {
        html.push_str(&format!(
          "<code style=\"{}\">{}</code>",
          SCOPE_ITEM_STYLE,
          html_escape(name)
        ));
      }
      BreadcrumbPart::Topic(topic) => {
        let name_html = match audit_data.topic_metadata.get(topic) {
          Some(metadata) => highlighted_name(metadata),
          None => "<code>?</code>".to_string(),
        };
        html.push_str(&format!(
          "<span style=\"{}\">{}</span>",
          SCOPE_ITEM_STYLE, name_html
        ));
      }
    }
  }

  html
}

// ============================================================================
// Subscope Title
// ============================================================================

/// Render a subscope member title div.
fn render_subscope_title(
  subscope_topic: &topic::Topic,
  audit_data: &AuditData,
) -> String {
  let name_html = match audit_data.topic_metadata.get(subscope_topic) {
    Some(metadata) => highlighted_name(metadata),
    None => format!("<code>{}</code>", html_escape(subscope_topic.id())),
  };
  format!(
    "<div style=\"{}\">{}</div>",
    COMBINED_PANEL_MEMBER_TITLE_STYLE, name_html
  )
}

// ============================================================================
// Source Text Helpers
// ============================================================================

/// Get or render source text HTML for a topic.
fn get_source_text(
  topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  // Check cache first
  if let Some(html) = source_text_cache.get(topic.id()) {
    return html.clone();
  }

  // Check for global builtins
  if let Some(global) = crate::solidity::formatter::global_to_source_text(topic)
  {
    return global;
  }

  // Render from AST node
  match audit_data.nodes.get(topic) {
    Some(Node::Solidity(solidity_node)) => {
      crate::solidity::formatter::node_to_source_text(
        solidity_node,
        &audit_data.nodes,
        &audit_data.topic_metadata,
      )
    }
    Some(Node::Documentation(doc_node)) => {
      crate::documentation::formatter::node_to_html(
        doc_node,
        &audit_data.nodes,
        &FormatContext {
          comment_formatting: false,
          target_topic: topic.clone(),
        },
      )
    }
    None => format!(
      "<div class=\"error\">Source text not found for {}</div>",
      html_escape(topic.id())
    ),
  }
}

// ============================================================================
// First/Last Border Styling
// ============================================================================

/// Build inline style string with first/last border styles applied.
fn first_last_style(index: usize, total: usize) -> String {
  let mut style = String::new();
  if index == 0 {
    style.push_str(COMBINED_PANEL_FIRST_STYLE);
    style.push(' ');
  }
  if index == total - 1 {
    style.push_str(COMBINED_PANEL_LAST_STYLE);
    style.push(' ');
  }
  style
}

// ============================================================================
// Indent Wrapping
// ============================================================================

/// Wrap content in nested indent divs based on depth.
fn wrap_in_indent(content: &str, depth: usize) -> String {
  if depth == 0 {
    return content.to_string();
  }
  let mut result = content.to_string();
  for _ in 0..depth {
    result = format!("<div class=\"indent\">{}</div>", result);
  }
  result
}

// ============================================================================
// Count Blocks
// ============================================================================

/// Count total visual blocks for a list of SourceChild items.
fn count_source_child_blocks(children: &[SourceChild]) -> usize {
  children.iter().fold(0, |acc, child| match child {
    SourceChild::Reference(_) => acc + 1,
    SourceChild::AnnotatedBlock(block) => {
      // Opening block + children + closing block
      acc + 2 + count_source_child_blocks(block.children())
    }
  })
}

// ============================================================================
// Reference Source Rendering
// ============================================================================

/// Render a single reference's source HTML (source text + mention comments).
/// Placeholders for inline info comments are left in place for the frontend.
fn render_reference_source(
  reference: &Reference,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  let ref_topic = reference.reference_topic();
  let source_text = get_source_text(ref_topic, audit_data, source_text_cache);

  match reference {
    Reference::ProjectReference { .. } => {
      format!("<div>{}</div>", source_text)
    }
    Reference::ProjectReferenceWithMentions { mention_topics, .. }
    | Reference::CommentMention { mention_topics, .. } => {
      let mut html = String::new();

      // Render mention comments before the source
      html.push_str("<div>");
      for mention_topic in mention_topics {
        let comment_html =
          get_source_text(mention_topic, audit_data, source_text_cache);
        html.push_str(&format!(
          "<div class=\"inline-comment code-style\"><div>{}</div></div>",
          comment_html
        ));
      }
      html.push_str("</div>");

      // Then the source text
      html.push_str(&format!("<div>{}</div>", source_text));

      html
    }
  }
}

// ============================================================================
// Control Flow Delimiter Rendering
// ============================================================================

/// Render opening delimiter for an annotated block.
fn render_opening_delimiter(
  annotation_topic: &topic::Topic,
  annotation_kind: &BlockAnnotationKind,
  has_sibling_branch: bool,
  audit_data: &AuditData,
) -> String {
  let kw_html = |text: &str| format!("<span class=\"keyword\">{}</span>", text);

  match annotation_kind {
    BlockAnnotationKind::If(ControlFlowBranch::False)
      if !has_sibling_branch =>
    {
      // False branch without sibling: render "if (cond) {" then "} else {"
      let delimiter_html = get_delimiter_opening(annotation_topic, audit_data);
      format!(
        "<div>{}</div><div>}} {} {{</div>",
        delimiter_html,
        kw_html("else")
      )
    }
    BlockAnnotationKind::If(ControlFlowBranch::False) => {
      // False branch with sibling: just render "} else {"
      format!("<div>}} {} {{</div>", kw_html("else"))
    }
    _ => {
      // For, while, if true, do-while, unchecked, assembly: render opening delimiter
      let delimiter_html = get_delimiter_opening(annotation_topic, audit_data);
      format!("<div>{}</div>", delimiter_html)
    }
  }
}

/// Render closing delimiter for an annotated block.
fn render_closing_delimiter(
  annotation_topic: &topic::Topic,
  annotation_kind: &BlockAnnotationKind,
  has_sibling_branch: bool,
  audit_data: &AuditData,
) -> String {
  let kw_html = |text: &str| format!("<span class=\"keyword\">{}</span>", text);

  match (annotation_kind, has_sibling_branch) {
    (BlockAnnotationKind::If(ControlFlowBranch::True), true) => {
      format!("<div>}} {} {{</div>", kw_html("else"))
    }
    (BlockAnnotationKind::DoWhile, _) => {
      // "} while (cond)"
      let closing_html = get_delimiter_closing(annotation_topic, audit_data);
      match closing_html {
        Some(closing) => format!("<div>}} {}</div>", closing),
        None => "<div>}</div>".to_string(),
      }
    }
    _ => "<div>}</div>".to_string(),
  }
}

/// Get the opening delimiter HTML from the formatter.
fn get_delimiter_opening(
  topic: &topic::Topic,
  audit_data: &AuditData,
) -> String {
  match audit_data.nodes.get(topic) {
    Some(Node::Solidity(node)) => {
      match crate::solidity::formatter::node_to_delimiter(
        node,
        &audit_data.nodes,
        &audit_data.topic_metadata,
      ) {
        Some(delimiter) => delimiter.opening,
        None => "<code>...</code>".to_string(),
      }
    }
    _ => "<code>...</code>".to_string(),
  }
}

/// Get the closing delimiter HTML from the formatter.
fn get_delimiter_closing(
  topic: &topic::Topic,
  audit_data: &AuditData,
) -> Option<String> {
  match audit_data.nodes.get(topic) {
    Some(Node::Solidity(node)) => {
      crate::solidity::formatter::node_to_delimiter(
        node,
        &audit_data.nodes,
        &audit_data.topic_metadata,
      )
      .and_then(|d| d.closing)
    }
    _ => None,
  }
}

// ============================================================================
// Control Flow Syntax Block
// ============================================================================

/// Render a control flow syntax block as part of the dashed-border chain.
fn render_control_flow_syntax_block(
  content: &str,
  is_in_scope: bool,
  index: usize,
  total: usize,
  depth: usize,
) -> String {
  let mut style = String::from(COMBINED_PANEL_STYLE);
  style.push(' ');
  if !is_in_scope {
    style.push_str(OUT_OF_SCOPE_BORDER);
    style.push_str("; ");
  }
  style.push_str(&first_last_style(index, total));

  let wrapped = wrap_in_indent(content, depth);

  format!("<div style=\"{}\">{}</div>", style, wrapped)
}

// ============================================================================
// Source Children Rendering
// ============================================================================

/// Render a list of SourceChild items in source order.
/// Returns (html, next_index).
fn render_source_children(
  children: &[SourceChild],
  scope: &topic::Topic,
  is_in_scope: bool,
  current_index: usize,
  total_references: usize,
  depth: usize,
  subscope_title: Option<&topic::Topic>,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> (String, usize) {
  let mut html = String::new();
  let mut index = current_index;
  let mut remaining_title = subscope_title;

  for child in children {
    match child {
      SourceChild::Reference(reference) => {
        let ref_topic = reference.reference_topic();

        let mut container_style = String::from(COMBINED_PANEL_STYLE);
        container_style.push_str(" padding-left: 0.5rem;");
        if !is_in_scope {
          container_style.push(' ');
          container_style.push_str(OUT_OF_SCOPE_BORDER);
          container_style.push(';');
        }
        container_style.push(' ');
        container_style.push_str(&first_last_style(index, total_references));

        // Build subscope title if this is the first child
        let title_html = match remaining_title.take() {
          Some(title_topic) => render_subscope_title(title_topic, audit_data),
          None => String::new(),
        };

        let source_html =
          render_reference_source(reference, audit_data, source_text_cache);
        let indented_source = wrap_in_indent(&source_html, depth);

        html.push_str(&format!(
          "<div><div class=\"source-container\" data-topic=\"{}\" data-contract=\"{}\" style=\"{}\">{}{}</div></div>",
          html_escape(ref_topic.id()),
          html_escape(scope.id()),
          container_style,
          title_html,
          indented_source
        ));

        index += 1;
      }
      SourceChild::AnnotatedBlock(block) => {
        let (block_html, next_index) = render_annotated_block_group(
          block.annotation(),
          block.children(),
          block.has_sibling_branch(),
          scope,
          is_in_scope,
          index,
          total_references,
          remaining_title.take(),
          depth,
          audit_data,
          source_text_cache,
        );
        html.push_str(&block_html);
        index = next_index;
      }
    }
  }

  (html, index)
}

// ============================================================================
// Annotated Block Group Rendering
// ============================================================================

/// Render an annotated block group with opening/closing syntax and indented children.
/// Returns (html, next_index).
fn render_annotated_block_group(
  annotation: &core::BlockAnnotation,
  children: &[SourceChild],
  has_sibling_branch: bool,
  scope: &topic::Topic,
  is_in_scope: bool,
  current_index: usize,
  total_references: usize,
  subscope_title: Option<&topic::Topic>,
  depth: usize,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> (String, usize) {
  let mut html = String::new();

  // Opening syntax block
  let mut opening_content = String::new();

  // Mount subscope title if provided
  if let Some(title_topic) = subscope_title {
    opening_content.push_str(&render_subscope_title(title_topic, audit_data));
  }

  // Render opening delimiter
  opening_content.push_str(&render_opening_delimiter(
    &annotation.topic,
    &annotation.kind,
    has_sibling_branch,
    audit_data,
  ));

  html.push_str(&render_control_flow_syntax_block(
    &opening_content,
    is_in_scope,
    current_index,
    total_references,
    depth,
  ));

  // Render children
  let (children_html, index_after_children) = render_source_children(
    children,
    scope,
    is_in_scope,
    current_index + 1,
    total_references,
    depth + 1,
    None,
    audit_data,
    source_text_cache,
  );
  html.push_str(&children_html);

  // Closing syntax block
  let closing_content = render_closing_delimiter(
    &annotation.topic,
    &annotation.kind,
    has_sibling_branch,
    audit_data,
  );

  html.push_str(&render_control_flow_syntax_block(
    &closing_content,
    is_in_scope,
    index_after_children,
    total_references,
    depth,
  ));

  (html, index_after_children + 1)
}

// ============================================================================
// Grouped Source Panel Rendering
// ============================================================================

/// Render a complete panel from a list of SourceContext groups.
/// This is the main entry point for rendering topic, mentions, expanded references, and comments panels.
/// When `scope_name_only` is true, the scope header renders just the component's
/// highlighted name instead of the full breadcrumb (used for the topic context panel).
pub fn render_grouped_source_panel(
  groups: &[SourceContext],
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
  scope_name_only: bool,
) -> String {
  let mut html = String::new();

  for group in groups {
    html.push_str(
      "<div class=\"reference-group\" style=\"margin-bottom: 0.5rem;\">",
    );

    // Render scope breadcrumb
    html.push_str(&format!(
      "<div class=\"topic-reference-title scope-standard\" style=\"{}\">",
      SCOPE_STYLE
    ));
    if scope_name_only {
      html.push_str(&render_scope_name(group.scope(), audit_data));
    } else {
      html.push_str(&render_breadcrumb(group.scope(), audit_data));
    }
    html.push_str("</div>");

    // Calculate total reference count for first/last styling
    let total_references = group.scope_references().len()
      + group
        .nested_references()
        .iter()
        .map(|nested| count_source_child_blocks(nested.children()))
        .sum::<usize>();

    // Render scope-level references
    let mut index = 0;
    for ref_entry in group.scope_references() {
      let ref_topic = ref_entry.reference_topic();

      let mut container_style = String::from(COMBINED_PANEL_STYLE);
      container_style.push_str(" padding-left: 0.5rem;");
      if !group.is_in_scope() {
        container_style.push(' ');
        container_style.push_str(OUT_OF_SCOPE_BORDER);
        container_style.push(';');
      }
      container_style.push(' ');
      container_style.push_str(&first_last_style(index, total_references));

      let source_html =
        render_reference_source(ref_entry, audit_data, source_text_cache);

      html.push_str(&format!(
        "<div><div class=\"source-container\" data-topic=\"{}\" data-contract=\"{}\" style=\"{}\">{}</div></div>",
        html_escape(ref_topic.id()),
        html_escape(group.scope().id()),
        container_style,
        source_html
      ));

      index += 1;
    }

    // Render nested-level children, grouped by subscope
    for nested_group in group.nested_references() {
      let (nested_html, next_index) = render_source_children(
        nested_group.children(),
        group.scope(),
        group.is_in_scope(),
        index,
        total_references,
        0,
        Some(nested_group.subscope()),
        audit_data,
        source_text_cache,
      );
      html.push_str(&nested_html);
      index = next_index;
    }

    html.push_str("</div>"); // close reference-group
  }

  html
}

// ============================================================================
// Highlight CSS Rendering
// ============================================================================

/// Collect reference topic IDs from a list of SourceChild
fn collect_source_child_ids(children: &[SourceChild]) -> Vec<String> {
  let mut ids = Vec::new();
  for child in children {
    match child {
      SourceChild::Reference(reference) => {
        ids.push(reference.reference_topic().id().to_string());
      }
      SourceChild::AnnotatedBlock(block) => {
        ids.extend(collect_source_child_ids(block.children()));
      }
    }
  }
  ids
}

/// Flatten expanded_context into a list of topic IDs
fn flatten_expanded_context(expanded_context: &[SourceContext]) -> Vec<String> {
  let mut ids = Vec::new();
  for group in expanded_context {
    ids.push(group.scope().id().to_string());
    for ref_entry in group.scope_references() {
      ids.push(ref_entry.reference_topic().id().to_string());
    }
    for nested_group in group.nested_references() {
      ids.push(nested_group.subscope().id().to_string());
      ids.extend(collect_source_child_ids(nested_group.children()));
    }
  }
  ids
}

/// Generate CSS rules for active topic highlighting.
pub fn render_highlight_css(
  topic_id: &str,
  metadata: &TopicMetadata,
) -> String {
  let expanded_ref_panel = "#expanded-references-panel";

  // Active topic: solid underline everywhere
  let active_style = format!(
    "span[data-topic=\"{}\"] {{ text-decoration: underline; }}",
    topic_id
  );

  let (ancestor_ids, descendant_ids, relative_ids) = match metadata {
    TopicMetadata::NamedTopic {
      ancestors,
      descendants,
      expanded_context,
      ..
    } => {
      let ancestor_ids: Vec<String> =
        ancestors.iter().map(|t| t.id().to_string()).collect();
      let descendant_ids: Vec<String> =
        descendants.iter().map(|t| t.id().to_string()).collect();
      let relative_ids = flatten_expanded_context(expanded_context);
      (ancestor_ids, descendant_ids, relative_ids)
    }
    _ => (vec![], vec![], vec![]),
  };

  let mut css = String::new();

  // Relative styles
  for id in &relative_ids {
    css.push_str(&format!(
      "{} span[data-topic=\"{}\"] {{ text-decoration: underline; }}\n",
      expanded_ref_panel, id
    ));
  }

  // Ancestor styles
  for id in &ancestor_ids {
    css.push_str(&format!(
      "{} span[data-topic=\"{}\"] {{ text-decoration: underline; }}\n",
      expanded_ref_panel, id
    ));
  }

  // Descendant styles
  for id in &descendant_ids {
    css.push_str(&format!(
      "{} span[data-topic=\"{}\"] {{ text-decoration: underline; }}\n",
      expanded_ref_panel, id
    ));
  }

  // Active topic style comes last to take precedence
  css.push_str(&active_style);

  css
}

// ============================================================================
// Public API: Build Full Topic View Response
// ============================================================================

/// Resolve a comment topic to its first non-comment target topic.
/// Follows the `target_topic` chain recursively until a non-comment topic is found.
fn resolve_comment_target<'a>(
  metadata: &'a TopicMetadata,
  audit_data: &'a AuditData,
) -> &'a TopicMetadata {
  let mut current = metadata;
  while let Some(target) = current.target_topic() {
    match audit_data.topic_metadata.get(target) {
      Some(target_metadata) => current = target_metadata,
      None => break,
    }
  }
  current
}

/// Build the TopicViewResponse for a given topic (static panels only).
/// If `cached` is provided, the static panels are reused from the cache.
/// For comment topics, the view is rendered as if for the comment's
/// (first recursive non-comment) target topic.
pub fn build_topic_view(
  topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
  cached: Option<&core::CachedTopicView>,
) -> Option<TopicViewResponse> {
  let topic = topic::new_topic(topic_id);
  let metadata = audit_data.topic_metadata.get(&topic)?;

  // For comment topics, resolve to the target topic's metadata
  let view_metadata = resolve_comment_target(metadata, audit_data);

  let (
    topic_panel_html,
    expanded_references_panel_html,
    breadcrumb_html,
    highlight_css,
  ) = match cached {
    Some(c) => (
      c.topic_panel_html.clone(),
      c.expanded_references_panel_html.clone(),
      c.breadcrumb_html.clone(),
      c.highlight_css.clone(),
    ),
    None => {
      let topic_html = render_grouped_source_panel(
        view_metadata.context(),
        audit_data,
        source_text_cache,
        true,
      );
      let expanded_html = render_grouped_source_panel(
        view_metadata.expanded_context(),
        audit_data,
        source_text_cache,
        true,
      );
      let breadcrumb =
        render_history_breadcrumb(view_metadata, audit_data);
      let css =
        render_highlight_css(view_metadata.topic().id(), view_metadata);
      (topic_html, expanded_html, breadcrumb, css)
    }
  };

  Some(TopicViewResponse {
    topic_panel_html,
    expanded_references_panel_html,
    breadcrumb_html,
    highlight_css,
  })
}

/// Build the MentionsPanelResponse for a given topic.
/// Mentions are dynamic (depend on user-created comments) and are never cached.
pub fn build_mentions_panel(
  topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Option<MentionsPanelResponse> {
  let topic = topic::new_topic(topic_id);
  let metadata = audit_data.topic_metadata.get(&topic)?;

  let mentions_panel_html = render_grouped_source_panel(
    metadata.mentions(),
    audit_data,
    source_text_cache,
    true,
  );

  Some(MentionsPanelResponse {
    mentions_panel_html,
  })
}
