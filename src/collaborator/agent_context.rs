use serde::Serialize;
use serde_json::json;

use crate::collaborator::formatter as comment_formatter;
use crate::core::{
  self, AuditData, BlockAnnotationKind, ControlFlowStatementKind,
  NamedTopicKind, NamedTopicVisibility, Node, Reference, Scope, SourceChild,
  SourceContext, TitledTopicKind, TopicMetadata, UnnamedTopicKind, topic,
};

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
  pub condition: Option<serde_json::Value>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ast_snippet: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    comments: Option<Vec<AgentComment>>,
  },
  #[serde(rename = "annotated_block")]
  AnnotatedBlock {
    kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    condition: Option<serde_json::Value>,
    children: Vec<AgentSourceChild>,
  },
}

#[derive(Debug, Serialize)]
pub struct AgentComment {
  pub comment_type: String,
  pub content: String,
}

// ============================================================================
// Utility: HTML content retrieval
// ============================================================================

/// Get rendered HTML content for a comment or documentation topic.
fn get_html_content(
  topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> String {
  if let Some(html) = source_text_cache.get(topic.id()) {
    return html.clone();
  }

  match audit_data.nodes.get(topic) {
    Some(Node::Documentation(doc_node)) => {
      crate::documentation::formatter::node_to_html(
        doc_node,
        &audit_data.nodes,
      )
    }
    _ => topic.id().to_string(),
  }
}

// ============================================================================
// Utility: Topic name resolution
// ============================================================================

/// Resolve a topic to its display name.
fn resolve_topic_name(
  topic: &topic::Topic,
  audit_data: &AuditData,
) -> String {
  match audit_data.topic_metadata.get(topic) {
    Some(TopicMetadata::NamedTopic { name, .. }) => name.clone(),
    Some(TopicMetadata::TitledTopic { title, .. }) => title.clone(),
    Some(TopicMetadata::UnnamedTopic { kind, .. }) => {
      unnamed_kind_to_string(kind)
    }
    Some(TopicMetadata::ControlFlow { kind, .. }) => {
      control_flow_kind_to_string(kind).to_string()
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
) -> String {
  match scope {
    Scope::Global => "Global".to_string(),
    Scope::Container { container } => container.file_path.clone(),
    Scope::Component {
      container,
      component,
    } => {
      let component_name =
        resolve_topic_name(component, audit_data);
      format!("{} > {}", container.file_path, component_name)
    }
    Scope::Member {
      container,
      component,
      member,
      ..
    } => {
      let component_name =
        resolve_topic_name(component, audit_data);
      let member_name =
        resolve_topic_name(member, audit_data);
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
        resolve_topic_name(component, audit_data);
      let member_name =
        resolve_topic_name(member, audit_data);
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

/// Render the condition of a control flow annotation as an AST snippet.
fn render_condition_ast_snippet(
  annotation_topic: &topic::Topic,
  annotation_kind: &BlockAnnotationKind,
  target_topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Option<serde_json::Value> {
  match annotation_kind {
    BlockAnnotationKind::If(_)
    | BlockAnnotationKind::While
    | BlockAnnotationKind::DoWhile
    | BlockAnnotationKind::For => {}
    _ => return None,
  }

  let condition_topic = match audit_data.nodes.get(annotation_topic) {
    Some(Node::Solidity(ast_node)) => get_condition_topic(ast_node),
    _ => None,
  }?;

  match audit_data.nodes.get(&condition_topic) {
    Some(Node::Solidity(node)) => {
      let render_ctx = ASTRenderContext {
        target_topic: target_topic.clone(),
        omit_function_and_modifier_bodies: false,
      };
      Some(render_ast_snippet(
        node,
        &render_ctx,
        audit_data,
        source_text_cache,
      ))
    }
    _ => None,
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
// AST Snippet Rendering
// ============================================================================

/// Controls which parts of the AST tree are expanded vs. stubbed.
struct ASTRenderContext {
  /// The topic the agent requested context for.
  /// Used to decide whether to expand function bodies.
  target_topic: topic::Topic,
  /// When true, function/modifier bodies are omitted.
  /// Set to true when converting ContractDefinition members.
  omit_function_and_modifier_bodies: bool,
}

/// Render a type AST node to a plain-text string directly from its fields.
fn render_type_name(node: &ASTNode) -> String {
  match node {
    ASTNode::ElementaryTypeName { name, .. } => name.clone(),
    ASTNode::UserDefinedTypeName { path_node, .. } => {
      render_type_name(path_node)
    }
    ASTNode::IdentifierPath { name, .. } => name.clone(),
    ASTNode::Identifier { name, .. } => name.clone(),
    ASTNode::ArrayTypeName { base_type, .. } => {
      format!("{}[]", render_type_name(base_type))
    }
    ASTNode::Mapping {
      key_type,
      value_type,
      ..
    } => {
      format!(
        "mapping({} => {})",
        render_type_name(key_type),
        render_type_name(value_type)
      )
    }
    ASTNode::FunctionTypeName { .. } => "function".to_string(),
    _ => "unknown".to_string(),
  }
}

/// Look up comments targeting a node from the CommentIndex.
fn lookup_node_comments(
  node_id: i32,
  audit_data: &AuditData,
) -> Vec<serde_json::Value> {
  let node_topic = topic::new_node_topic(&node_id);
  let comment_topics = audit_data.comment_index.get(node_topic.id());
  comment_topics
    .iter()
    .filter_map(|comment_topic| {
      let content = match audit_data.nodes.get(comment_topic) {
        Some(Node::Comment(nodes)) => {
          comment_formatter::render_comment_plain_text(nodes)
        }
        _ => return None,
      };
      let content = content.trim().to_string();
      if content.is_empty() {
        return None;
      }
      let comment_type =
        match audit_data.topic_metadata.get(comment_topic) {
          Some(TopicMetadata::CommentTopic { comment_type, .. }) => {
            comment_type.clone()
          }
          _ => "note".to_string(),
        };
      Some(json!({
        "comment_type": comment_type,
        "content": content,
      }))
    })
    .collect()
}

/// Build a JSON object for a node, attaching comments if present.
fn make_node_json(
  mut obj: serde_json::Value,
  comments: Vec<serde_json::Value>,
) -> serde_json::Value {
  if !comments.is_empty() {
    obj["comments"] = json!(comments);
  }
  obj
}

/// Render an ASTNode as a structured AST snippet (JSON value).
fn render_ast_snippet(
  node: &ASTNode,
  render_ctx: &ASTRenderContext,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> serde_json::Value {
  let resolved = node.resolve(&audit_data.nodes);

  // Unresolved stub → TopicRef
  if let ASTNode::Stub {
    node_id, topic, ..
  } = resolved
  {
    let name =
      resolve_topic_name(topic, audit_data);
    let comments =
      lookup_node_comments(*node_id, audit_data);
    return make_node_json(
      json!({
        "type": "topic_ref",
        "id": topic.id(),
        "name": name,
      }),
      comments,
    );
  }

  let node_id = resolved.node_id();
  let id = topic::new_node_topic(&node_id).id().to_string();
  let comments =
    lookup_node_comments(node_id, audit_data);

  // Helper closure for recursive conversion
  let recurse = |child: &ASTNode| -> serde_json::Value {
    render_ast_snippet(child, render_ctx, audit_data, source_text_cache)
  };

  let obj = match resolved {
    // === Leaf nodes ===
    ASTNode::Identifier {
      name,
      referenced_declaration,
      ..
    } => json!({
      "type": "identifier",
      "id": id,
      "name": name,
      "referenced_declaration": topic::new_node_topic(referenced_declaration).id(),
    }),

    ASTNode::IdentifierPath {
      name,
      referenced_declaration,
      ..
    } => json!({
      "type": "identifier",
      "id": id,
      "name": name,
      "referenced_declaration": topic::new_node_topic(referenced_declaration).id(),
    }),

    ASTNode::Literal { kind, value, .. } => json!({
      "type": "literal",
      "id": id,
      "kind": kind.as_str(),
      "value": value,
    }),

    // === Type nodes ===
    ASTNode::ElementaryTypeName { .. }
    | ASTNode::UserDefinedTypeName { .. }
    | ASTNode::ArrayTypeName { .. }
    | ASTNode::Mapping { .. }
    | ASTNode::FunctionTypeName { .. } => json!({
      "type": "type_name",
      "id": id,
      "name": render_type_name(resolved),
    }),

    // === Expression nodes ===
    ASTNode::Assignment {
      operator,
      left_hand_side,
      right_hand_side,
      ..
    } => json!({
      "type": "assignment",
      "id": id,
      "operator": operator.as_str(),
      "left": recurse(left_hand_side),
      "right": recurse(right_hand_side),
    }),

    ASTNode::BinaryOperation {
      operator,
      left_expression,
      right_expression,
      ..
    } => json!({
      "type": "binary_operation",
      "id": id,
      "operator": operator.as_str(),
      "left": recurse(left_expression),
      "right": recurse(right_expression),
    }),

    ASTNode::UnaryOperation {
      operator,
      prefix,
      sub_expression,
      ..
    } => json!({
      "type": "unary_operation",
      "id": id,
      "operator": operator.as_str(),
      "prefix": prefix,
      "operand": recurse(sub_expression),
    }),

    ASTNode::FunctionCall {
      expression,
      arguments,
      ..
    } => json!({
      "type": "function_call",
      "id": id,
      "expression": recurse(expression),
      "arguments": arguments.iter().map(|a| recurse(a)).collect::<Vec<_>>(),
    }),

    ASTNode::TypeConversion {
      expression,
      argument,
      ..
    } => json!({
      "type": "function_call",
      "id": id,
      "expression": recurse(expression),
      "arguments": [recurse(argument)],
    }),

    ASTNode::StructConstructor {
      expression,
      arguments,
      ..
    } => json!({
      "type": "function_call",
      "id": id,
      "expression": recurse(expression),
      "arguments": arguments.iter().map(|a| recurse(a)).collect::<Vec<_>>(),
    }),

    ASTNode::MemberAccess {
      expression,
      member_name,
      referenced_declaration,
      ..
    } => {
      let mut obj = json!({
        "type": "member_access",
        "id": id,
        "expression": recurse(expression),
        "member": member_name,
      });
      if let Some(ref_decl) = referenced_declaration {
        obj["referenced_declaration"] =
          json!(topic::new_node_topic(ref_decl).id());
      }
      obj
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let mut obj = json!({
        "type": "index_access",
        "id": id,
        "base": recurse(base_expression),
      });
      if let Some(index) = index_expression {
        obj["index"] = recurse(index);
      }
      obj
    }

    ASTNode::Conditional {
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let mut obj = json!({
        "type": "conditional",
        "id": id,
        "condition": recurse(condition),
        "true_expression": recurse(true_expression),
      });
      if let Some(false_expr) = false_expression {
        obj["false_expression"] = recurse(false_expr);
      }
      obj
    }

    ASTNode::TupleExpression { components, .. } => json!({
      "type": "tuple",
      "id": id,
      "components": components.iter().map(|c| recurse(c)).collect::<Vec<_>>(),
    }),

    // === Statement nodes ===
    ASTNode::ExpressionStatement { expression, .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "expression",
      "expression": recurse(expression),
    }),

    ASTNode::Return { expression, .. } => {
      let mut obj = json!({
        "type": "statement",
        "id": id,
        "kind": "return",
      });
      if let Some(expr) = expression {
        obj["expression"] = recurse(expr);
      }
      obj
    }

    ASTNode::EmitStatement { event_call, .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "emit",
      "expression": recurse(event_call),
    }),

    ASTNode::RevertStatement { error_call, .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "revert",
      "expression": recurse(error_call),
    }),

    ASTNode::Break { .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "break",
    }),

    ASTNode::Continue { .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "continue",
    }),

    ASTNode::PlaceholderStatement { .. } => json!({
      "type": "statement",
      "id": id,
      "kind": "placeholder",
    }),

    // === Variable declarations ===
    ASTNode::VariableDeclarationStatement {
      declarations,
      initial_value,
      ..
    } => {
      let mut obj = json!({
        "type": "variable_declaration",
        "id": id,
        "declarations": declarations.iter().map(|d| recurse(d)).collect::<Vec<_>>(),
      });
      if let Some(val) = initial_value {
        obj["initial_value"] = recurse(val);
      }
      obj
    }

    ASTNode::VariableDeclaration {
      name, type_name, value, ..
    } => {
      let mut obj = json!({
        "type": "variable_declaration",
        "id": id,
        "name": name,
        "type_name": render_type_name(type_name),
      });
      if let Some(val) = value {
        obj["initial_value"] = recurse(val);
      }
      obj
    }

    // === Block nodes ===
    ASTNode::Block { statements, .. } => json!({
      "type": "block",
      "id": id,
      "statements": statements.iter().map(|s| recurse(s)).collect::<Vec<_>>(),
    }),

    ASTNode::SemanticBlock { statements, .. } => json!({
      "type": "block",
      "id": id,
      "kind": "semantic",
      "statements": statements.iter().map(|s| recurse(s)).collect::<Vec<_>>(),
    }),

    ASTNode::UncheckedBlock { statements, .. } => json!({
      "type": "block",
      "id": id,
      "kind": "unchecked",
      "statements": statements.iter().map(|s| recurse(s)).collect::<Vec<_>>(),
    }),

    // === Control flow ===
    ASTNode::IfStatement {
      condition,
      true_body,
      false_body,
      ..
    } => {
      let mut obj = json!({
        "type": "control_flow",
        "id": id,
        "kind": "if",
        "condition": recurse(condition),
        "body": recurse(true_body),
      });
      if let Some(fb) = false_body {
        obj["else_body"] = recurse(fb);
      }
      obj
    }

    ASTNode::ForStatement {
      condition, body, ..
    } => json!({
      "type": "control_flow",
      "id": id,
      "kind": "for",
      "condition": recurse(condition),
      "body": recurse(body),
    }),

    ASTNode::WhileStatement {
      condition, body, ..
    } => {
      let mut obj = json!({
        "type": "control_flow",
        "id": id,
        "kind": "while",
        "condition": recurse(condition),
      });
      if let Some(b) = body {
        obj["body"] = recurse(b);
      }
      obj
    }

    ASTNode::DoWhileStatement {
      condition, body, ..
    } => {
      let mut obj = json!({
        "type": "control_flow",
        "id": id,
        "kind": "do_while",
        "condition": recurse(condition),
      });
      if let Some(b) = body {
        obj["body"] = recurse(b);
      }
      obj
    }

    // === Definitions ===
    ASTNode::ContractDefinition {
      signature,
      nodes,
      ..
    } => {
      let (name, kind) = match signature.as_ref() {
        ASTNode::ContractSignature {
          name,
          contract_kind,
          ..
        } => (name.clone(), format!("{:?}", contract_kind).to_lowercase()),
        _ => ("unknown".to_string(), "contract".to_string()),
      };

      let member_ctx = ASTRenderContext {
        target_topic: render_ctx.target_topic.clone(),
        omit_function_and_modifier_bodies: true,
      };
      let members: Vec<serde_json::Value> = nodes
        .iter()
        .map(|n| {
          render_ast_snippet(
            n,
            &member_ctx,
            audit_data,
            source_text_cache,
          )
        })
        .collect();

      json!({
        "type": "contract_definition",
        "id": id,
        "name": name,
        "kind": kind,
        "signature": render_ast_snippet(signature, render_ctx, audit_data, source_text_cache),
        "members": members,
      })
    }

    ASTNode::FunctionDefinition {
      node_id,
      signature,
      body,
      ..
    } => {
      let (name, kind) = match signature.as_ref() {
        ASTNode::FunctionSignature { name, kind, .. } => {
          (name.clone(), format!("{:?}", kind).to_lowercase())
        }
        ASTNode::ModifierSignature { name, .. } => {
          (name.clone(), "modifier".to_string())
        }
        _ => ("unknown".to_string(), "function".to_string()),
      };

      let sig_json = render_ast_snippet(
        signature,
        render_ctx,
        audit_data,
        source_text_cache,
      );

      let is_target =
        render_ctx.target_topic == topic::new_node_topic(node_id);
      let body_json =
        if !render_ctx.omit_function_and_modifier_bodies || is_target {
          body.as_ref().map(|b| {
            render_ast_snippet(
              b,
              render_ctx,
              audit_data,
              source_text_cache,
            )
          })
        } else {
          None
        };

      let mut obj = json!({
        "type": "function_definition",
        "id": id,
        "name": name,
        "kind": kind,
        "signature": sig_json,
      });
      if let Some(body_val) = body_json {
        obj["body"] = body_val;
      }
      obj
    }

    ASTNode::ModifierDefinition {
      node_id,
      signature,
      body,
      ..
    } => {
      let name = match signature.as_ref() {
        ASTNode::ModifierSignature { name, .. } => name.clone(),
        _ => "unknown".to_string(),
      };

      let sig_json = render_ast_snippet(
        signature,
        render_ctx,
        audit_data,
        source_text_cache,
      );

      let is_target =
        render_ctx.target_topic == topic::new_node_topic(node_id);
      let body_json =
        if !render_ctx.omit_function_and_modifier_bodies || is_target {
          Some(render_ast_snippet(
            body,
            render_ctx,
            audit_data,
            source_text_cache,
          ))
        } else {
          None
        };

      let mut obj = json!({
        "type": "function_definition",
        "id": id,
        "name": name,
        "kind": "modifier",
        "signature": sig_json,
      });
      if let Some(body_val) = body_json {
        obj["body"] = body_val;
      }
      obj
    }

    // === Fallback: all other node types ===
    _ => {
      let children: Vec<serde_json::Value> =
        resolved.nodes().iter().map(|n| recurse(n)).collect();
      let mut obj = json!({
        "type": "other",
        "id": id,
        "node_type": resolved.type_name(),
      });
      if !children.is_empty() {
        obj["children"] = json!(children);
      }
      obj
    }
  };

  make_node_json(obj, comments)
}

// ============================================================================
// Source Context Conversion
// ============================================================================

/// Convert a list of SourceContext groups to AgentSourceGroup entries.
fn convert_source_groups(
  groups: &[SourceContext],
  target_topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Vec<AgentSourceGroup> {
  groups
    .iter()
    .map(|group| {
      convert_source_group(
        group,
        target_topic,
        audit_data,
        source_text_cache,
      )
    })
    .collect()
}

fn convert_source_group(
  group: &SourceContext,
  target_topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> AgentSourceGroup {
  let scope_name =
    resolve_topic_name(group.scope(), audit_data);

  let scope_references = group
    .scope_references()
    .iter()
    .map(|r| {
      convert_reference(r, target_topic, audit_data, source_text_cache)
    })
    .collect();

  let nested_references = group
    .nested_references()
    .iter()
    .map(|nested| {
      let subscope_name =
        resolve_topic_name(nested.subscope(), audit_data);
      let children = convert_source_children(
        nested.children(),
        target_topic,
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
  target_topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> Vec<AgentSourceChild> {
  children
    .iter()
    .map(|child| match child {
      SourceChild::Reference(reference) => {
        convert_reference(
          reference,
          target_topic,
          audit_data,
          source_text_cache,
        )
      }
      SourceChild::AnnotatedBlock(block) => {
        let annotation = block.annotation();
        let kind = annotation_kind_to_string(&annotation.kind).to_string();
        let condition = render_condition_ast_snippet(
          &annotation.topic,
          &annotation.kind,
          target_topic,
          audit_data,
          source_text_cache,
        );
        let children = convert_source_children(
          block.children(),
          target_topic,
          audit_data,
          source_text_cache,
        );
        AgentSourceChild::AnnotatedBlock {
          kind,
          condition,
          children,
        }
      }
    })
    .collect()
}

/// Convert a single Reference to an AgentSourceChild.
///
/// For Solidity nodes, produces an `ast_snippet` field with structured AST JSON.
/// For documentation nodes (and fallbacks), produces a `code` field with HTML.
///
/// Mention comments are attached as structured `AgentComment` entries.
fn convert_reference(
  reference: &Reference,
  target_topic: &topic::Topic,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) -> AgentSourceChild {
  let ref_topic = reference.reference_topic();

  // Determine ast_snippet vs code based on node type
  let (code, ast_snippet) = match audit_data.nodes.get(ref_topic) {
    Some(Node::Solidity(solidity_node)) => {
      let render_ctx = ASTRenderContext {
        target_topic: target_topic.clone(),
        omit_function_and_modifier_bodies: false,
      };
      let ast_json = render_ast_snippet(
        solidity_node,
        &render_ctx,
        audit_data,
        source_text_cache,
      );
      (None, Some(ast_json))
    }
    _ => {
      let code =
        get_html_content(ref_topic, audit_data, source_text_cache);
      (Some(code), None)
    }
  };

  // Resolve mention comments
  let mut comments = Vec::new();

  if let Some(mention_topics) = reference.mention_topics() {
    for mention_topic in mention_topics {
      let content = match audit_data.nodes.get(mention_topic) {
        Some(Node::Comment(nodes)) => {
          comment_formatter::render_comment_plain_text(nodes)
        }
        _ => continue,
      };
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

      comments.push(AgentComment {
        comment_type,
        content,
      });
    }
  }

  let comments = if comments.is_empty() {
    None
  } else {
    Some(comments)
  };

  AgentSourceChild::Reference {
    code,
    ast_snippet,
    comments,
  }
}

// ============================================================================
// Public API: Build Agent Context
// ============================================================================

/// Build the agent context for a given topic.
///
/// Returns a resolved JSON-serializable structure where all topic IDs
/// are replaced with human-readable values. Solidity topics are rendered
/// as structured AST snippets; documentation and comments preserve their
/// HTML representation.
pub fn build_agent_context(
  topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
  include_expanded_context: bool,
) -> Option<AgentTopicContext> {
  let topic = topic::new_topic(topic_id);
  let metadata = audit_data.topic_metadata.get(&topic)?;

  let name = resolve_topic_name(&topic, audit_data);
  let scope = scope_to_breadcrumb(metadata.scope(), audit_data);

  let context = convert_source_groups(
    metadata.context(),
    &topic,
    audit_data,
    source_text_cache,
  );
  let mentions = convert_source_groups(
    metadata.mentions(),
    &topic,
    audit_data,
    source_text_cache,
  );

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
            .map(|t| resolve_topic_name(t, audit_data))
            .collect(),
        )
      };

      let resolved_descendants = if descendants.is_empty() {
        None
      } else {
        Some(
          descendants
            .iter()
            .map(|t| resolve_topic_name(t, audit_data))
            .collect(),
        )
      };

      let resolved_relatives = if relatives.is_empty() {
        None
      } else {
        Some(
          relatives
            .iter()
            .map(|t| resolve_topic_name(t, audit_data))
            .collect(),
        )
      };

      let expanded = if include_expanded_context {
        Some(convert_source_groups(
          expanded_context,
          &topic,
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
        condition: None,
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
      condition: None,
      ancestors: None,
      descendants: None,
      relatives: None,
      context,
      expanded_context: None,
      mentions,
    }),

    TopicMetadata::ControlFlow {
      kind, condition, ..
    } => {
      let condition_snippet = match audit_data.nodes.get(condition) {
        Some(Node::Solidity(node)) => {
          let render_ctx = ASTRenderContext {
            target_topic: topic.clone(),
            omit_function_and_modifier_bodies: false,
          };
          Some(render_ast_snippet(
            node,
            &render_ctx,
            audit_data,
            source_text_cache,
          ))
        }
        _ => None,
      };

      Some(AgentTopicContext {
        name,
        kind: control_flow_kind_to_string(kind).to_string(),
        sub_kind: None,
        visibility: None,
        scope,
        is_mutable: None,
        condition: condition_snippet,
        ancestors: None,
        descendants: None,
        relatives: None,
        context,
        expanded_context: None,
        mentions,
      })
    }

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
        condition: None,
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
        resolve_topic_name(target_topic, audit_data);

      Some(AgentTopicContext {
        name,
        kind: "Comment".to_string(),
        sub_kind: Some(comment_type.clone()),
        visibility: None,
        scope: format!("{} (on: {})", scope, target_name),
        is_mutable: None,
        condition: None,
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
