use axum::{
  Json,
  extract::{Path, State},
  http::StatusCode,
  response::{Html, IntoResponse},
};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

use crate::core::{Node, project};
use crate::{api::AppState, core::topic::new_topic};

// Health check handler
pub async fn health_check() -> StatusCode {
  println!("GET /health");
  StatusCode::OK
}

// DataContext response (placeholder structure - will be populated from analyzer)
#[derive(Debug, Serialize)]
pub struct DataContextResponse {
  pub in_scope_files: Vec<String>,
  pub nodes: serde_json::Value,
  pub declarations: serde_json::Value,
}

// Get DataContext for a specific audit
pub async fn get_data_context(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<DataContextResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/data-context", audit_id);
  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_data_context: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  // Convert in_scope_files to Vec<String>
  let in_scope_files: Vec<String> = audit_data
    .in_scope_files
    .iter()
    .map(|p| p.file_path.clone())
    .collect();

  Ok(Json(DataContextResponse {
    in_scope_files,
    nodes: serde_json::json!({}),
    declarations: serde_json::json!({}),
  }))
}

// Chat model
#[derive(Debug, Serialize, Deserialize, FromRow)]
pub struct Chat {
  pub id: i64,
  pub content: String,
  pub created_at: String,
}

#[derive(Debug, Deserialize)]
pub struct CreateChatRequest {
  pub content: String,
}

// Get all chats
pub async fn get_chats(
  State(state): State<AppState>,
) -> Result<Json<Vec<Chat>>, StatusCode> {
  println!("GET /api/v1/chats");
  let chats = sqlx::query_as::<_, Chat>(
    "SELECT id, content, created_at FROM chats ORDER BY created_at DESC",
  )
  .fetch_all(&state.db)
  .await
  .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(chats))
}

// Create a new chat
pub async fn create_chat(
  State(state): State<AppState>,
  Json(payload): Json<CreateChatRequest>,
) -> Result<Json<Chat>, StatusCode> {
  println!("POST /api/v1/chats");
  let result = sqlx::query("INSERT INTO chats (content) VALUES (?)")
    .bind(&payload.content)
    .execute(&state.db)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let chat = sqlx::query_as::<_, Chat>(
    "SELECT id, content, created_at FROM chats WHERE id = ?",
  )
  .bind(result.last_insert_rowid())
  .fetch_one(&state.db)
  .await
  .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(chat))
}

// Boundaries response (placeholder for future implementation)
#[derive(Debug, Serialize)]
pub struct BoundariesResponse {
  pub boundaries: Vec<String>,
}

// Get boundaries for a specific audit
pub async fn get_boundaries(
  State(_state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<BoundariesResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/boundaries", audit_id);
  // TODO: Implement actual boundaries from checker
  Ok(Json(BoundariesResponse { boundaries: vec![] }))
}

#[derive(Debug, Serialize)]
pub struct InScopeFilesResponse {
  pub in_scope_files: Vec<String>,
}

// Get in scope files for a specific audit
pub async fn get_in_scope_files(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<InScopeFilesResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/in_scope_files", audit_id);

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_in_scope_files: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  let in_scope_files: Vec<String> = audit_data
    .in_scope_files
    .iter()
    .map(|p| p.file_path.clone())
    .collect();

  Ok(Json(InScopeFilesResponse { in_scope_files }))
}

// Audit management handlers

#[derive(Debug, Serialize)]
pub struct AuditInfo {
  pub audit_id: String,
}

#[derive(Debug, Serialize)]
pub struct AuditsListResponse {
  pub audits: Vec<AuditInfo>,
}

// List all audits
pub async fn list_audits(
  State(state): State<AppState>,
) -> Result<Json<AuditsListResponse>, StatusCode> {
  println!("GET /api/v1/audits");
  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in list_audits: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;
  let audits = ctx
    .list_audits()
    .into_iter()
    .map(|audit_id| AuditInfo { audit_id })
    .collect();

  Ok(Json(AuditsListResponse { audits }))
}

#[derive(Debug, Deserialize)]
pub struct CreateAuditRequest {
  pub audit_id: String,
  pub project_root: String,
}

#[derive(Debug, Serialize)]
pub struct CreateAuditResponse {
  pub audit_id: String,
  pub message: String,
}

// Create a new audit
pub async fn create_audit(
  State(state): State<AppState>,
  Json(payload): Json<CreateAuditRequest>,
) -> Result<Json<CreateAuditResponse>, StatusCode> {
  println!("POST /api/v1/audits");
  let project_root = std::path::Path::new(&payload.project_root);

  // Load the project for this audit
  project::load_project(project_root, &payload.audit_id, &state.data_context)
    .map_err(|e| {
    eprintln!(
      "Failed to load project for audit '{}': {}",
      payload.audit_id, e
    );
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  Ok(Json(CreateAuditResponse {
    audit_id: payload.audit_id.clone(),
    message: format!("Audit '{}' created successfully", payload.audit_id),
  }))
}

#[derive(Debug, Serialize)]
pub struct DeleteAuditResponse {
  pub message: String,
}

// Delete an audit
pub async fn delete_audit(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<DeleteAuditResponse>, StatusCode> {
  println!("DELETE /api/v1/audits/{}", audit_id);
  let mut ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in delete_audit: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  if ctx.delete_audit(&audit_id) {
    Ok(Json(DeleteAuditResponse {
      message: format!("Audit '{}' deleted successfully", audit_id),
    }))
  } else {
    Err(StatusCode::NOT_FOUND)
  }
}

#[derive(Debug, Serialize)]
pub struct ContractsResponse {
  pub contracts: Vec<TopicMetadataResponse>,
}

// Get all contracts for an audit
pub async fn get_contracts(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<ContractsResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/contracts", audit_id);

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_contracts: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  let mut contracts = Vec::new();

  // Iterate through all topic metadata and filter for contracts in scope files
  for (topic, metadata) in &audit_data.topic_metadata {
    let is_contract = match metadata {
      crate::core::TopicMetadata::NamedTopic { kind, .. } => {
        matches!(kind, crate::core::NamedTopicKind::Contract(_))
      }
      crate::core::TopicMetadata::UnnamedTopic { .. } => false,
      crate::core::TopicMetadata::NamedMutableTopic { .. } => false,
    };

    if is_contract {
      // Check if the contract is in an in-scope file
      let is_in_scope = match metadata.scope() {
        crate::core::Scope::Container { container } => {
          audit_data.in_scope_files.contains(container)
        }
        _ => false,
      };

      if !is_in_scope {
        continue;
      }

      contracts.push(topic_metadata_to_response(topic, metadata));
    }
  }

  Ok(Json(ContractsResponse { contracts }))
}

// Get source text for a specific topic within an audit
pub async fn get_source_text(
  State(state): State<AppState>,
  Path((audit_id, topic_id)): Path<(String, String)>,
) -> Result<impl IntoResponse, StatusCode> {
  println!("GET /api/v1/audits/{}/source_text/{}", audit_id, topic_id);

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_source_text: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  // Create topic from the topic_id
  let topic = new_topic(&topic_id);

  match crate::solidity::formatter::global_to_source_text(&topic) {
    Some(global) => {
      return Ok(Html(global));
    }
    None => (),
  }

  // Get the node for this topic
  let node = audit_data.nodes.get(&topic).ok_or_else(|| {
    eprintln!("Topic '{}' not found in audit '{}'", topic_id, audit_id);
    StatusCode::NOT_FOUND
  })?;

  // Convert the node to source text based on its type
  let source_text = match node {
    Node::Solidity(solidity_node) => {
      crate::solidity::formatter::node_to_source_text(
        solidity_node,
        &audit_data.nodes,
        &audit_data.topic_metadata,
      )
    }
    Node::Documentation(doc_node) => {
      crate::documentation::formatter::node_to_html(doc_node)
    }
  };

  Ok(Html(source_text))
}

// Topic metadata response
#[derive(Debug, Serialize)]
pub struct ScopeInfo {
  pub scope_type: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub container: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub component: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub member: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub semantic_block: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct TopicMetadataResponse {
  pub topic_id: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name: Option<String>,
  pub kind: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub sub_kind: Option<String>,
  pub scope: ScopeInfo,
  pub references: Vec<String>,
}

// Helper function to convert TopicMetadata to TopicMetadataResponse
fn topic_metadata_to_response(
  topic: &crate::core::topic::Topic,
  metadata: &crate::core::TopicMetadata,
) -> TopicMetadataResponse {
  // Extract scope information
  let scope_info = match metadata.scope() {
    crate::core::Scope::Global => ScopeInfo {
      scope_type: "Global".to_string(),
      container: None,
      component: None,
      member: None,
      semantic_block: None,
    },
    crate::core::Scope::Container { container } => ScopeInfo {
      scope_type: "Container".to_string(),
      container: Some(container.file_path.clone()),
      component: None,
      member: None,
      semantic_block: None,
    },
    crate::core::Scope::Component {
      container,
      component,
    } => ScopeInfo {
      scope_type: "Component".to_string(),
      container: Some(container.file_path.clone()),
      component: Some(component.id.clone()),
      member: None,
      semantic_block: None,
    },
    crate::core::Scope::Member {
      container,
      component,
      member,
    } => ScopeInfo {
      scope_type: "Member".to_string(),
      container: Some(container.file_path.clone()),
      component: Some(component.id.clone()),
      member: Some(member.id.clone()),
      semantic_block: None,
    },
    crate::core::Scope::SemanticBlock {
      container,
      component,
      member,
      semantic_block,
    } => ScopeInfo {
      scope_type: "SemanticBlock".to_string(),
      container: Some(container.file_path.clone()),
      component: Some(component.id.clone()),
      member: Some(member.id.clone()),
      semantic_block: Some(semantic_block.id.clone()),
    },
  };

  // Format the kind and sub_kind
  let (kind_str, sub_kind) = match metadata {
    crate::core::TopicMetadata::NamedTopic { kind, .. } => match kind {
      crate::core::NamedTopicKind::Contract(contract_kind) => {
        ("Contract".to_string(), Some(format!("{:?}", contract_kind)))
      }
      crate::core::NamedTopicKind::Function(function_kind) => {
        ("Function".to_string(), Some(format!("{:?}", function_kind)))
      }
      crate::core::NamedTopicKind::StateVariable(mutability) => (
        "StateVariable".to_string(),
        Some(format!("{:?}", mutability)),
      ),
      kind => (format!("{:?}", kind), None),
    },
    crate::core::TopicMetadata::NamedMutableTopic { kind, .. } => {
      (format!("{:?}", kind), None)
    }
    crate::core::TopicMetadata::UnnamedTopic { kind, .. } => {
      (format!("{:?}", kind), None)
    }
  };

  // Only include name for NamedTopic and NamedMutableTopic
  let name = match metadata {
    crate::core::TopicMetadata::NamedTopic { name, .. }
    | crate::core::TopicMetadata::NamedMutableTopic { name, .. } => {
      Some(name.clone())
    }
    crate::core::TopicMetadata::UnnamedTopic { .. } => None,
  };

  // Extract references (only NamedTopics have references)
  let references: Vec<String> =
    metadata.references().iter().map(|t| t.id.clone()).collect();

  TopicMetadataResponse {
    topic_id: topic.id.clone(),
    name,
    kind: kind_str,
    sub_kind,
    scope: scope_info,
    references,
  }
}

// Get metadata for a specific topic within an audit
pub async fn get_metadata(
  State(state): State<AppState>,
  Path((audit_id, topic_id)): Path<(String, String)>,
) -> Result<Json<TopicMetadataResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/metadata/{}", audit_id, topic_id);

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_metadata: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  // Create topic from the topic_id
  let topic = new_topic(&topic_id);

  // Get the metadata for this topic
  let metadata = audit_data.topic_metadata.get(&topic).ok_or_else(|| {
    eprintln!(
      "Metadata for topic '{}' not found in audit '{}'",
      topic_id, audit_id
    );
    StatusCode::NOT_FOUND
  })?;

  Ok(Json(topic_metadata_to_response(&topic, metadata)))
}
