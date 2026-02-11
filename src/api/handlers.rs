use axum::{
  Json,
  extract::{Path, Query, State},
  http::StatusCode,
  response::{Html, IntoResponse},
};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

use crate::collaborator::{db, formatter, models::*, parser, store};
use crate::core::{self, Node, project, topic::new_topic};
use crate::{api::AppState, documentation::FormatContext};

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

#[derive(Debug, Serialize)]
pub struct DocumentsResponse {
  pub documents: Vec<TopicMetadataResponse>,
}

// Get all documents for an audit
pub async fn get_documents(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
) -> Result<Json<DocumentsResponse>, StatusCode> {
  println!("GET /api/v1/audits/{}/documents", audit_id);

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_documents: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;

  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  let mut documents = Vec::new();

  // Iterate through all topic metadata and filter for documentation roots
  for (topic, metadata) in &audit_data.topic_metadata {
    match metadata {
      crate::core::TopicMetadata::UnnamedTopic { kind, .. }
        if *kind == crate::core::UnnamedTopicKind::DocumentationRoot =>
      {
        // Documents don't have comment mentions (they're unnamed topics)
        documents.push(topic_metadata_to_response(topic, metadata));
      }
      _ => (),
    };
  }

  Ok(Json(DocumentsResponse { documents }))
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
      crate::core::TopicMetadata::UnnamedTopic { .. }
      | crate::core::TopicMetadata::TitledTopic { .. }
      | crate::core::TopicMetadata::CommentTopic { .. } => false,
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

      // Contracts list doesn't include comment mentions for performance
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

  // Check source text cache first
  if let Some(html) = ctx.get_cached_source_text(&audit_id, &topic_id) {
    return Ok(Html(html.clone()));
  }

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
      crate::documentation::formatter::node_to_html(
        doc_node,
        &audit_data.nodes,
        &FormatContext {
          comment_formatting: false,
          target_topic: topic.clone(),
        },
      )
    }
  };

  Ok(Html(source_text))
}

// Topic metadata response

/// Serializable scope information for storing in database and API responses
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScopeInfo {
  pub scope_type: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub container: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub component: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub member: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub containing_block: Option<String>,
}

impl ScopeInfo {
  /// Convert from core::Scope to ScopeInfo
  pub fn from_scope(scope: &core::Scope) -> Self {
    match scope {
      core::Scope::Global => ScopeInfo {
        scope_type: "Global".to_string(),
        container: None,
        component: None,
        member: None,
        containing_block: None,
      },
      core::Scope::Container { container } => ScopeInfo {
        scope_type: "Container".to_string(),
        container: Some(container.file_path.clone()),
        component: None,
        member: None,
        containing_block: None,
      },
      core::Scope::Component {
        container,
        component,
      } => ScopeInfo {
        scope_type: "Component".to_string(),
        container: Some(container.file_path.clone()),
        component: Some(component.id.clone()),
        member: None,
        containing_block: None,
      },
      core::Scope::Member {
        container,
        component,
        member,
      } => ScopeInfo {
        scope_type: "Member".to_string(),
        container: Some(container.file_path.clone()),
        component: Some(component.id.clone()),
        member: Some(member.id.clone()),
        containing_block: None,
      },
      core::Scope::ContainingBlock {
        container,
        component,
        member,
        containing_block,
      } => ScopeInfo {
        scope_type: "ContainingBlock".to_string(),
        container: Some(container.file_path.clone()),
        component: Some(component.id.clone()),
        member: Some(member.id.clone()),
        containing_block: Some(containing_block.id.clone()),
      },
    }
  }

  /// Get the scope from a topic's metadata, or return Global scope if not found
  pub fn from_topic(topic_id: &str, audit_data: &core::AuditData) -> Self {
    let topic = new_topic(topic_id);
    if let Some(metadata) = audit_data.topic_metadata.get(&topic) {
      Self::from_scope(metadata.scope())
    } else {
      Self::default()
    }
  }

  /// Returns the lowest (most specific) scope topic ID.
  /// Returns containing_block > member > component > None for Container/Global.
  pub fn lowest_scope_topic_id(&self) -> Option<&str> {
    self
      .containing_block
      .as_deref()
      .or(self.member.as_deref())
      .or(self.component.as_deref())
  }

  /// Convert from ScopeInfo back to core::Scope
  pub fn to_scope(&self) -> core::Scope {
    let container = || core::ProjectPath {
      file_path: self.container.clone().unwrap(),
    };
    match self.scope_type.as_str() {
      "ContainingBlock" => core::Scope::ContainingBlock {
        container: container(),
        component: new_topic(self.component.as_ref().unwrap()),
        member: new_topic(self.member.as_ref().unwrap()),
        containing_block: new_topic(self.containing_block.as_ref().unwrap()),
      },
      "Member" => core::Scope::Member {
        container: container(),
        component: new_topic(self.component.as_ref().unwrap()),
        member: new_topic(self.member.as_ref().unwrap()),
      },
      "Component" => core::Scope::Component {
        container: container(),
        component: new_topic(self.component.as_ref().unwrap()),
      },
      "Container" => core::Scope::Container {
        container: container(),
      },
      _ => core::Scope::Global,
    }
  }
}

impl Default for ScopeInfo {
  fn default() -> Self {
    ScopeInfo {
      scope_type: "Global".to_string(),
      container: None,
      component: None,
      member: None,
      containing_block: None,
    }
  }
}

/// Response type for a single reference
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
pub enum ReferenceResponse {
  #[serde(rename = "project")]
  Project { reference_topic: String },
  #[serde(rename = "project_with_mentions")]
  ProjectWithMentions {
    reference_topic: String,
    mention_topics: Vec<String>,
  },
  #[serde(rename = "comment")]
  Comment {
    reference_topic: String,
    mention_topics: Vec<String>,
  },
}

impl ReferenceResponse {
  fn from_reference(reference: &crate::core::Reference) -> Self {
    match reference {
      crate::core::Reference::ProjectReference {
        reference_topic, ..
      } => ReferenceResponse::Project {
        reference_topic: reference_topic.id().to_string(),
      },
      crate::core::Reference::ProjectReferenceWithMentions {
        reference_topic,
        mention_topics,
        ..
      } => ReferenceResponse::ProjectWithMentions {
        reference_topic: reference_topic.id().to_string(),
        mention_topics: mention_topics
          .iter()
          .map(|t| t.id().to_string())
          .collect(),
      },
      crate::core::Reference::CommentMention {
        reference_topic,
        mention_topics,
        ..
      } => ReferenceResponse::Comment {
        reference_topic: reference_topic.id().to_string(),
        mention_topics: mention_topics
          .iter()
          .map(|t| t.id().to_string())
          .collect(),
      },
    }
  }
}

#[derive(Debug, Clone, Serialize)]
pub struct NestedReferenceGroupResponse {
  pub subscope: String,
  pub references: Vec<ReferenceResponse>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ReferenceGroupResponse {
  pub scope: String,
  pub is_in_scope: bool,
  pub scope_references: Vec<ReferenceResponse>,
  pub nested_references: Vec<NestedReferenceGroupResponse>,
}

/// Response for NamedTopic metadata
#[derive(Debug, Serialize)]
pub struct NamedTopicResponse {
  pub topic_id: String,
  pub name: String,
  pub kind: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub sub_kind: Option<String>,
  pub visibility: String,
  pub scope: ScopeInfo,
  pub references: Vec<ReferenceGroupResponse>,
  pub expanded_references: Vec<ReferenceGroupResponse>,
  pub ancestry: Vec<ReferenceGroupResponse>,
  pub mentions: Vec<ReferenceGroupResponse>,
  pub ancestors: Vec<String>,
  pub descendants: Vec<String>,
  pub relatives: Vec<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub mutations: Option<Vec<String>>,
}

/// Response for TitledTopic metadata
#[derive(Debug, Serialize)]
pub struct TitledTopicResponse {
  pub topic_id: String,
  pub title: String,
  pub kind: String,
  pub scope: ScopeInfo,
}

/// Response for UnnamedTopic metadata
#[derive(Debug, Serialize)]
pub struct UnnamedTopicResponse {
  pub topic_id: String,
  pub kind: String,
  pub scope: ScopeInfo,
}

/// Response for CommentTopic metadata
#[derive(Debug, Serialize)]
pub struct CommentTopicResponse {
  pub topic_id: String,
  pub author_id: i64,
  pub comment_type: String,
  pub target_topic: String,
  pub created_at: String,
  pub scope: ScopeInfo,
  pub mentioned_topics: Vec<String>,
  pub mentions: Vec<ReferenceGroupResponse>,
}

/// Enum for different topic metadata response types
#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum TopicMetadataResponse {
  #[serde(rename = "named")]
  Named(NamedTopicResponse),
  #[serde(rename = "titled")]
  Titled(TitledTopicResponse),
  #[serde(rename = "unnamed")]
  Unnamed(UnnamedTopicResponse),
  #[serde(rename = "CommentTopic")]
  CommentTopic(CommentTopicResponse),
}

// Helper function to convert ReferenceGroup to ReferenceGroupResponse
fn convert_reference_groups(
  groups: &[crate::core::ReferenceGroup],
) -> Vec<ReferenceGroupResponse> {
  groups
    .iter()
    .map(|group| ReferenceGroupResponse {
      scope: group.scope().id().to_string(),
      is_in_scope: group.is_in_scope(),
      scope_references: group
        .scope_references()
        .iter()
        .map(ReferenceResponse::from_reference)
        .collect(),
      nested_references: group
        .nested_references()
        .iter()
        .map(|m| NestedReferenceGroupResponse {
          subscope: m.subscope().id().to_string(),
          references: m
            .references()
            .iter()
            .map(ReferenceResponse::from_reference)
            .collect(),
        })
        .collect(),
    })
    .collect()
}

// Helper function to convert TopicMetadata to TopicMetadataResponse
fn topic_metadata_to_response(
  topic: &crate::core::topic::Topic,
  metadata: &crate::core::TopicMetadata,
) -> TopicMetadataResponse {
  let scope_info = ScopeInfo::from_scope(metadata.scope());

  match metadata {
    crate::core::TopicMetadata::NamedTopic {
      name,
      kind,
      visibility,
      mutations,
      is_mutable,
      ..
    } => {
      // Format the kind and sub_kind for NamedTopic
      let (kind_str, sub_kind) = match kind {
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
      };

      let mutations_response = if *is_mutable {
        Some(mutations.iter().map(|t| t.id.clone()).collect())
      } else {
        None
      };

      TopicMetadataResponse::Named(NamedTopicResponse {
        topic_id: topic.id.clone(),
        name: name.clone(),
        kind: kind_str,
        sub_kind,
        visibility: format!("{:?}", visibility),
        scope: scope_info,
        references: convert_reference_groups(metadata.references()),
        expanded_references: convert_reference_groups(
          metadata.expanded_references(),
        ),
        ancestry: convert_reference_groups(metadata.ancestry()),
        ancestors: metadata.ancestors().iter().map(|t| t.id.clone()).collect(),
        descendants: metadata
          .descendants()
          .iter()
          .map(|t| t.id.clone())
          .collect(),
        relatives: metadata.relatives().iter().map(|t| t.id.clone()).collect(),
        mutations: mutations_response,
        mentions: convert_reference_groups(metadata.mentions()),
      })
    }

    crate::core::TopicMetadata::TitledTopic { title, kind, .. } => {
      TopicMetadataResponse::Titled(TitledTopicResponse {
        topic_id: topic.id.clone(),
        title: title.clone(),
        kind: format!("{:?}", kind),
        scope: scope_info,
      })
    }

    crate::core::TopicMetadata::UnnamedTopic { kind, .. } => {
      TopicMetadataResponse::Unnamed(UnnamedTopicResponse {
        topic_id: topic.id.clone(),
        kind: format!("{:?}", kind),
        scope: scope_info,
      })
    }

    crate::core::TopicMetadata::CommentTopic {
      author_id,
      comment_type,
      target_topic,
      created_at,
      mentioned_topics,
      ..
    } => TopicMetadataResponse::CommentTopic(CommentTopicResponse {
      topic_id: topic.id.clone(),
      author_id: *author_id,
      comment_type: comment_type.clone(),
      target_topic: target_topic.id.clone(),
      created_at: created_at.clone(),
      scope: scope_info,
      mentioned_topics: mentioned_topics.iter().map(|t| t.id.clone()).collect(),
      mentions: convert_reference_groups(metadata.mentions()),
    }),
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

// ============================================================================
// Collaborator query parameter types
// ============================================================================

#[derive(Debug, Deserialize)]
pub struct UserIdQuery {
  pub user_id: i64,
}

#[derive(Debug, Deserialize)]
pub struct OptionalUserIdQuery {
  pub user_id: Option<i64>,
}

// ============================================================================
// Comment handlers
// ============================================================================

/// GET /api/v1/audits/:audit_id/topics/:topic_id/comments
/// Returns comment topic IDs and types for comments on this topic.
pub async fn get_topic_comments(
  State(state): State<AppState>,
  Path((audit_id, topic_id)): Path<(String, String)>,
) -> Result<Json<TopicCommentsResponse>, StatusCode> {
  println!(
    "GET /api/v1/audits/{}/topics/{}/comments",
    audit_id, topic_id
  );
  let comments =
    db::get_comments_for_topic_raw(&state.db, &audit_id, &topic_id)
      .await
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let comments = comments
    .iter()
    .map(|c| TopicCommentEntry {
      comment_topic_id: c.comment_topic_id(),
      comment_type: c.comment_type.clone(),
    })
    .collect();

  Ok(Json(TopicCommentsResponse { comments }))
}

/// GET /api/v1/audits/:audit_id/comments/:comment_type/:status
/// Returns topic IDs of comments matching both the specified type and status.
pub async fn list_comments_by_type_and_status(
  State(state): State<AppState>,
  Path((audit_id, comment_type, status)): Path<(String, String, String)>,
) -> Result<Json<CommentListResponse>, StatusCode> {
  println!(
    "GET /api/v1/audits/{}/comments/{}/{}",
    audit_id, comment_type, status
  );

  // Validate comment_type
  if CommentType::from_str(&comment_type).is_none() {
    return Err(StatusCode::BAD_REQUEST);
  }

  // Validate status (CommentStatus::from_str has a catch-all fallback, so check explicitly)
  match status.as_str() {
    "active" | "hidden" | "resolved" | "unanswered" | "answered"
    | "unconfirmed" | "confirmed" | "rejected" => {}
    _ => return Err(StatusCode::BAD_REQUEST),
  }

  let comments = db::get_comments_by_type_and_status(
    &state.db,
    &audit_id,
    &comment_type,
    &status,
  )
  .await
  .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let comment_topic_ids =
    comments.iter().map(|c| c.comment_topic_id()).collect();

  Ok(Json(CommentListResponse { comment_topic_ids }))
}

/// POST /api/v1/audits/:audit_id/comments
/// Creates a new comment. Returns the new comment's topic ID.
pub async fn create_comment(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
  Json(payload): Json<CreateCommentRequest>,
) -> Result<Json<CommentCreatedResponse>, StatusCode> {
  println!("POST /api/v1/audits/{}/comments", audit_id);
  // Determine the scope from the target topic
  // If target is a comment (starts with "C"), copy scope from parent comment
  // Otherwise, get scope from the topic's metadata in audit data
  let scope = if payload.topic_id.starts_with('C') {
    // Target is a comment - get scope from parent comment
    let parent_comment_id: i64 = payload
      .topic_id
      .trim_start_matches('C')
      .parse()
      .map_err(|_| StatusCode::BAD_REQUEST)?;
    let parent_comment = db::get_comment_raw(&state.db, parent_comment_id)
      .await
      .map_err(|_| StatusCode::NOT_FOUND)?;
    // Parse the stored scope JSON
    serde_json::from_str(&parent_comment.scope).unwrap_or_default()
  } else {
    // Target is a regular topic - get scope from audit metadata
    let ctx = state
      .data_context
      .lock()
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;
    ScopeInfo::from_topic(&payload.topic_id, audit_data)
  };

  // Insert comment into database with scope
  let comment = db::create_comment(&state.db, &audit_id, &payload, &scope)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let comment_topic_id = comment.comment_topic_id();
  let comment_topic = comment.comment_topic();

  // Parse mentions, render HTML, register in audit_data, and cache source text
  let mut mentions_updates: Vec<(String, Vec<ReferenceGroupResponse>)> =
    Vec::new();
  {
    let mut ctx = state
      .data_context
      .lock()
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    let audit_data =
      ctx.get_audit_mut(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

    let (mentions, ast) = parser::parse_comment(&payload.content, audit_data);
    let html =
      formatter::render_comment_html(&ast, &comment_topic, &audit_data.nodes);

    store::register_comment_in_audit_data(
      audit_data, &comment, &scope, &mentions,
    );

    // Read back updated mentions for broadcasting
    if !mentions.is_empty() {
      let mut mentioned_ids: Vec<&str> =
        mentions.iter().map(|m| m.id.as_str()).collect();
      mentioned_ids.sort_unstable();
      mentioned_ids.dedup();

      for topic_id in mentioned_ids {
        let topic = new_topic(topic_id);
        if let Some(topic_meta) = audit_data.topic_metadata.get(&topic) {
          mentions_updates.push((
            topic_id.to_string(),
            convert_reference_groups(topic_meta.mentions()),
          ));
        }
      }
    }

    // Cache rendered HTML
    ctx.cache_source_text(&audit_id, &comment_topic_id, html);
  }

  // Broadcast via WebSocket
  let _ = state.comment_broadcast.send(CommentEvent::Created {
    audit_id: audit_id.clone(),
    comment_topic_id: comment_topic_id.clone(),
    target_topic: payload.topic_id.clone(),
    comment_type: payload.comment_type.clone(),
  });

  for (topic_id, updated_mentions) in mentions_updates {
    let _ = state.comment_broadcast.send(CommentEvent::MentionsUpdated {
      audit_id: audit_id.clone(),
      topic_id,
      mentions: updated_mentions,
    });
  }

  Ok(Json(CommentCreatedResponse { comment_topic_id }))
}

/// GET /api/v1/audits/:audit_id/mentions/:topic_id
/// Returns topic IDs of comments that mention the given topic.
pub async fn get_comments_mentioning_topic(
  State(state): State<AppState>,
  Path((audit_id, mentioned_topic_id)): Path<(String, String)>,
) -> Result<Json<CommentListResponse>, StatusCode> {
  println!(
    "GET /api/v1/audits/{}/mentions/{}",
    audit_id, mentioned_topic_id
  );

  let ctx = state.data_context.lock().map_err(|e| {
    eprintln!("Mutex poisoned in get_comments_mentioning_topic: {}", e);
    StatusCode::INTERNAL_SERVER_ERROR
  })?;
  let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

  let mentioned_topic = new_topic(&mentioned_topic_id);
  let topic_meta = audit_data
    .topic_metadata
    .get(&mentioned_topic)
    .ok_or(StatusCode::NOT_FOUND)?;

  // Collect comment topic IDs from the mentions reference groups
  let mut comment_topic_ids: Vec<String> = Vec::new();
  for group in topic_meta.mentions() {
    for reference in group.scope_references() {
      if let Some(mention_topics) = reference.mention_topics() {
        for t in mention_topics {
          comment_topic_ids.push(t.id.clone());
        }
      }
    }
    for nested in group.nested_references() {
      for reference in nested.references() {
        if let Some(mention_topics) = reference.mention_topics() {
          for t in mention_topics {
            comment_topic_ids.push(t.id.clone());
          }
        }
      }
    }
  }

  comment_topic_ids.sort_unstable();
  comment_topic_ids.dedup();

  Ok(Json(CommentListResponse { comment_topic_ids }))
}

// ============================================================================
// Status handlers
// ============================================================================

/// GET /api/v1/audits/:audit_id/comments/:comment_id/status
/// Returns status for a single comment.
pub async fn get_comment_status(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
) -> Result<Json<CommentStatusResponse>, StatusCode> {
  println!(
    "GET /api/v1/audits/{}/comments/{}/status",
    audit_id, comment_id
  );
  let response = db::get_comment_status(&state.db, comment_id)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(response))
}

/// PUT /api/v1/audits/:audit_id/comments/:comment_id/status
/// Updates comment status.
pub async fn update_comment_status(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
  Json(payload): Json<UpdateStatusRequest>,
) -> Result<Json<CommentStatusResponse>, StatusCode> {
  println!(
    "PUT /api/v1/audits/{}/comments/{}/status",
    audit_id, comment_id
  );
  // Update status in database
  let response = db::update_status(&state.db, comment_id, &payload.status)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Broadcast status update via WebSocket
  let _ = state.comment_broadcast.send(CommentEvent::StatusUpdated {
    audit_id: audit_id.clone(),
    comment_topic_id: response.comment_topic_id.clone(),
    status: response.status.clone(),
  });

  Ok(Json(response))
}

// ============================================================================
// Vote handlers
// ============================================================================

/// GET /api/v1/audits/:audit_id/votes/:comment_id
/// Returns vote summary for a comment.
pub async fn get_vote_summary(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
  Query(params): Query<OptionalUserIdQuery>,
) -> Result<Json<CommentVoteSummary>, StatusCode> {
  println!("GET /api/v1/audits/{}/votes/{}", audit_id, comment_id);
  let vote_info = db::get_vote_info(&state.db, comment_id, params.user_id)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(CommentVoteSummary {
    comment_id,
    comment_topic_id: format!("C{}", comment_id),
    score: vote_info.score,
    upvotes: vote_info.upvotes,
    downvotes: vote_info.downvotes,
    user_vote: vote_info.user_vote,
  }))
}

/// GET /api/v1/audits/:audit_id/votes/unvoted?user_id=N
/// Returns comment topic IDs the user has not voted on.
pub async fn get_unvoted_comment_ids(
  State(state): State<AppState>,
  Path(audit_id): Path<String>,
  Query(params): Query<UserIdQuery>,
) -> Result<Json<Vec<String>>, StatusCode> {
  println!(
    "GET /api/v1/audits/{}/votes/unvoted?user_id={}",
    audit_id, params.user_id
  );
  let comment_ids =
    db::get_unvoted_comment_ids(&state.db, &audit_id, params.user_id)
      .await
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Return as topic IDs (C1, C2, etc.)
  Ok(Json(
    comment_ids
      .into_iter()
      .map(|id| format!("C{}", id))
      .collect(),
  ))
}

/// POST /api/v1/audits/:audit_id/votes/:comment_id
/// Casts or updates a vote.
pub async fn cast_vote(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
  Json(payload): Json<VoteRequest>,
) -> Result<Json<CommentVoteSummary>, StatusCode> {
  println!("POST /api/v1/audits/{}/votes/{}", audit_id, comment_id);
  let vote_value = payload.vote.to_i32();

  db::upsert_vote(&state.db, comment_id, payload.user_id, vote_value)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Return updated vote summary
  let vote_info =
    db::get_vote_info(&state.db, comment_id, Some(payload.user_id))
      .await
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let comment_topic_id = format!("C{}", comment_id);

  // Broadcast vote update via WebSocket
  let _ = state.comment_broadcast.send(CommentEvent::VoteUpdated {
    audit_id,
    comment_topic_id: comment_topic_id.clone(),
    score: vote_info.score,
    upvotes: vote_info.upvotes,
    downvotes: vote_info.downvotes,
  });

  Ok(Json(CommentVoteSummary {
    comment_id,
    comment_topic_id,
    score: vote_info.score,
    upvotes: vote_info.upvotes,
    downvotes: vote_info.downvotes,
    user_vote: vote_info.user_vote,
  }))
}

/// DELETE /api/v1/audits/:audit_id/votes/:comment_id?user_id=N
/// Removes a user's vote.
pub async fn remove_vote(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
  Query(params): Query<UserIdQuery>,
) -> Result<StatusCode, StatusCode> {
  println!(
    "DELETE /api/v1/audits/{}/votes/{}?user_id={}",
    audit_id, comment_id, params.user_id
  );
  db::delete_vote(&state.db, comment_id, params.user_id)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Get updated vote info and broadcast
  let vote_info = db::get_vote_info(&state.db, comment_id, None)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let _ = state.comment_broadcast.send(CommentEvent::VoteUpdated {
    audit_id,
    comment_topic_id: format!("C{}", comment_id),
    score: vote_info.score,
    upvotes: vote_info.upvotes,
    downvotes: vote_info.downvotes,
  });

  Ok(StatusCode::NO_CONTENT)
}
