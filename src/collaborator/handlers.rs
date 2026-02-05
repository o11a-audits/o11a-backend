use axum::{
  Json,
  extract::{Path, Query, State},
  http::StatusCode,
};
use serde::Deserialize;

use crate::api::AppState;
use crate::collaborator::{db, formatter, models::*, parser};

// ============================================================================
// Query parameter types
// ============================================================================

#[derive(Debug, Deserialize)]
pub struct UserIdQuery {
  pub user_id: i64,
}

#[derive(Debug, Deserialize)]
pub struct OptionalUserIdQuery {
  pub user_id: Option<i64>,
}

#[derive(Debug, Deserialize)]
pub struct BatchStatusQuery {
  pub ids: String, // Comma-separated: "C1,C2,C3"
}

// ============================================================================
// Comment handlers
// ============================================================================

/// GET /api/v1/audits/:audit_id/topics/:topic_id/comments
/// Returns topic IDs of comments on this topic.
pub async fn get_topic_comments(
  State(state): State<AppState>,
  Path((audit_id, topic_id)): Path<(String, String)>,
) -> Result<Json<CommentListResponse>, StatusCode> {
  let comments =
    db::get_comments_for_topic_raw(&state.db, &audit_id, &topic_id)
      .await
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  let comment_topic_ids =
    comments.iter().map(|c| c.comment_topic_id()).collect();

  Ok(Json(CommentListResponse { comment_topic_ids }))
}

/// GET /api/v1/audits/:audit_id/comments/:comment_type
/// Returns topic IDs of comments of the specified type.
pub async fn list_comments_by_type(
  State(state): State<AppState>,
  Path((audit_id, comment_type)): Path<(String, String)>,
) -> Result<Json<CommentListResponse>, StatusCode> {
  let comments =
    db::get_comments_for_audit_raw(&state.db, &audit_id, &comment_type)
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
  // Insert comment into database
  let comment = db::create_comment(&state.db, &audit_id, &payload)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Parse and add to in-memory store
  let (html, mentions) = {
    let ctx = state
      .data_context
      .lock()
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    let audit_data = ctx.get_audit(&audit_id).ok_or(StatusCode::NOT_FOUND)?;

    let mentions = parser::parse_comment(&payload.content, audit_data);
    let html = formatter::render_comment_html(&payload.content, &mentions);
    (html, mentions)
  };

  // Update in-memory store
  {
    let mut store = state
      .comment_store
      .write()
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    store.insert(comment.id, html, mentions);
  }

  let comment_topic_id = comment.comment_topic_id();

  // Broadcast via WebSocket
  let _ = state.comment_broadcast.send(CommentEvent::Created {
    audit_id: audit_id.clone(),
    comment_topic_id: comment_topic_id.clone(),
  });

  Ok(Json(CommentCreatedResponse { comment_topic_id }))
}

/// GET /api/v1/audits/:audit_id/mentions/:topic_id
/// Returns topic IDs of comments that mention the given topic.
pub async fn get_comments_mentioning_topic(
  State(state): State<AppState>,
  Path((audit_id, mentioned_topic_id)): Path<(String, String)>,
) -> Result<Json<CommentListResponse>, StatusCode> {
  // Look up comment IDs from in-memory store
  let comment_ids = {
    let store = state
      .comment_store
      .read()
      .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    store.get_comments_mentioning(&mentioned_topic_id)
  };

  // Fetch raw comments from database to filter by audit_id
  let comments = db::get_comments_by_ids(&state.db, &comment_ids)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Filter by audit_id and collect topic IDs
  let comment_topic_ids = comments
    .iter()
    .filter(|c| c.audit_id == audit_id)
    .map(|c| c.comment_topic_id())
    .collect();

  Ok(Json(CommentListResponse { comment_topic_ids }))
}

// ============================================================================
// Status handlers
// ============================================================================

/// GET /api/v1/audits/:audit_id/comments/:comment_id/status
/// Returns status for a single comment.
pub async fn get_status(
  State(state): State<AppState>,
  Path((_audit_id, comment_id)): Path<(String, i64)>,
) -> Result<Json<CommentStatusResponse>, StatusCode> {
  let response = db::get_comment_status(&state.db, comment_id)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(response))
}

/// GET /api/v1/audits/:audit_id/comments/status?ids=C1,C2,C3
/// Returns status for multiple comments.
pub async fn get_batch_status(
  State(state): State<AppState>,
  Path(_audit_id): Path<String>,
  Query(params): Query<BatchStatusQuery>,
) -> Result<Json<Vec<CommentStatusResponse>>, StatusCode> {
  let comment_ids: Vec<i64> = params
    .ids
    .split(',')
    .filter_map(|s| s.trim().trim_start_matches('C').parse().ok())
    .collect();

  let statuses = db::get_comment_statuses(&state.db, &comment_ids)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(Json(statuses))
}

/// PUT /api/v1/audits/:audit_id/comments/:comment_id/status
/// Updates comment status.
pub async fn update_status(
  State(state): State<AppState>,
  Path((audit_id, comment_id)): Path<(String, i64)>,
  Json(payload): Json<UpdateStatusRequest>,
) -> Result<Json<CommentStatusResponse>, StatusCode> {
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
  Path((_audit_id, comment_id)): Path<(String, i64)>,
  Query(params): Query<OptionalUserIdQuery>,
) -> Result<Json<CommentVoteSummary>, StatusCode> {
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
  Path((_audit_id, comment_id)): Path<(String, i64)>,
  Json(payload): Json<VoteRequest>,
) -> Result<Json<CommentVoteSummary>, StatusCode> {
  let vote_value = payload.vote.to_i32();

  db::upsert_vote(&state.db, comment_id, payload.user_id, vote_value)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  // Return updated vote summary
  let vote_info =
    db::get_vote_info(&state.db, comment_id, Some(payload.user_id))
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

/// DELETE /api/v1/audits/:audit_id/votes/:comment_id?user_id=N
/// Removes a user's vote.
pub async fn remove_vote(
  State(state): State<AppState>,
  Path((_audit_id, comment_id)): Path<(String, i64)>,
  Query(params): Query<UserIdQuery>,
) -> Result<StatusCode, StatusCode> {
  db::delete_vote(&state.db, comment_id, params.user_id)
    .await
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

  Ok(StatusCode::NO_CONTENT)
}
