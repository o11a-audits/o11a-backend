use axum::{
  Router,
  routing::{delete, get, post},
};
use tower_http::cors::CorsLayer;

use crate::api::{AppState, handlers};
use crate::collaborator::websocket;

pub fn create_router(state: AppState) -> Router {
  Router::new()
    .route("/health", get(handlers::health_check))
    // Audit management
    .route("/api/v1/audits", get(handlers::list_audits))
    .route("/api/v1/audits", post(handlers::create_audit))
    .route("/api/v1/audits/:audit_id", delete(handlers::delete_audit))
    // Audit-specific data
    .route(
      "/api/v1/audits/:audit_id/data-context",
      get(handlers::get_data_context),
    )
    .route(
      "/api/v1/audits/:audit_id/boundaries",
      get(handlers::get_boundaries),
    )
    .route(
      "/api/v1/audits/:audit_id/in_scope_files",
      get(handlers::get_in_scope_files),
    )
    // Chat endpoints (global for now)
    .route("/api/v1/chats", get(handlers::get_chats))
    .route("/api/v1/chats", post(handlers::create_chat))
    // Implemented
    .route(
      "/api/v1/audits/:audit_id/contracts",
      get(handlers::get_contracts),
    )
    .route(
      "/api/v1/audits/:audit_id/documents",
      get(handlers::get_documents),
    )
    .route(
      "/api/v1/audits/:audit_id/source_text/:topic_id",
      get(handlers::get_source_text),
    )
    .route(
      "/api/v1/audits/:audit_id/metadata/:topic_id",
      get(handlers::get_metadata),
    )
    // ============================================
    // Collaborator comment routes
    // ============================================
    .route(
      "/api/v1/audits/:audit_id/comments/:comment_type/:status",
      get(handlers::list_comments_by_type_and_status),
    )
    .route(
      "/api/v1/audits/:audit_id/comments",
      post(handlers::create_comment),
    )
    .route(
      "/api/v1/audits/:audit_id/comments/:comment_id/status",
      get(handlers::get_comment_status).put(handlers::update_comment_status),
    )
    .route(
      "/api/v1/audits/:audit_id/topics/:topic_id/comments",
      get(handlers::get_topic_comments),
    )
    .route(
      "/api/v1/audits/:audit_id/mentions/:topic_id",
      get(handlers::get_comments_mentioning_topic),
    )
    // WebSocket for real-time comment updates
    .route(
      "/api/v1/audits/:audit_id/comments/ws",
      get(websocket::comment_websocket),
    )
    // ============================================
    // Vote routes
    // ============================================
    .route(
      "/api/v1/audits/:audit_id/votes/unvoted",
      get(handlers::get_unvoted_comment_ids),
    )
    .route(
      "/api/v1/audits/:audit_id/votes/:comment_id",
      get(handlers::get_vote_summary)
        .post(handlers::cast_vote)
        .delete(handlers::remove_vote),
    )
    .layer(CorsLayer::permissive())
    .with_state(state)
}
