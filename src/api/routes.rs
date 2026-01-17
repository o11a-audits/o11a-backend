use axum::{
  Router,
  routing::{get, post},
};
use tower_http::cors::CorsLayer;

use crate::api::{AppState, handlers};

pub fn create_router(state: AppState) -> Router {
  Router::new()
    .route("/health", get(handlers::health_check))
    // Audit management
    .route("/api/v1/audits", get(handlers::list_audits))
    .route("/api/v1/audits", post(handlers::create_audit))
    .route(
      "/api/v1/audits/:audit_id",
      axum::routing::delete(handlers::delete_audit),
    )
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
      "/api/v1/audits/:audit_id/source_text/:topic_id",
      get(handlers::get_source_text),
    )
    .route(
      "/api/v1/audits/:audit_id/metadata/:topic_id",
      get(handlers::get_metadata),
    )
    .layer(CorsLayer::permissive())
    .with_state(state)
}
