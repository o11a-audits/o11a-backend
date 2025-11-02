use axum::{
  Router,
  routing::{get, post},
};

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
      "/api/v1/audits/:audit_id/contracts",
      get(handlers::get_contracts),
    )
    // Chat endpoints (global for now)
    .route("/api/v1/chats", get(handlers::get_chats))
    .route("/api/v1/chats", post(handlers::create_chat))
    .with_state(state)
}
