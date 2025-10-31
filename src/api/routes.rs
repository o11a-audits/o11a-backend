use axum::{
  Router,
  routing::{get, post},
};

use crate::api::{AppState, handlers};

pub fn create_router(state: AppState) -> Router {
  Router::new()
    .route("/health", get(handlers::health_check))

    // Audit management
    .route("/api/audits", get(handlers::list_audits))
    .route("/api/audits", post(handlers::create_audit))
    .route("/api/audits/:audit_id", axum::routing::delete(handlers::delete_audit))

    // Audit-specific data
    .route("/api/audits/:audit_id/data-context", get(handlers::get_data_context))
    .route("/api/audits/:audit_id/boundaries", get(handlers::get_boundaries))

    // Chat endpoints (global for now)
    .route("/api/chats", get(handlers::get_chats))
    .route("/api/chats", post(handlers::create_chat))

    .with_state(state)
}
