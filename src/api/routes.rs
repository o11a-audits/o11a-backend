use axum::{
  Router,
  routing::{get, post},
};

use crate::api::{AppState, handlers};

pub fn create_router(state: AppState) -> Router {
  Router::new()
    .route("/health", get(handlers::health_check))
    .route("/api/data-context", get(handlers::get_data_context))
    .route("/api/chats", get(handlers::get_chats))
    .route("/api/chats", post(handlers::create_chat))
    .route("/api/boundaries", get(handlers::get_boundaries))
    .with_state(state)
}
