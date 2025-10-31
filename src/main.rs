use o11a_backend::api::{AppState, routes};
use o11a_backend::core::{self, project};
use o11a_backend::db;
use std::path::Path;

#[tokio::main]
async fn main() {
  // Ensure data directory exists
  let data_dir = Path::new("data");
  if !data_dir.exists() {
    std::fs::create_dir_all(data_dir).expect("Failed to create data directory");
  }

  // Database setup
  let database_url = std::env::var("DATABASE_URL")
    .unwrap_or_else(|_| "sqlite://data/o11a.db".to_string());

  println!("Connecting to database: {}", database_url);

  let pool = db::create_pool(&database_url)
    .await
    .expect("Failed to create database pool");

  println!("Running migrations...");
  db::run_migrations(&pool)
    .await
    .expect("Failed to run migrations");

  println!("Creating DataContext...");

  // Create empty DataContext
  let data_context = core::new_data_context();

  println!("DataContext created successfully");

  // Create app state
  let state = AppState::new(pool, data_context);

  // Get project root and audit ID from environment variables
  let project_root =
    std::env::var("PROJECT_ROOT").unwrap_or_else(|_| ".".to_string());
  let project_root = Path::new(&project_root);

  let audit_id =
    std::env::var("AUDIT_ID").unwrap_or_else(|_| "default".to_string());

  println!(
    "Loading audit '{}' from project: {}",
    audit_id,
    project_root.display()
  );

  project::load_project(project_root, &audit_id, &state.data_context)
    .expect("Unable to load project");

  // Build router
  let app = routes::create_router(state);

  // Start server
  let addr = "0.0.0.0:3000";
  println!("Server running on http://{}", addr);

  let listener = tokio::net::TcpListener::bind(addr)
    .await
    .expect("Failed to bind to address");

  axum::serve(listener, app)
    .await
    .expect("Failed to start server");
}
