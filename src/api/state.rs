use crate::core::DataContext;
use sqlx::SqlitePool;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct AppState {
  pub db: SqlitePool,
  pub data_context: Arc<Mutex<DataContext>>,
}

impl AppState {
  pub fn new(db: SqlitePool, data_context: DataContext) -> Self {
    Self {
      db,
      data_context: Arc::new(Mutex::new(data_context)),
    }
  }
}
