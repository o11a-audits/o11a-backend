use crate::collaborator::{CommentEvent, CommentStore};
use crate::core::DataContext;
use sqlx::SqlitePool;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::broadcast;

#[derive(Clone)]
pub struct AppState {
  pub db: SqlitePool,
  pub data_context: Arc<Mutex<DataContext>>,
  pub comment_broadcast: broadcast::Sender<CommentEvent>,
  pub comment_store: Arc<RwLock<CommentStore>>,
}

impl AppState {
  pub fn new(
    db: SqlitePool,
    data_context: DataContext,
    comment_store: CommentStore,
  ) -> Self {
    let (tx, _) = broadcast::channel(100); // Buffer 100 events
    Self {
      db,
      data_context: Arc::new(Mutex::new(data_context)),
      comment_broadcast: tx,
      comment_store: Arc::new(RwLock::new(comment_store)),
    }
  }
}
