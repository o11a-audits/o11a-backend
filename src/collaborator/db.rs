use crate::collaborator::models::*;
use crate::collaborator::store::CommentStore;
use crate::collaborator::{formatter, parser};
use crate::core::DataContext;
use sqlx::SqlitePool;

// ============================================================================
// Migrations
// ============================================================================

/// Creates the comments and votes tables
pub async fn run_migrations(pool: &SqlitePool) -> Result<(), sqlx::Error> {
  // Comments table
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS comments (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            audit_id TEXT NOT NULL,
            topic_id TEXT NOT NULL,
            content_markdown TEXT NOT NULL,
            author_id INTEGER NOT NULL,
            comment_type TEXT NOT NULL DEFAULT 'note',
            created_at TEXT NOT NULL DEFAULT (datetime('now')),
            status TEXT NOT NULL DEFAULT 'active',
            scope TEXT NOT NULL DEFAULT '{}'
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query("CREATE INDEX IF NOT EXISTS idx_comments_audit_topic ON comments(audit_id, topic_id)")
        .execute(pool)
        .await?;
  sqlx::query("CREATE INDEX IF NOT EXISTS idx_comments_audit_status ON comments(audit_id, status)")
        .execute(pool)
        .await?;
  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_comments_author ON comments(author_id)",
  )
  .execute(pool)
  .await?;
  sqlx::query("CREATE INDEX IF NOT EXISTS idx_comments_type ON comments(audit_id, comment_type)")
        .execute(pool)
        .await?;

  // Votes table
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS comment_votes (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            comment_id INTEGER NOT NULL,
            user_id INTEGER NOT NULL,
            vote INTEGER NOT NULL,
            created_at TEXT NOT NULL DEFAULT (datetime('now')),
            UNIQUE(comment_id, user_id)
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_votes_comment ON comment_votes(comment_id)",
  )
  .execute(pool)
  .await?;
  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_votes_user ON comment_votes(user_id)",
  )
  .execute(pool)
  .await?;

  Ok(())
}

// ============================================================================
// Startup loading
// ============================================================================

/// Load and parse all comments on server startup.
/// Populates the CommentStore and registers each comment in
/// audit_data.topic_metadata (including mention wiring).
pub async fn load_and_parse_all_comments(
  pool: &SqlitePool,
  data_context: &mut DataContext,
) -> Result<CommentStore, sqlx::Error> {
  let mut store = CommentStore::new();

  // Fetch all non-hidden comments from database
  let comments = sqlx::query_as::<_, Comment>(
    "SELECT * FROM comments WHERE status != 'hidden'",
  )
  .fetch_all(pool)
  .await?;

  // Parse each comment with its audit's data
  for comment in comments {
    if let Some(audit_data) = data_context.get_audit_mut(&comment.audit_id) {
      let mentions =
        parser::parse_comment(&comment.content_markdown, audit_data);
      let html = formatter::render_comment_html(&comment.content_markdown);

      // Parse scope from stored JSON
      let scope: crate::api::ScopeInfo =
        serde_json::from_str(&comment.scope).unwrap_or_default();

      // Register in topic_metadata and wire up mentions
      super::store::register_comment_in_audit_data(
        audit_data, &comment, &scope, &mentions,
      );

      store.insert(comment.id, html, mentions);
    }
  }

  Ok(store)
}

// ============================================================================
// Comment CRUD operations
// ============================================================================

/// Creates a new comment
pub async fn create_comment(
  pool: &SqlitePool,
  audit_id: &str,
  request: &CreateCommentRequest,
  scope: &ScopeInfo,
) -> Result<Comment, sqlx::Error> {
  let comment_type = request.comment_type.as_str();
  let status = request.comment_type.default_status().as_str();
  let scope_json =
    serde_json::to_string(scope).unwrap_or_else(|_| "{}".to_string());

  let result = sqlx::query(
        r#"
        INSERT INTO comments (audit_id, topic_id, content_markdown, author_id, comment_type, status, scope)
        VALUES (?, ?, ?, ?, ?, ?, ?)
        "#,
    )
    .bind(audit_id)
    .bind(&request.topic_id)
    .bind(&request.content)
    .bind(request.author_id)
    .bind(comment_type)
    .bind(status)
    .bind(scope_json)
    .execute(pool)
    .await?;

  let comment_id = result.last_insert_rowid();
  get_comment_raw(pool, comment_id).await
}

/// Gets a single comment by ID (raw database row)
pub async fn get_comment_raw(
  pool: &SqlitePool,
  comment_id: i64,
) -> Result<Comment, sqlx::Error> {
  sqlx::query_as::<_, Comment>("SELECT * FROM comments WHERE id = ?")
    .bind(comment_id)
    .fetch_one(pool)
    .await
}

/// Gets comments for a specific topic (raw database rows)
pub async fn get_comments_for_topic_raw(
  pool: &SqlitePool,
  audit_id: &str,
  topic_id: &str,
) -> Result<Vec<Comment>, sqlx::Error> {
  sqlx::query_as::<_, Comment>(
        "SELECT * FROM comments WHERE audit_id = ? AND topic_id = ? AND status != 'hidden' ORDER BY created_at ASC",
    )
    .bind(audit_id)
    .bind(topic_id)
    .fetch_all(pool)
    .await
}

/// Gets all comments for an audit filtered by type
pub async fn get_comments_for_audit_raw(
  pool: &SqlitePool,
  audit_id: &str,
  comment_type: &str,
) -> Result<Vec<Comment>, sqlx::Error> {
  sqlx::query_as::<_, Comment>(
        "SELECT * FROM comments WHERE audit_id = ? AND comment_type = ? AND status != 'hidden' ORDER BY created_at DESC",
    )
    .bind(audit_id)
    .bind(comment_type)
    .fetch_all(pool)
    .await
}

/// Gets comments by IDs (for mention lookups)
pub async fn get_comments_by_ids(
  pool: &SqlitePool,
  comment_ids: &[i64],
) -> Result<Vec<Comment>, sqlx::Error> {
  if comment_ids.is_empty() {
    return Ok(vec![]);
  }

  let placeholders = comment_ids
    .iter()
    .map(|_| "?")
    .collect::<Vec<_>>()
    .join(",");
  let query = format!(
    "SELECT * FROM comments WHERE id IN ({}) AND status != 'hidden' ORDER BY created_at DESC",
    placeholders
  );

  let mut q = sqlx::query_as::<_, Comment>(&query);
  for id in comment_ids {
    q = q.bind(id);
  }
  q.fetch_all(pool).await
}

// ============================================================================
// Status operations
// ============================================================================

/// Updates comment status
pub async fn update_status(
  pool: &SqlitePool,
  comment_id: i64,
  status: &CommentStatus,
) -> Result<CommentStatusResponse, sqlx::Error> {
  let status_str = status.as_str();

  sqlx::query("UPDATE comments SET status = ? WHERE id = ?")
    .bind(status_str)
    .bind(comment_id)
    .execute(pool)
    .await?;

  get_comment_status(pool, comment_id).await
}

/// Gets status for a comment
pub async fn get_comment_status(
  pool: &SqlitePool,
  comment_id: i64,
) -> Result<CommentStatusResponse, sqlx::Error> {
  let comment = get_comment_raw(pool, comment_id).await?;

  Ok(CommentStatusResponse {
    comment_topic_id: comment.comment_topic_id(),
    status: comment.get_status(),
  })
}

/// Gets status for multiple comments
pub async fn get_comment_statuses(
  pool: &SqlitePool,
  comment_ids: &[i64],
) -> Result<Vec<CommentStatusResponse>, sqlx::Error> {
  let mut statuses = Vec::new();
  for &id in comment_ids {
    if let Ok(status) = get_comment_status(pool, id).await {
      statuses.push(status);
    }
  }
  Ok(statuses)
}

// ============================================================================
// Vote operations
// ============================================================================

/// Vote info result
#[derive(Debug, Default)]
pub struct VoteInfo {
  pub score: i64,
  pub upvotes: i64,
  pub downvotes: i64,
  pub user_vote: Option<VoteValue>,
}

/// Upsert a vote (insert or update existing)
pub async fn upsert_vote(
  pool: &SqlitePool,
  comment_id: i64,
  user_id: i64,
  vote: i32,
) -> Result<(), sqlx::Error> {
  sqlx::query(
    r#"
        INSERT INTO comment_votes (comment_id, user_id, vote)
        VALUES (?, ?, ?)
        ON CONFLICT(comment_id, user_id) DO UPDATE SET vote = excluded.vote
        "#,
  )
  .bind(comment_id)
  .bind(user_id)
  .bind(vote)
  .execute(pool)
  .await?;
  Ok(())
}

/// Delete a vote
pub async fn delete_vote(
  pool: &SqlitePool,
  comment_id: i64,
  user_id: i64,
) -> Result<(), sqlx::Error> {
  sqlx::query("DELETE FROM comment_votes WHERE comment_id = ? AND user_id = ?")
    .bind(comment_id)
    .bind(user_id)
    .execute(pool)
    .await?;
  Ok(())
}

/// Get vote information for a comment
pub async fn get_vote_info(
  pool: &SqlitePool,
  comment_id: i64,
  user_id: Option<i64>,
) -> Result<VoteInfo, sqlx::Error> {
  let score: i64 = sqlx::query_scalar(
    "SELECT COALESCE(SUM(vote), 0) FROM comment_votes WHERE comment_id = ?",
  )
  .bind(comment_id)
  .fetch_one(pool)
  .await?;

  let upvotes: i64 = sqlx::query_scalar(
    "SELECT COUNT(*) FROM comment_votes WHERE comment_id = ? AND vote = 1",
  )
  .bind(comment_id)
  .fetch_one(pool)
  .await?;

  let downvotes: i64 = sqlx::query_scalar(
    "SELECT COUNT(*) FROM comment_votes WHERE comment_id = ? AND vote = -1",
  )
  .bind(comment_id)
  .fetch_one(pool)
  .await?;

  let user_vote = if let Some(uid) = user_id {
    sqlx::query_scalar::<_, i32>(
      "SELECT vote FROM comment_votes WHERE comment_id = ? AND user_id = ?",
    )
    .bind(comment_id)
    .bind(uid)
    .fetch_optional(pool)
    .await?
    .map(VoteValue::from_i32)
  } else {
    None
  };

  Ok(VoteInfo {
    score,
    upvotes,
    downvotes,
    user_vote,
  })
}

/// Get comment IDs that a user has not voted on
pub async fn get_unvoted_comment_ids(
  pool: &SqlitePool,
  audit_id: &str,
  user_id: i64,
) -> Result<Vec<i64>, sqlx::Error> {
  sqlx::query_scalar::<_, i64>(
    r#"
        SELECT c.id FROM comments c
        WHERE c.audit_id = ?
          AND c.status != 'hidden'
          AND NOT EXISTS (
              SELECT 1 FROM comment_votes v
              WHERE v.comment_id = c.id AND v.user_id = ?
          )
        ORDER BY c.created_at DESC
        "#,
  )
  .bind(audit_id)
  .bind(user_id)
  .fetch_all(pool)
  .await
}
