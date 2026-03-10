use crate::collaborator::models::*;
use crate::collaborator::{formatter, parser};
use crate::core::{self, topic, DataContext, Feature, Requirement};
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

  // Features table
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS features (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            audit_id TEXT NOT NULL,
            name TEXT NOT NULL,
            description TEXT NOT NULL,
            created_at TEXT NOT NULL DEFAULT (datetime('now'))
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_features_audit ON features(audit_id)",
  )
  .execute(pool)
  .await?;

  // Feature-topic associations
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS feature_topics (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            feature_id INTEGER NOT NULL,
            topic_id TEXT NOT NULL,
            relation TEXT NOT NULL,
            UNIQUE(feature_id, topic_id, relation)
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_feature_topics_feature ON feature_topics(feature_id)",
  )
  .execute(pool)
  .await?;

  // Requirements table
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS requirements (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            feature_id INTEGER NOT NULL,
            description TEXT NOT NULL,
            created_at TEXT NOT NULL DEFAULT (datetime('now'))
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_requirements_feature ON requirements(feature_id)",
  )
  .execute(pool)
  .await?;

  // Requirement source topic associations
  sqlx::query(
    r#"
        CREATE TABLE IF NOT EXISTS requirement_source_topics (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            requirement_id INTEGER NOT NULL,
            topic_id TEXT NOT NULL,
            UNIQUE(requirement_id, topic_id)
        )
        "#,
  )
  .execute(pool)
  .await?;

  sqlx::query(
    "CREATE INDEX IF NOT EXISTS idx_req_source_topics_req ON requirement_source_topics(requirement_id)",
  )
  .execute(pool)
  .await?;

  Ok(())
}

// ============================================================================
// Startup loading
// ============================================================================

/// Load and parse all comments on server startup.
/// Registers each comment in audit_data.topic_metadata (including mention
/// wiring) and caches rendered HTML in data_context.source_text_cache.
/// Returns the number of comments loaded.
pub async fn load_and_parse_all_comments(
  pool: &SqlitePool,
  data_context: &mut DataContext,
) -> Result<usize, sqlx::Error> {
  // Fetch all non-hidden comments from database
  let comments = sqlx::query_as::<_, Comment>(
    "SELECT * FROM comments WHERE status != 'hidden'",
  )
  .fetch_all(pool)
  .await?;

  let count = comments.len();

  // Parse each comment with its audit's data
  for comment in &comments {
    if let Some(audit_data) = data_context.get_audit_mut(&comment.audit_id) {
      let (mentions, nodes) =
        parser::parse_comment(&comment.content_markdown, audit_data);
      let comment_topic = comment.comment_topic();
      let html = formatter::render_comment_html(
        &nodes,
        &comment_topic,
        &audit_data.nodes,
      );

      // Store comment AST in nodes
      audit_data
        .nodes
        .insert(comment_topic.clone(), core::Node::Comment(nodes));

      // Parse scope from stored JSON
      let scope: crate::api::ScopeInfo =
        serde_json::from_str(&comment.scope).unwrap_or_default();

      // Register in topic_metadata and wire up mentions
      super::store::register_comment_in_audit_data(
        audit_data, &comment, &scope, &mentions,
      );

      // Cache rendered HTML
      data_context.cache_source_text(
        &comment.audit_id,
        &comment.comment_topic_id(),
        html,
      );
    }
  }

  Ok(count)
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

/// Gets all comments for an audit filtered by type and status
pub async fn get_comments_by_type_and_status(
  pool: &SqlitePool,
  audit_id: &str,
  comment_type: &str,
  status: &str,
) -> Result<Vec<Comment>, sqlx::Error> {
  sqlx::query_as::<_, Comment>(
        "SELECT * FROM comments WHERE audit_id = ? AND comment_type = ? AND status = ? ORDER BY created_at DESC",
    )
    .bind(audit_id)
    .bind(comment_type)
    .bind(status)
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

// ============================================================================
// Feature CRUD operations
// ============================================================================

/// Database row for a feature
#[derive(Debug, sqlx::FromRow)]
pub struct FeatureRow {
  pub id: i64,
  pub audit_id: String,
  pub name: String,
  pub description: String,
  pub created_at: String,
}

/// Database row for a feature-topic association
#[derive(Debug, sqlx::FromRow)]
pub struct FeatureTopicRow {
  pub id: i64,
  pub feature_id: i64,
  pub topic_id: String,
  pub relation: String,
}

/// Creates a new feature and returns the row
pub async fn create_feature(
  pool: &SqlitePool,
  audit_id: &str,
  name: &str,
  description: &str,
) -> Result<FeatureRow, sqlx::Error> {
  let result = sqlx::query(
    r#"
        INSERT INTO features (audit_id, name, description)
        VALUES (?, ?, ?)
        "#,
  )
  .bind(audit_id)
  .bind(name)
  .bind(description)
  .execute(pool)
  .await?;

  let id = result.last_insert_rowid();
  sqlx::query_as::<_, FeatureRow>("SELECT * FROM features WHERE id = ?")
    .bind(id)
    .fetch_one(pool)
    .await
}

/// Adds a topic association to a feature
pub async fn add_feature_topic(
  pool: &SqlitePool,
  feature_id: i64,
  topic_id: &str,
  relation: &str,
) -> Result<(), sqlx::Error> {
  sqlx::query(
    r#"
        INSERT OR IGNORE INTO feature_topics (feature_id, topic_id, relation)
        VALUES (?, ?, ?)
        "#,
  )
  .bind(feature_id)
  .bind(topic_id)
  .bind(relation)
  .execute(pool)
  .await?;
  Ok(())
}

/// Removes a topic association from a feature
pub async fn remove_feature_topic(
  pool: &SqlitePool,
  feature_id: i64,
  topic_id: &str,
  relation: &str,
) -> Result<(), sqlx::Error> {
  sqlx::query(
    "DELETE FROM feature_topics WHERE feature_id = ? AND topic_id = ? AND relation = ?",
  )
  .bind(feature_id)
  .bind(topic_id)
  .bind(relation)
  .execute(pool)
  .await?;
  Ok(())
}

/// Deletes all features and their associated data for an audit.
/// Cascades: requirement_source_topics → requirements → feature_topics → features.
pub async fn delete_all_features_for_audit(
  pool: &SqlitePool,
  audit_id: &str,
) -> Result<(), sqlx::Error> {
  // Delete requirement source topic associations
  sqlx::query(
    r#"
        DELETE FROM requirement_source_topics WHERE requirement_id IN (
            SELECT r.id FROM requirements r
            JOIN features f ON r.feature_id = f.id
            WHERE f.audit_id = ?
        )
        "#,
  )
  .bind(audit_id)
  .execute(pool)
  .await?;

  // Delete requirements
  sqlx::query(
    r#"
        DELETE FROM requirements WHERE feature_id IN (
            SELECT id FROM features WHERE audit_id = ?
        )
        "#,
  )
  .bind(audit_id)
  .execute(pool)
  .await?;

  // Delete feature topic associations
  sqlx::query(
    r#"
        DELETE FROM feature_topics WHERE feature_id IN (
            SELECT id FROM features WHERE audit_id = ?
        )
        "#,
  )
  .bind(audit_id)
  .execute(pool)
  .await?;

  // Delete features
  sqlx::query("DELETE FROM features WHERE audit_id = ?")
    .bind(audit_id)
    .execute(pool)
    .await?;

  Ok(())
}

/// Load all features and requirements from the database.
/// Returns the number of features loaded.
pub async fn load_all_features(
  pool: &SqlitePool,
  data_context: &mut DataContext,
) -> Result<usize, sqlx::Error> {
  let features = sqlx::query_as::<_, FeatureRow>("SELECT * FROM features")
    .fetch_all(pool)
    .await?;

  let feature_topics =
    sqlx::query_as::<_, FeatureTopicRow>("SELECT * FROM feature_topics")
      .fetch_all(pool)
      .await?;

  let requirements =
    sqlx::query_as::<_, RequirementRow>("SELECT * FROM requirements")
      .fetch_all(pool)
      .await?;

  let req_source_topics =
    sqlx::query_as::<_, RequirementSourceTopicRow>(
      "SELECT * FROM requirement_source_topics",
    )
    .fetch_all(pool)
    .await?;

  // Group feature topic associations by feature_id
  let mut topics_by_feature: std::collections::HashMap<i64, Vec<&FeatureTopicRow>> =
    std::collections::HashMap::new();
  for ft in &feature_topics {
    topics_by_feature.entry(ft.feature_id).or_default().push(ft);
  }

  // Group requirements by feature_id
  let mut reqs_by_feature: std::collections::HashMap<i64, Vec<&RequirementRow>> =
    std::collections::HashMap::new();
  for r in &requirements {
    reqs_by_feature.entry(r.feature_id).or_default().push(r);
  }

  // Group requirement source topics by requirement_id
  let mut src_by_req: std::collections::HashMap<i64, Vec<&RequirementSourceTopicRow>> =
    std::collections::HashMap::new();
  for rst in &req_source_topics {
    src_by_req.entry(rst.requirement_id).or_default().push(rst);
  }

  let count = features.len();

  for row in &features {
    if let Some(audit_data) = data_context.get_audit_mut(&row.audit_id) {
      let mut doc_topics = Vec::new();

      if let Some(assocs) = topics_by_feature.get(&row.id) {
        for ft in assocs {
          if ft.relation == "documentation" {
            doc_topics.push(topic::new_topic(&ft.topic_id));
          }
        }
      }

      let feature_topic = topic::new_feature_topic(row.id as i32);

      // Load requirements for this feature
      let mut requirement_topics = Vec::new();
      if let Some(reqs) = reqs_by_feature.get(&row.id) {
        for req in reqs {
          let req_topic = topic::new_requirement_topic(req.id as i32);
          requirement_topics.push(req_topic.clone());

          let mut source_topics = Vec::new();
          if let Some(srcs) = src_by_req.get(&req.id) {
            for s in srcs {
              source_topics.push(topic::new_topic(&s.topic_id));
            }
          }

          audit_data.requirements.insert(
            req_topic,
            Requirement {
              description: req.description.clone(),
              feature_topic: feature_topic.clone(),
              source_topics,
            },
          );
        }
      }

      audit_data.features.insert(
        feature_topic,
        Feature {
          name: row.name.clone(),
          description: row.description.clone(),
          documentation_topics: doc_topics,
          requirement_topics,
        },
      );
    }
  }

  Ok(count)
}

// ============================================================================
// Requirement CRUD operations
// ============================================================================

/// Database row for a requirement
#[derive(Debug, sqlx::FromRow)]
pub struct RequirementRow {
  pub id: i64,
  pub feature_id: i64,
  pub description: String,
  pub created_at: String,
}

/// Database row for a requirement source topic association
#[derive(Debug, sqlx::FromRow)]
pub struct RequirementSourceTopicRow {
  pub id: i64,
  pub requirement_id: i64,
  pub topic_id: String,
}

/// Creates a new requirement and returns the row
pub async fn create_requirement(
  pool: &SqlitePool,
  feature_id: i64,
  description: &str,
) -> Result<RequirementRow, sqlx::Error> {
  let result = sqlx::query(
    r#"
        INSERT INTO requirements (feature_id, description)
        VALUES (?, ?)
        "#,
  )
  .bind(feature_id)
  .bind(description)
  .execute(pool)
  .await?;

  let id = result.last_insert_rowid();
  sqlx::query_as::<_, RequirementRow>("SELECT * FROM requirements WHERE id = ?")
    .bind(id)
    .fetch_one(pool)
    .await
}

/// Deletes a requirement and its source topic associations
pub async fn delete_requirement(
  pool: &SqlitePool,
  requirement_id: i64,
) -> Result<(), sqlx::Error> {
  sqlx::query("DELETE FROM requirement_source_topics WHERE requirement_id = ?")
    .bind(requirement_id)
    .execute(pool)
    .await?;
  sqlx::query("DELETE FROM requirements WHERE id = ?")
    .bind(requirement_id)
    .execute(pool)
    .await?;
  Ok(())
}

/// Adds a source topic to a requirement
pub async fn add_requirement_source_topic(
  pool: &SqlitePool,
  requirement_id: i64,
  topic_id: &str,
) -> Result<(), sqlx::Error> {
  sqlx::query(
    r#"
        INSERT OR IGNORE INTO requirement_source_topics (requirement_id, topic_id)
        VALUES (?, ?)
        "#,
  )
  .bind(requirement_id)
  .bind(topic_id)
  .execute(pool)
  .await?;
  Ok(())
}

/// Removes a source topic from a requirement
pub async fn remove_requirement_source_topic(
  pool: &SqlitePool,
  requirement_id: i64,
  topic_id: &str,
) -> Result<(), sqlx::Error> {
  sqlx::query(
    "DELETE FROM requirement_source_topics WHERE requirement_id = ? AND topic_id = ?",
  )
  .bind(requirement_id)
  .bind(topic_id)
  .execute(pool)
  .await?;
  Ok(())
}
