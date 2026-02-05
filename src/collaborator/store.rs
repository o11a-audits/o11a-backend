use std::collections::HashMap;

/// Represents an extracted mention from comment content
#[derive(Debug, Clone)]
pub struct ExtractedMention {
  pub topic_id: String,
  pub mention_text: String,
  pub start_offset: usize,
  pub end_offset: usize,
}

/// In-memory store for parsed comment data
pub struct CommentStore {
  /// Parsed HTML keyed by comment_id
  pub html_cache: HashMap<i64, String>,
  /// Extracted mentions keyed by comment_id
  pub mentions: HashMap<i64, Vec<ExtractedMention>>,
  /// Reverse index: topic_id -> comment_ids that mention it
  pub mentions_by_topic: HashMap<String, Vec<i64>>,
}

impl CommentStore {
  pub fn new() -> Self {
    Self {
      html_cache: HashMap::new(),
      mentions: HashMap::new(),
      mentions_by_topic: HashMap::new(),
    }
  }

  /// Add a parsed comment to the store
  pub fn insert(
    &mut self,
    comment_id: i64,
    html: String,
    mentions: Vec<ExtractedMention>,
  ) {
    // Update reverse index
    for mention in &mentions {
      self
        .mentions_by_topic
        .entry(mention.topic_id.clone())
        .or_default()
        .push(comment_id);
    }

    self.html_cache.insert(comment_id, html);
    self.mentions.insert(comment_id, mentions);
  }

  /// Remove a comment from the store
  pub fn remove(&mut self, comment_id: i64) {
    if let Some(mentions) = self.mentions.remove(&comment_id) {
      for mention in mentions {
        if let Some(ids) = self.mentions_by_topic.get_mut(&mention.topic_id) {
          ids.retain(|&id| id != comment_id);
        }
      }
    }
    self.html_cache.remove(&comment_id);
  }

  /// Get all comment IDs that mention a topic
  pub fn get_comments_mentioning(&self, topic_id: &str) -> Vec<i64> {
    self
      .mentions_by_topic
      .get(topic_id)
      .cloned()
      .unwrap_or_default()
  }

  /// Get the cached HTML for a comment
  pub fn get_html(&self, comment_id: i64) -> Option<&String> {
    self.html_cache.get(&comment_id)
  }

  /// Get the mentions for a comment
  pub fn get_mentions(
    &self,
    comment_id: i64,
  ) -> Option<&Vec<ExtractedMention>> {
    self.mentions.get(&comment_id)
  }
}

impl Default for CommentStore {
  fn default() -> Self {
    Self::new()
  }
}
