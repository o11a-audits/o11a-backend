use crate::api::ScopeInfo;
use crate::collaborator::models::Comment;
use crate::core::topic::Topic;
use crate::core::{self, insert_reference, topic::new_topic};
use std::collections::HashMap;

/// In-memory store for parsed comment data
pub struct CommentStore {
  /// Parsed HTML keyed by comment_id
  pub html_cache: HashMap<i64, String>,
  /// Extracted mentions keyed by comment_id
  pub mentions: HashMap<i64, Vec<Topic>>,
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
    mentions: Vec<Topic>,
  ) {
    // Update reverse index
    for mention in &mentions {
      self
        .mentions_by_topic
        .entry(mention.id.clone())
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
        if let Some(ids) = self.mentions_by_topic.get_mut(&mention.id) {
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
  pub fn get_mentions(&self, comment_id: i64) -> Option<&Vec<Topic>> {
    self.mentions.get(&comment_id)
  }
}

impl Default for CommentStore {
  fn default() -> Self {
    Self::new()
  }
}

/// Registers a comment in audit_data's topic_metadata and wires up mentions.
///
/// 1. Inserts a `TopicMetadata::CommentTopic` entry for this comment
/// 2. For each mention, inserts a `CommentMention` reference into the
///    mentioned topic's `mentions` field
pub fn register_comment_in_audit_data(
  audit_data: &mut core::AuditData,
  comment: &Comment,
  scope: &ScopeInfo,
  mentions: &[Topic],
) {
  let comment_topic_id = comment.comment_topic_id();
  let comment_topic = new_topic(&comment_topic_id);

  // Build the flat mentioned_topics list from mentions
  let mut mentioned_topics: Vec<Topic> = mentions.to_vec();
  mentioned_topics.sort_unstable();
  mentioned_topics.dedup();

  // Insert CommentTopic metadata
  audit_data.topic_metadata.insert(
    comment_topic.clone(),
    core::TopicMetadata::CommentTopic {
      topic: comment_topic.clone(),
      author_id: comment.author_id,
      comment_type: comment.comment_type.clone(),
      target_topic: new_topic(&comment.topic_id),
      created_at: comment.created_at.clone(),
      scope: scope.to_scope(),
      mentioned_topics,
      mentions: vec![],
    },
  );

  // Insert mention references into each mentioned topic's mentions field
  for mention in mentions {
    insert_comment_mention(
      audit_data,
      scope,
      &mention.id,
      comment_topic.clone(),
    );
  }
}

/// Inserts a CommentMention reference into the mentioned topic's
/// `TopicMetadata.mentions` ReferenceGroups.
///
/// Uses the comment's scope to determine the correct group (component) and
/// nested group (member), and the reference_topic (lowest scope topic).
fn insert_comment_mention(
  audit_data: &mut core::AuditData,
  scope: &ScopeInfo,
  mentioned_topic_id: &str,
  mention_topic: Topic,
) {
  // Determine reference_topic from the comment's scope (lowest scope level)
  let reference_topic_id = match scope.lowest_scope_topic_id() {
    Some(id) => id.to_string(),
    None => return, // Global/Container scope — can't place in a ReferenceGroup
  };

  let reference_topic = new_topic(&reference_topic_id);
  let ref_sort_key = audit_data
    .nodes
    .get(&reference_topic)
    .and_then(|n| n.source_location_start());

  // Determine component (group scope) — fall back to reference_topic if no component
  let component_id = match &scope.component {
    Some(id) => id.clone(),
    None => return, // No component means we can't group it
  };
  let component_topic = new_topic(&component_id);
  let component_sort_key = audit_data
    .nodes
    .get(&component_topic)
    .and_then(|n| n.source_location_start());

  // Determine subscope (nested group) from member, if present
  let subscope = scope.member.as_ref().map(|member_id| {
    let member_topic = new_topic(member_id);
    let member_sort_key = audit_data
      .nodes
      .get(&member_topic)
      .and_then(|n| n.source_location_start());
    (member_topic, member_sort_key)
  });

  let reference = core::Reference::comment_mention(
    reference_topic,
    mention_topic,
    ref_sort_key,
  );

  // Get the mentioned topic's metadata and insert into its mentions field
  let mentioned_topic = new_topic(mentioned_topic_id);
  let mentions = match audit_data.topic_metadata.get_mut(&mentioned_topic) {
    Some(core::TopicMetadata::NamedTopic { mentions, .. })
    | Some(core::TopicMetadata::UnnamedTopic { mentions, .. })
    | Some(core::TopicMetadata::TitledTopic { mentions, .. })
    | Some(core::TopicMetadata::CommentTopic { mentions, .. }) => mentions,
    None => return,
  };
  insert_reference(
    mentions,
    component_topic,
    component_sort_key,
    true,
    subscope,
    reference,
  );
}
