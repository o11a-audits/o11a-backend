use crate::collaborator::models::Comment;
use crate::core::topic::Topic;
use crate::core::{self, topic::new_topic};

/// Registers a comment in audit_data's topic_metadata, comment_index, and mentions_index.
///
/// 1. Inserts a `TopicMetadata::CommentTopic` entry for this comment
/// 2. Adds the comment to the comment_index for its target topic
/// 3. Adds the comment to the mentions_index for each mentioned topic
pub fn register_comment_in_audit_data(
  audit_data: &mut core::AuditData,
  comment: &Comment,
  scope: &crate::api::ScopeInfo,
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
    },
  );

  // Update comment index
  let target_topic = new_topic(&comment.topic_id);
  let comments = audit_data
    .comment_index
    .entry(target_topic)
    .or_default();
  if !comments.contains(&comment_topic) {
    comments.push(comment_topic.clone());
  }

  // Update mentions index
  for mention in mentions {
    let entries = audit_data
      .mentions_index
      .entry(mention.clone())
      .or_default();
    if !entries.contains(&comment_topic) {
      entries.push(comment_topic.clone());
    }
  }
}
