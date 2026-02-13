use crate::api::ScopeInfo;
use crate::collaborator::models::Comment;
use crate::core::topic::Topic;
use crate::core::{self, insert_into_context, topic::new_topic};

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
      context: vec![],
      mentions: vec![],
    },
  );

  // Insert mention references into each mentioned topic's mentions field
  for mention in mentions {
    insert_comment_mention(
      audit_data,
      scope,
      &comment.topic_id,
      &mention.id,
      comment_topic.clone(),
    );
  }
}

/// Inserts a CommentMention reference into the mentioned topic's
/// `TopicMetadata.mentions` SourceContexts.
///
/// Uses the comment's target topic as the reference_topic and
/// the comment's scope to determine the correct group (component) and
/// nested group (member).
fn insert_comment_mention(
  audit_data: &mut core::AuditData,
  scope: &ScopeInfo,
  target_topic_id: &str,
  mentioned_topic_id: &str,
  mention_topic: Topic,
) {
  // Use the comment's target topic as the reference_topic
  let reference_topic = new_topic(target_topic_id);
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
    | Some(core::TopicMetadata::ControlFlow { mentions, .. })
    | Some(core::TopicMetadata::CommentTopic { mentions, .. }) => mentions,
    None => return,
  };
  insert_into_context(
    mentions,
    component_topic,
    component_sort_key,
    true,
    subscope,
    &[],
    reference,
  );
}
