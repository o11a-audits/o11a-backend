use crate::collaborator::store::ExtractedMention;
use crate::core::AuditData;
use crate::core::topic;
use regex::Regex;

/// Extracts mentions from markdown content.
/// Mentions can be:
/// - Direct topic IDs: N123, D45, C99
/// - Named references: functionName, ContractName
pub fn extract_mentions(
  content: &str,
  audit_data: &AuditData,
) -> Vec<ExtractedMention> {
  // Match identifiers that could be topic IDs or names
  // Topic IDs: N123, D45, C99 (letter followed by digits)
  // Names: valid identifiers (letter/underscore followed by alphanumeric/underscore)
  let mention_re = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_]*)\b").unwrap();
  let mut mentions = Vec::new();

  for cap in mention_re.captures_iter(content) {
    let full_match = cap.get(0).unwrap();
    let identifier = cap.get(1).unwrap().as_str();

    // Try to resolve: first as topic ID, then by name
    if let Some(topic_id) = resolve_mention(identifier, audit_data) {
      mentions.push(ExtractedMention {
        topic_id,
        mention_text: full_match.as_str().to_string(),
        start_offset: full_match.start(),
        end_offset: full_match.end(),
      });
    }
  }

  mentions
}

/// Resolves a mention identifier to a topic ID
fn resolve_mention(identifier: &str, audit_data: &AuditData) -> Option<String> {
  // First: check if it's a direct topic ID (N123, D45, C99)
  let topic = topic::new_topic(identifier);
  if audit_data.topic_metadata.contains_key(&topic) {
    return Some(identifier.to_string());
  }

  // Second: search by name in topic_metadata
  for (t, metadata) in &audit_data.topic_metadata {
    if metadata.name() == identifier {
      return Some(t.id.clone());
    }
  }

  None // Unresolved mention - keep as plain text
}

/// Parses comment markdown and extracts mentions.
/// Returns the extracted mentions (HTML rendering is done by formatter).
pub fn parse_comment(
  content: &str,
  audit_data: &AuditData,
) -> Vec<ExtractedMention> {
  extract_mentions(content, audit_data)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_mention_regex_matches_topic_ids() {
    let re = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_]*)\b").unwrap();

    // Should match topic ID patterns
    assert!(re.is_match("N123"));
    assert!(re.is_match("D45"));
    assert!(re.is_match("C99"));

    // Should match identifier patterns
    assert!(re.is_match("transferFrom"));
    assert!(re.is_match("_privateVar"));
    assert!(re.is_match("MyContract"));
  }
}
