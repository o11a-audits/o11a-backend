use std::collections::BTreeMap;

use serde::Deserialize;

use crate::collaborator::agent::context;
use crate::collaborator::agent::router::{self, TaskSize};
use crate::core::{AuditData, Feature, AST, topic};

/// Raw feature as returned by the LLM (no topic ID yet).
#[derive(Deserialize)]
struct LLMFeature {
  name: String,
  description: String,
  documentation_topics: Vec<String>,
}

/// Render all documentation ASTs into a single JSON string for the LLM prompt.
fn render_all_documentation(audit_data: &AuditData) -> String {
  let mut doc_sections = Vec::new();

  for (path, ast) in &audit_data.asts {
    let doc_ast = match ast {
      AST::Documentation(doc_ast) => doc_ast,
      _ => continue,
    };

    let rendered: Vec<serde_json::Value> = doc_ast
      .nodes
      .iter()
      .map(|node| context::render_documentation_ast_snippet(node, audit_data))
      .collect();

    doc_sections.push(serde_json::json!({
      "file": path.file_path,
      "content": rendered,
    }));
  }

  serde_json::to_string(&doc_sections).unwrap_or_default()
}

/// Build the prompt for feature extraction.
fn build_prompt(documentation_json: &str) -> String {
  format!(
    "Below is the complete documentation for a smart contract project, \
     rendered as structured JSON with topic IDs (D-prefixed, like \"D42\") \
     on each section, paragraph, list, and code block.\n\n\
     Your task is to identify all distinct **features** of the project \
     based on the documentation. A feature is a discrete capability or \
     behavior of the system (e.g., \"Token Minting\", \"Access Control\", \
     \"Fee Distribution\").\n\n\
     For each feature, provide:\n\
     - `name`: a short, descriptive name\n\
     - `description`: a one-to-two sentence summary of the feature\n\
     - `documentation_topics`: an array of D-prefixed topic ID strings \
     (e.g., [\"D12\", \"D34\"]) for every documentation section, paragraph, \
     list, or code block that informs or describes this feature\n\n\
     Rules:\n\
     - Every documentation topic ID that contains substantive content \
     should appear in at least one feature.\n\
     - Choose the most specific section topic IDs to describe each feature, as \
     the documentation has nested sections.\n\
     - A documentation topic may appear in multiple features if it is \
     relevant to more than one.\n\
     - Do not invent topic IDs. Only use IDs present in the documentation.\n\
     - Return ONLY a JSON array of feature objects, no other text.\n\n\
     Documentation:\n{documentation_json}"
  )
}

/// Parse the LLM response into features, assigning sequential F-prefixed topic IDs.
fn parse_features_response(
  response: &str,
) -> Result<BTreeMap<topic::Topic, Feature>, String> {
  // Strip markdown code fences if present
  let json_str = response
    .trim()
    .strip_prefix("```json")
    .or_else(|| response.trim().strip_prefix("```"))
    .unwrap_or(response.trim());
  let json_str = json_str
    .strip_suffix("```")
    .unwrap_or(json_str)
    .trim();

  let raw_features: Vec<LLMFeature> = serde_json::from_str(json_str)
    .map_err(|e| format!("Failed to parse features JSON: {}", e))?;

  let mut features = BTreeMap::new();

  for (i, raw) in raw_features.into_iter().enumerate() {
    let feature_topic = topic::new_feature_topic((i + 1) as i32);
    let doc_topics = raw
      .documentation_topics
      .into_iter()
      .map(|id| topic::new_topic(&id))
      .collect();

    features.insert(
      feature_topic,
      Feature {
        name: raw.name,
        description: raw.description,
        documentation_topics: doc_topics,
        source_topics: Vec::new(),
      },
    );
  }

  Ok(features)
}

/// Read all documentation and extract project features via LLM.
///
/// Returns a map of F-prefixed feature topics to Feature structs,
/// with `source_topics` left empty for later population by `document_contracts`.
pub async fn build_features(
  audit_data: &AuditData,
) -> Result<BTreeMap<topic::Topic, Feature>, String> {
  let documentation_json = render_all_documentation(audit_data);

  if documentation_json == "[]" {
    return Err("No documentation found in audit".to_string());
  }

  let prompt = build_prompt(&documentation_json);
  let response = router::chat_completion(TaskSize::Large, &prompt).await?;

  parse_features_response(&response)
}

/// Generate documentation for all top-level contracts in the audit.
pub fn document_contracts(
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) {
  todo!()
}

/// Generate documentation for all members of a specific contract.
pub fn document_contract_members(
  contract_topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) {
  todo!()
}

/// Generate documentation for a specific topic.
pub fn document_topic(
  topic_id: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) {
  todo!()
}

/// Answer a user question in the context of a specific topic.
pub fn answer_question(
  topic_id: &str,
  question: &str,
  audit_data: &AuditData,
  source_text_cache: &std::collections::HashMap<String, String>,
) {
  todo!()
}
