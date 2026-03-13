use std::collections::BTreeMap;

use serde::Deserialize;

use crate::collaborator::agent::context;
use crate::collaborator::agent::router::{self, TaskSize};
use crate::collaborator::models::AUTHOR_AGENT;
use crate::core::{self, AST, AuditData, Feature, Requirement, topic};

/// Raw requirement as returned by the LLM (no topic ID yet).
#[derive(Deserialize)]
struct LLMRequirement {
  description: String,
  documentation_topics: Vec<String>,
}

/// Raw feature as returned by the LLM (no topic ID yet).
#[derive(Deserialize)]
struct LLMFeature {
  name: String,
  description: String,
  requirements: Vec<LLMRequirement>,
}

/// Render all documentation ASTs into a single JSON string for the LLM prompt.
pub fn render_all_documentation(audit_data: &AuditData) -> String {
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
const BUILD_FEATURES_PROMPT: &str = "Below is the complete documentation for a smart contract project, \
rendered as structured JSON with topic IDs (D-prefixed, like \"D42\") \
on each section, paragraph, list, and code block.\n\n\
Your task is to identify all distinct smart contract **features** \
(or **goals**) of the project based on the documentation. A feature is \
a discrete capability or behavior of the system. Be specific with the \
features, avoiding overlap where one \"feature\" encompasses multiple \
distinct implementations (e.g., \"Access Control\" being a feature to \
represent both admin operations and permissioned user actions, or \
\"Fee Distribution\" being a feature to represent two distinct kinds of \
fee distribution within the project).\n\n\
For each feature, provide:\n\
- `name`: a short, descriptive name\n\
- `description`: a one-to-two sentence summary of the feature\n\
- `requirements`: an array of requirement objects, where each object has:\n\
  - `description`: a behavioral requirement that the implementation must \
satisfy. Include both **happy-path** requirements (what the system should \
do, e.g., \"Users can deposit tokens into the vault\") and \
**non-happy-path** requirements (what the system must prevent, \
e.g., \"Do not allow a user to withdraw another user's funds\"). \
Each requirement should be a single, specific, testable statement.\n\
  - `documentation_topics`: an array of D-prefixed topic ID strings \
(e.g., [\"D12\", \"D34\"]) for every documentation section, paragraph, \
list, or code block that informed this specific requirement. Each \
requirement must have at least one documentation topic.\n\n\
Rules:\n\
- Every documentation topic ID that describes system behavior, \
requirements, or constraints should appear in at least one requirement. \
Exclude boilerplate like tables of contents, version history, or \
author credits.\n\
- Choose the most specific section topic IDs to describe each requirement, \
preferring paragraphs over sections, but choosing sections where all \
contained paragraphs are relevant.\n\
- A documentation topic may appear in multiple requirements if it is \
relevant to more than one.\n\
- Do not invent topic IDs. Only use IDs present in the documentation.\n\
- When in doubt whether something is one feature or two, prefer \
splitting into more specific features.\n\
- Each feature should have at least one requirement.\n\
- Return ONLY a JSON array of feature objects, no other text.\n\n\
Documentation:\n";

/// Result of parsing LLM features: features and their requirements as separate maps.
pub struct ParsedFeatures {
  pub features: BTreeMap<topic::Topic, Feature>,
  pub requirements: BTreeMap<topic::Topic, Requirement>,
  pub topic_metadata: BTreeMap<topic::Topic, core::TopicMetadata>,
}

/// Parse the LLM response into features and requirements,
/// assigning sequential F-prefixed and R-prefixed topic IDs.
fn parse_features_response(response: &str) -> Result<ParsedFeatures, String> {
  // Strip markdown code fences if present
  let json_str = response
    .trim()
    .strip_prefix("```json")
    .or_else(|| response.trim().strip_prefix("```"))
    .unwrap_or(response.trim());
  let json_str = json_str.strip_suffix("```").unwrap_or(json_str).trim();

  let raw_features: Vec<LLMFeature> =
    serde_json::from_str(json_str).map_err(|e| {
      eprintln!(
        "Failed to parse features JSON: {}\nResponse:\n{}",
        e, json_str
      );
      format!("Failed to parse features JSON: {}", e)
    })?;

  let mut features = BTreeMap::new();
  let mut requirements = BTreeMap::new();
  let mut topic_metadata = BTreeMap::new();
  let mut req_counter = 0i32;

  for (i, raw) in raw_features.into_iter().enumerate() {
    let feature_topic = topic::new_feature_topic((i + 1) as i32);

    let mut requirement_topics = Vec::new();
    for raw_req in raw.requirements {
      req_counter += 1;
      let req_topic = topic::new_requirement_topic(req_counter);
      requirement_topics.push(req_topic.clone());
      let doc_topics: Vec<topic::Topic> = raw_req
        .documentation_topics
        .into_iter()
        .map(|id| topic::new_topic(&id))
        .collect();
      topic_metadata.insert(
        req_topic.clone(),
        core::TopicMetadata::RequirementTopic {
          topic: req_topic.clone(),
          description: raw_req.description,
          feature_topic: feature_topic.clone(),
          author_id: AUTHOR_AGENT,
          created_at: String::new(),
        },
      );
      requirements.insert(
        req_topic,
        Requirement {
          documentation_topics: doc_topics,
          source_topics: Vec::new(),
        },
      );
    }

    topic_metadata.insert(
      feature_topic.clone(),
      core::TopicMetadata::FeatureTopic {
        topic: feature_topic.clone(),
        name: raw.name,
        description: raw.description,
        author_id: AUTHOR_AGENT,
        created_at: String::new(),
      },
    );

    features.insert(feature_topic, Feature { requirement_topics });
  }

  Ok(ParsedFeatures {
    features,
    requirements,
    topic_metadata,
  })
}

/// Extract project features and requirements from pre-rendered documentation JSON via LLM.
///
/// This variant does not require holding a lock on `AuditData` — the caller
/// renders documentation while holding the lock, then passes the JSON string
/// to this function after releasing it.
pub async fn build_features_from_documentation(
  documentation_json: &str,
) -> Result<ParsedFeatures, String> {
  if documentation_json == "[]" {
    return Err("No documentation found in audit".to_string());
  }

  let prompt = format!("{}{}", BUILD_FEATURES_PROMPT, documentation_json);
  let response = router::chat_completion(
    TaskSize::Large,
    router::SYSTEM_MESSAGE_DOCUMENTATION,
    &prompt,
  )
  .await?;

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
