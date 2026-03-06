use crate::core::{AuditData, topic};

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
