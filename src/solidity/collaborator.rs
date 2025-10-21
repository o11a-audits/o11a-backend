/// Converts a node ID to a topic ID by prefixing it with "N"
pub fn node_id_to_topic_id(node_id: i32) -> String {
  format!("N{}", node_id)
}
