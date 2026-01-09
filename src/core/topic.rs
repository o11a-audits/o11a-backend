use serde::Serialize;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Serialize)]
pub struct Topic {
  pub id: String,
}

impl Topic {
  pub fn id(&self) -> &str {
    &self.id
  }

  pub fn underlying_id(&self) -> Result<i32, ()> {
    // Check if the topic ID starts with "N" (node topic)
    if self.id.starts_with("N") {
      // Try to parse the numeric part after "N"
      if let Ok(node_id) = self.id[1..].parse::<i32>() {
        return Ok(node_id);
      }
    }
    Err(())
  }
}

pub fn new_topic(id: &str) -> Topic {
  Topic { id: id.to_string() }
}

pub fn new_node_topic(node_id: &i32) -> Topic {
  Topic {
    id: format!("N{}", node_id),
  }
}

pub fn new_documentation_topic(doc_id: i32) -> Topic {
  Topic {
    id: format!("D{}", doc_id),
  }
}
