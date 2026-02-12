use serde::Serialize;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize)]
pub struct Topic {
  pub id: String,
}

impl Topic {
  pub fn id(&self) -> &str {
    &self.id
  }

  pub fn underlying_id(&self) -> Result<i32, ()> {
    // Check if the topic ID starts with "N" (node topic)
    if self.id.starts_with("N") || self.id.starts_with("D") {
      // Try to parse the numeric part after "N" or "D"
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

pub fn new_comment_topic(comment_id: i32) -> Topic {
  Topic {
    id: format!("C{}", comment_id),
  }
}

pub fn new_invariant_topic(invariant_id: i32) -> Topic {
  Topic {
    id: format!("I{}", invariant_id),
  }
}

pub fn new_attack_vector_topic(id: i32) -> Topic {
  Topic {
    id: format!("A{}", id),
  }
}
