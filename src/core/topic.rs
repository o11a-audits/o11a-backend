#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Topic {
  pub id: String,
}

pub fn new_topic(id: &str) -> Topic {
  Topic { id: id.to_string() }
}

pub fn new_node_topic(node_id: i32) -> Topic {
  Topic {
    id: format!("N{}", node_id),
  }
}

pub fn new_documentation_topic(doc_id: i32) -> Topic {
  Topic {
    id: format!("D{}", doc_id),
  }
}
