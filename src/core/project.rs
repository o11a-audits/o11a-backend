use crate::core;
use crate::core::DataContext;
use crate::documentation;
use crate::solidity;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub fn load_project(
  project_root: &Path,
  audit_id: &str,
  data_context: &Arc<Mutex<DataContext>>,
) -> Result<(), String> {
  // Load in-scope files from scope.txt
  let in_scope_files =
    core::load_in_scope_files(project_root).map_err(|e| {
      format!("Failed to load in-scope files from scope.txt: {}", e)
    })?;

  let audit_name = core::load_audit_name(project_root)
    .map_err(|e| format!("Failed to load audit name from audit.txt: {}", e))?;

  // Create the audit if it doesn't exist
  {
    let mut ctx = data_context.lock().unwrap();
    if !ctx.create_audit(audit_id.to_string(), audit_name, in_scope_files) {
      return Err(format!("Audit '{}' already exists", audit_id));
    }
  }

  println!("Analyzing Solidity project at: {}", project_root.display());

  // Analyze Solidity project and populate AuditData
  {
    let mut ctx = data_context.lock().unwrap();
    solidity::analyze(project_root, audit_id, &mut ctx)
      .map_err(|e| format!("Failed to analyze Solidity project: {}", e))?;
  }

  println!("Analyzing documentation files...");

  // Analyze documentation and augment AuditData
  {
    let mut ctx = data_context.lock().unwrap();
    documentation::analyze(project_root, audit_id, &mut ctx)
      .map_err(|e| format!("Failed to analyze documentation files: {}", e))?;
  }

  Ok(())
}
