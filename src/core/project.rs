use crate::core;
use crate::core::DataContext;
use crate::documentation;
use crate::solidity;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub fn load_project(
  project_root: &Path,
  data_context: &Arc<Mutex<DataContext>>,
) -> Result<(), String> {
  // Load in-scope files from scope.txt
  let in_scope_files =
    core::load_in_scope_files(project_root).map_err(|e| {
      format!("Failed to load in-scope files from scope.txt: {}", e)
    })?;

  {
    let mut ctx = data_context.lock().unwrap();
    for file in in_scope_files {
      ctx.in_scope_files.insert(file);
    }
  }

  println!("Analyzing Solidity project at: {}", project_root.display());

  // Analyze Solidity project and populate DataContext
  {
    let mut ctx = data_context.lock().unwrap();
    solidity::analyze(project_root, &mut ctx)
      .map_err(|e| format!("Failed to analyze Solidity project: {}", e))?;
  }

  println!("Analyzing documentation files...");

  // Analyze documentation and augment DataContext
  {
    let mut ctx = data_context.lock().unwrap();
    documentation::analyze(project_root, &mut ctx)
      .map_err(|e| format!("Failed to analyze documentation files: {}", e))?;
  }

  Ok(())
}
