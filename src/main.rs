use std::path::Path;

mod solidity;

fn main() {
  let project_path = Path::new("/home/john/olla/server/priv/audits/lido-crmv2");

  match solidity::analyze(project_path) {
    Ok((declarations, in_scope_files)) => {
      println!("Analyzer Results:");
      println!("In-scope files: {}", in_scope_files.len());
      for file in &in_scope_files {
        println!("  - {}", file);
      }

      println!("\nDeclarations found: {}", declarations.len());

      // Group declarations by variant type
      let mut by_variant = std::collections::BTreeMap::new();
      for (_node_id, decl) in &declarations {
        let variant_type = if decl.has_executable_code() {
          "Block"
        } else {
          "Flat"
        };
        let count = by_variant.entry(variant_type.to_string()).or_insert(0);
        *count += 1;
      }

      println!("\nDeclarations by variant:");
      for (variant, count) in by_variant {
        println!("  {}: {}", variant, count);
      }

      // Show some examples
      println!("\nFirst 5 declarations:");
      for (node_id, decl) in declarations.iter().take(5) {
        let variant_type = if decl.has_executable_code() {
          "Block"
        } else {
          "Flat"
        };
        println!(
          "  {} ({}): [{}] variant",
          node_id,
          if decl.is_publicly_in_scope() {
            "publicly-in-scope"
          } else {
            "not-publicly-in-scope"
          },
          variant_type
        );
      }
    }
    Err(e) => {
      eprintln!("Error analyzing project: {}", e);
      std::process::exit(1);
    }
  }
}
