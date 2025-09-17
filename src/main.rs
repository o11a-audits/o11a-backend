use std::path::Path;

mod solidity;

fn main() {
  let project_path = Path::new("/home/john/olla/server/priv/audits/lido-crmv2");

  match solidity::analyze(project_path) {
    Ok((declarations, in_scope_files, in_scope_declarations)) => {
      println!("Analyzer Results:");
      println!("In-scope files: {}", in_scope_files.len());
      for file in &in_scope_files {
        println!("  - {}", file);
      }

      println!("\nFirst pass declarations found: {}", declarations.len());

      // Group declarations by kind
      let mut by_kind = std::collections::BTreeMap::new();
      for (_node_id, decl) in &declarations {
        let variant_type = if decl.has_executable_code() {
          "Block"
        } else {
          "Flat"
        };
        let kind_str = match decl.declaration_kind() {
          solidity::DeclarationKind::Contract(kind) => {
            format!("Contract({:?}) [{}]", kind, variant_type)
          }
          solidity::DeclarationKind::Function(kind) => {
            format!("Function({:?}) [{}]", kind, variant_type)
          }
          other => format!("{:?} [{}]", other, variant_type),
        };
        let count = by_kind.entry(kind_str).or_insert(0);
        *count += 1;
      }

      println!("\nDeclarations by kind:");
      for (kind, count) in by_kind {
        println!("  {}: {}", kind, count);
      }

      // Show some examples
      println!(
        "\nIn-scope declarations after tree shaking: {}",
        in_scope_declarations.len()
      );

      println!("\nFirst 5 first-pass declarations:");
      for (node_id, decl) in declarations.iter().take(5) {
        let variant_type = if decl.has_executable_code() {
          "Block"
        } else {
          "Flat"
        };
        let kind_str = match decl.declaration_kind() {
          solidity::DeclarationKind::Contract(kind) => format!("Contract({:?})", kind),
          solidity::DeclarationKind::Function(kind) => format!("Function({:?})", kind),
          other => format!("{:?}", other),
        };
        println!(
          "  {} ({}): {} [{}]",
          node_id,
          if decl.is_publicly_in_scope() {
            "publicly-in-scope"
          } else {
            "not-publicly-in-scope"
          },
          kind_str,
          variant_type
        );
      }

      println!("\nFirst 5 in-scope declarations:");
      for (node_id, in_scope_decl) in in_scope_declarations.iter().take(5) {
        let kind_str = match &in_scope_decl.declaration_kind {
          solidity::DeclarationKind::Contract(kind) => format!("Contract({:?})", kind),
          solidity::DeclarationKind::Function(kind) => format!("Function({:?})", kind),
          other => format!("{:?}", other),
        };
        println!(
          "  {} - {}, referenced by {} node(s): {:?}",
          node_id,
          kind_str,
          in_scope_decl.references.len(),
          in_scope_decl.references
        );
        println!(
          "    require/revert: {}, function calls: {}, variable mutations: {}",
          in_scope_decl.require_revert_statements.len(),
          in_scope_decl.function_calls.len(),
          in_scope_decl.variable_mutations.len()
        );
      }
    }
    Err(e) => {
      eprintln!("Error analyzing project: {}", e);
      std::process::exit(1);
    }
  }
}
