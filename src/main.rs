use std::path::Path;

mod solidity;

fn main() {
  match solidity::process(Path::new("/home/john/olla/server/priv/audits/lido-crmv2")) {
    Ok(ast_map) => {
      let total_asts: usize = ast_map.values().map(|v| v.len()).sum();
      println!(
        "Processed {} unique paths with {} total ASTs",
        ast_map.len(),
        total_asts
      );

      for (path, asts) in &ast_map {
        println!("  {}: {} AST(s)", path, asts.len());
      }
    }
    Err(e) => {
      eprintln!("Error processing ASTs: {}", e);
      std::process::exit(1);
    }
  }
}
