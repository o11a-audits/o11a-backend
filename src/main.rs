use std::path::Path;

mod solidity;

fn main() {
    let ast_map = solidity::process(Path::new("/home/john/olla/server/priv/audits/nudgexyz"));
    println!("Processed {} ASTs", ast_map.len());
}
