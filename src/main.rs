use foundry_compilers::{Project, ProjectPathsConfig, artifacts::Remapping};
use foundry_compilers_artifacts::NodeType;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;
use std::vec;

mod solidity;

fn main() {
    process(Path::new("/home/john/olla/server/priv/audits/nudgexyz"));
}

fn process(root: &Path) {
    // Gather remappings
    let mut remappings = vec![];

    if let Ok(file) = File::open(root.join(Path::new("remappings.txt"))) {
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let line = line.unwrap();
            // If the line is empty, skip it
            if line.is_empty() {
                continue;
            }
            let remapping = Remapping::from_str(line.as_str()).unwrap();
            remappings.push(remapping);
        }
    }

    let project_path_config = ProjectPathsConfig::builder()
        .root(root)
        .sources(ProjectPathsConfig::<_>::find_source_dir(root))
        .libs(ProjectPathsConfig::<_>::find_libs(root))
        .artifacts(ProjectPathsConfig::<_>::find_artifacts_dir(root))
        .tests(root.join(Path::new("test")))
        .cache(root.join(Path::new("cache")))
        .remappings(remappings)
        // The standard, implicit foundry remapping
        .remapping(Remapping::from_str("forge-std/=lib/forge-std/src/").unwrap())
        .build()
        .unwrap();

    let project = Project::builder()
        .paths(project_path_config)
        .build(Default::default())
        .unwrap();

    let output = project.compile().unwrap();

    for (_artifact_id, artifact) in output.artifacts() {
        if let Some(source_unit) = artifact.ast.as_ref() {
            let solidity_ast = solidity::ast_from_artifact(source_unit);
            todo!()
        }
    }
}
