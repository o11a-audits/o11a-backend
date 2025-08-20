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
            print_node_types(&source_unit.nodes);
        }
    }
}

fn print_node_types(nodes: &[foundry_compilers::artifacts::ast::Node]) {
    for node in nodes {
        render_node(node);

        // Recursively process child nodes
        if !node.nodes.is_empty() {
            print_node_types(&node.nodes);
        }
    }
}

fn render_node(node: &foundry_compilers::artifacts::ast::Node) -> &'static str {
    match node.node_type {
        NodeType::Assignment => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::BinaryOperation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Conditional => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ElementaryTypeNameExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::FunctionCall => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::FunctionCallOptions => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Identifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::IndexAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::IndexRangeAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Literal => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::MemberAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::NewExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::TupleExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::UnaryOperation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Block => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Break => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Continue => "<span class=\"keyword\">continue;</span>",
        NodeType::DoWhileStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::EmitStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ExpressionStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ForStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::IfStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::InlineAssembly => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::PlaceholderStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Return => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::RevertStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::TryStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::UncheckedBlock => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::VariableDeclarationStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::VariableDeclaration => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::WhileStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulAssignment => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulBlock => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulBreak => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulCase => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulContinue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulExpressionStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulLeave => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulForLoop => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulFunctionDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulIf => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulSwitch => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulVariableDeclaration => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulFunctionCall => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulIdentifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulLiteral => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulLiteralValue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulHexValue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::YulTypedName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ContractDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::FunctionDefinition => {
            println!("{:?} {:?}\n", node.node_type, node);
            "func"
        }
        NodeType::EventDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ErrorDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ModifierDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::StructDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::EnumDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::UserDefinedValueTypeDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::PragmaDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ImportDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::UsingForDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::SourceUnit => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::InheritanceSpecifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ElementaryTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::FunctionTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ParameterList => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::TryCatchClause => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ModifierInvocation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::UserDefinedTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::ArrayTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Mapping => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
        NodeType::Other(_) => {
            println!("Implement: {:?} {:?}\n", node.node_type, node);
            ""
        }
    }
}
