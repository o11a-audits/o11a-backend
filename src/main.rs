use foundry_compilers::{Project, ProjectPathsConfig, artifacts::Remapping};
use foundry_compilers_artifacts::NodeType;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;
use std::vec;

mod solidity;

fn main() {
    process(Path::new("/home/john/audits/fallback"));
}

fn process(root: &Path) {
    // Gather remappings
    let mut remappings = vec![];

    if let Ok(file) = File::open(root.join(Path::new("remappings.txt"))) {
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let remapping = Remapping::from_str(line.unwrap().as_str()).unwrap();
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
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::BinaryOperation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Conditional => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ElementaryTypeNameExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::FunctionCall => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::FunctionCallOptions => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Identifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::IndexAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::IndexRangeAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Literal => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::MemberAccess => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::NewExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::TupleExpression => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::UnaryOperation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Block => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Break => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Continue => "<span class=\"keyword\">continue;</span>",
        NodeType::DoWhileStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::EmitStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ExpressionStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ForStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::IfStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::InlineAssembly => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::PlaceholderStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Return => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::RevertStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::TryStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::UncheckedBlock => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::VariableDeclarationStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::VariableDeclaration => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::WhileStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulAssignment => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulBlock => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulBreak => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulCase => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulContinue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulExpressionStatement => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulLeave => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulForLoop => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulFunctionDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulIf => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulSwitch => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulVariableDeclaration => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulFunctionCall => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulIdentifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulLiteral => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulLiteralValue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulHexValue => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::YulTypedName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ContractDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::FunctionDefinition => {
            println!("{:?} {:?}\n", node.node_type, node.other);
            "func"
        }
        NodeType::EventDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ErrorDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ModifierDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::StructDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::EnumDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::UserDefinedValueTypeDefinition => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::PragmaDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ImportDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::UsingForDirective => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::SourceUnit => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::InheritanceSpecifier => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ElementaryTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::FunctionTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ParameterList => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::TryCatchClause => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ModifierInvocation => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::UserDefinedTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::ArrayTypeName => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Mapping => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
        NodeType::Other(_) => {
            println!("Implement: {:?} {:?}\n", node.node_type, node.other);
            ""
        }
    }
}
