use foundry_compilers::artifacts::ast;
use foundry_compilers::artifacts::ast::{LowFidelitySourceLocation, Node};
use foundry_compilers::compilers::solc;
use foundry_compilers::{Project, ProjectPathsConfig, artifacts::Remapping};
use foundry_compilers_artifacts::NodeType;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;
use std::{panic, vec};

pub fn process(root: &Path) -> HashMap<i32, AST> {
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

    let mut ast_map = HashMap::new();

    for (_artifact_id, artifact) in output.artifacts() {
        if let Some(source_unit) = artifact.ast.as_ref() {
            let solidity_ast = ast_from_artifact(source_unit).unwrap();
            ast_map.insert(solidity_ast.node_id, solidity_ast);
        }
    }

    ast_map
}

pub struct AST {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
    absolute_path: String,
}

pub fn ast_from_artifact(artifact: &ast::Ast) -> Result<AST, String> {
    let nodes: Result<Vec<ASTNode>, String> = artifact
        .nodes
        .iter()
        .map(|n| ASTNode::try_from_node(&n))
        .collect();

    Ok(AST {
        node_id: artifact.id as i32,
        src_location: SourceLocation::from(&artifact.src),
        nodes: nodes?,
        absolute_path: artifact.absolute_path.clone(),
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SourceLocation {
    pub start: Option<usize>,
    pub length: Option<usize>,
    pub index: Option<usize>,
}

impl From<&LowFidelitySourceLocation> for SourceLocation {
    fn from(low_fidelity: &LowFidelitySourceLocation) -> Self {
        SourceLocation {
            start: Some(low_fidelity.start),
            length: low_fidelity.length,
            index: low_fidelity.index,
        }
    }
}

impl FromStr for SourceLocation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let invalid_location = move || format!("{s} invalid source location");

        let mut split = s.split(':');
        let start = split
            .next()
            .ok_or_else(invalid_location)?
            .parse::<isize>()
            .map_err(|_| invalid_location())?;
        let length = split
            .next()
            .ok_or_else(invalid_location)?
            .parse::<isize>()
            .map_err(|_| invalid_location())?;
        let index = split
            .next()
            .ok_or_else(invalid_location)?
            .parse::<isize>()
            .map_err(|_| invalid_location())?;

        let start = if start < 0 {
            None
        } else {
            Some(start as usize)
        };
        let length = if length < 0 {
            None
        } else {
            Some(length as usize)
        };
        let index = if index < 0 {
            None
        } else {
            Some(index as usize)
        };

        Ok(Self {
            start,
            length,
            index,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionKind {
    Constructor,
    Function,
    Fallback,
    Receive,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ContractKind {
    Contract,
    Library,
    Abstract,
    Interface,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionStateMutability {
    Pure,
    View,
    NonPayable,
    Payable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionVisibility {
    Public,
    Private,
    Internal,
    External,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariableVisibility {
    Public,
    Private,
    Internal,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariableMutability {
    Mutable,
    Immutable,
    Constant,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StorageLocation {
    Default,
    Storage,
    Memory,
    Calldata,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Number,
    Bool,
    String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeDescriptions {
    pub type_identifier: String,
    pub type_string: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArgumentType {
    pub type_identifier: String,
    pub type_string: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionCallKind {
    FunctionCall,
    TypeConversion,
    StructConstructor,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Increment,  // ++
    Decrement,  // --
    Plus,       // +
    Minus,      // -
    BitwiseNot, // ~
    Not,        // !
    Delete,     // delete
}

impl FromStr for UnaryOperator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "!" => Ok(UnaryOperator::Not),
            "++" => Ok(UnaryOperator::Increment),
            "--" => Ok(UnaryOperator::Decrement),
            "+" => Ok(UnaryOperator::Plus),
            "-" => Ok(UnaryOperator::Minus),
            "~" => Ok(UnaryOperator::BitwiseNot),
            "delete" => Ok(UnaryOperator::Delete),
            _ => Err(format!("Invalid unary operator: {}", s)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    // Arithmetic
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Modulo,   // %
    Power,    // **

    // Comparison
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=

    // Logical
    And, // &&
    Or,  // ||

    // Bitwise
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    LeftShift,  // <<
    RightShift, // >>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignmentOperator {
    Assign,           // =
    AddAssign,        // +=
    SubtractAssign,   // -=
    MultiplyAssign,   // *=
    DivideAssign,     // /=
    ModuloAssign,     // %=
    BitwiseAndAssign, // &=
    BitwiseOrAssign,  // |=
    BitwiseXorAssign, // ^=
    LeftShiftAssign,  // <<=
    RightShiftAssign, // >>=
}

impl FromStr for FunctionKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "constructor" => Ok(FunctionKind::Constructor),
            "function" => Ok(FunctionKind::Function),
            "fallback" => Ok(FunctionKind::Fallback),
            "receive" => Ok(FunctionKind::Receive),
            _ => Err(format!("Unknown function kind: {}", s)),
        }
    }
}

impl FromStr for FunctionStateMutability {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pure" => Ok(FunctionStateMutability::Pure),
            "view" => Ok(FunctionStateMutability::View),
            "nonpayable" => Ok(FunctionStateMutability::NonPayable),
            "payable" => Ok(FunctionStateMutability::Payable),
            _ => Err(format!("Unknown state mutability: {}", s)),
        }
    }
}

impl FromStr for FunctionVisibility {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "public" => Ok(FunctionVisibility::Public),
            "private" => Ok(FunctionVisibility::Private),
            "internal" => Ok(FunctionVisibility::Internal),
            "external" => Ok(FunctionVisibility::External),
            _ => Err(format!("Unknown visibility: {}", s)),
        }
    }
}

impl FromStr for VariableVisibility {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "public" => Ok(VariableVisibility::Public),
            "private" => Ok(VariableVisibility::Private),
            "internal" => Ok(VariableVisibility::Internal),
            _ => Err(format!("Unknown variable visibility: {}", s)),
        }
    }
}

impl FromStr for LiteralKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "number" => Ok(LiteralKind::Number),
            "bool" => Ok(LiteralKind::Bool),
            "string" => Ok(LiteralKind::String),
            _ => Err(format!("Unknown literal kind: {}", s)),
        }
    }
}

impl FromStr for FunctionCallKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "functionCall" => Ok(FunctionCallKind::FunctionCall),
            "typeConversion" => Ok(FunctionCallKind::TypeConversion),
            "structConstructorCall" => Ok(FunctionCallKind::StructConstructor),
            _ => Err(format!("Unknown function call kind: {}", s)),
        }
    }
}

impl FromStr for BinaryOperator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Subtract),
            "*" => Ok(BinaryOperator::Multiply),
            "/" => Ok(BinaryOperator::Divide),
            "%" => Ok(BinaryOperator::Modulo),
            "**" => Ok(BinaryOperator::Power),
            "==" => Ok(BinaryOperator::Equal),
            "!=" => Ok(BinaryOperator::NotEqual),
            "<" => Ok(BinaryOperator::LessThan),
            "<=" => Ok(BinaryOperator::LessThanOrEqual),
            ">" => Ok(BinaryOperator::GreaterThan),
            ">=" => Ok(BinaryOperator::GreaterThanOrEqual),
            "&&" => Ok(BinaryOperator::And),
            "||" => Ok(BinaryOperator::Or),
            "&" => Ok(BinaryOperator::BitwiseAnd),
            "|" => Ok(BinaryOperator::BitwiseOr),
            "^" => Ok(BinaryOperator::BitwiseXor),
            "<<" => Ok(BinaryOperator::LeftShift),
            ">>" => Ok(BinaryOperator::RightShift),
            _ => Err(format!("Unknown binary operator: {}", s)),
        }
    }
}

impl FromStr for AssignmentOperator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "=" => Ok(AssignmentOperator::Assign),
            "+=" => Ok(AssignmentOperator::AddAssign),
            "-=" => Ok(AssignmentOperator::SubtractAssign),
            "*=" => Ok(AssignmentOperator::MultiplyAssign),
            "/=" => Ok(AssignmentOperator::DivideAssign),
            "%=" => Ok(AssignmentOperator::ModuloAssign),
            "&=" => Ok(AssignmentOperator::BitwiseAndAssign),
            "|=" => Ok(AssignmentOperator::BitwiseOrAssign),
            "^=" => Ok(AssignmentOperator::BitwiseXorAssign),
            "<<=" => Ok(AssignmentOperator::LeftShiftAssign),
            ">>=" => Ok(AssignmentOperator::RightShiftAssign),
            _ => Err(format!("Unknown binary operator: {}", s)),
        }
    }
}

impl FromStr for VariableMutability {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "mutable" => Ok(VariableMutability::Mutable),
            "immutable" => Ok(VariableMutability::Immutable),
            "constant" => Ok(VariableMutability::Constant),
            _ => Err(format!("Unknown mutability: {}", s)),
        }
    }
}

impl FromStr for StorageLocation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "default" => Ok(StorageLocation::Default),
            "storage" => Ok(StorageLocation::Storage),
            "memory" => Ok(StorageLocation::Memory),
            "calldata" => Ok(StorageLocation::Calldata),
            _ => Err(format!("Invalid storage location: {}", s)),
        }
    }
}

impl FromStr for ContractKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "contract" => Ok(ContractKind::Contract),
            "library" => Ok(ContractKind::Library),
            "abstract" => Ok(ContractKind::Abstract),
            "interface" => Ok(ContractKind::Interface),
            _ => Err(format!("Invalid contract kind: {}", s)),
        }
    }
}

impl FromStr for ContractVariableVisibility {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "public" => Ok(ContractVariableVisibility::Public),
            "private" => Ok(ContractVariableVisibility::Private),
            "internal" => Ok(ContractVariableVisibility::Internal),
            _ => Err(format!("Invalid contract variable visibility: {}", s)),
        }
    }
}

impl TypeDescriptions {
    pub fn from_json(value: &serde_json::Value) -> Result<Self, String> {
        let type_identifier = value
            .get("typeIdentifier")
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("TypeDescriptions missing typeIdentifier: {:?}", value))?
            .to_string();
        let type_string = value
            .get("typeString")
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("TypeDescriptions missing typeString: {:?}", value))?
            .to_string();

        Ok(TypeDescriptions {
            type_identifier,
            type_string,
        })
    }
}

impl ArgumentType {
    pub fn from_json(value: &serde_json::Value) -> Result<Self, String> {
        let type_identifier = value
            .get("typeIdentifier")
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("ArgumentType missing typeIdentifier: {:?}", value))?
            .to_string();
        let type_string = value
            .get("typeString")
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("ArgumentType missing typeString: {:?}", value))?
            .to_string();

        Ok(ArgumentType {
            type_identifier,
            type_string,
        })
    }
}

pub enum ContractVariableVisibility {
    Public,
    Private,
    Internal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    // Expression nodes
    Assignment {
        node_id: i32,
        src_location: SourceLocation,
        operator: AssignmentOperator,
        right_and_side: Box<ASTNode>,
        left_hand_side: Box<ASTNode>,
    },
    BinaryOperation {
        node_id: i32,
        src_location: SourceLocation,
        left_expression: Box<ASTNode>,
        operator: BinaryOperator,
        right_expression: Box<ASTNode>,
        type_descriptions: TypeDescriptions,
    },
    Conditional {
        node_id: i32,
        src_location: SourceLocation,
        condition: Box<ASTNode>,
        true_expression: Box<ASTNode>,
        false_expression: Option<Box<ASTNode>>,
    },
    ElementaryTypeNameExpression {
        node_id: i32,
        src_location: SourceLocation,
        argument_types: Vec<ArgumentType>,
        type_descriptions: TypeDescriptions,
        type_name: Box<ASTNode>,
    },
    FunctionCall {
        node_id: i32,
        src_location: SourceLocation,
        arguments: Vec<ASTNode>,
        expression: Box<ASTNode>,
        kind: FunctionCallKind,
        name_locations: Vec<SourceLocation>,
        names: Vec<String>,
        try_call: bool,
        type_descriptions: TypeDescriptions,
    },
    FunctionCallOptions {
        node_id: i32,
        src_location: SourceLocation,
        expression: Box<ASTNode>,
        options: Vec<ASTNode>,
    },
    Identifier {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
        overloaded_declarations: Vec<i32>,
        referenced_declaration: i32,
        type_descriptions: TypeDescriptions,
    },
    IdentifierPath {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
        name_locations: Vec<SourceLocation>,
        referenced_declaration: i32,
    },
    IndexAccess {
        node_id: i32,
        src_location: SourceLocation,
        base_expression: Box<ASTNode>,
        index_expression: Box<ASTNode>,
    },
    IndexRangeAccess {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Literal {
        node_id: i32,
        src_location: SourceLocation,
        hex_value: String,
        kind: LiteralKind,
        type_descriptions: TypeDescriptions,
        value: String,
    },
    MemberAccess {
        node_id: i32,
        src_location: SourceLocation,
        expression: Box<ASTNode>,
        member_location: SourceLocation,
        member_name: String,
    },
    NewExpression {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    TupleExpression {
        node_id: i32,
        src_location: SourceLocation,
        components: Vec<ASTNode>,
    },
    UnaryOperation {
        node_id: i32,
        src_location: SourceLocation,
        prefix: bool,
        operator: UnaryOperator,
        sub_expression: Box<ASTNode>,
    },
    EnumValue {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
        name_location: SourceLocation,
    },

    // Statement nodes
    Block {
        node_id: i32,
        src_location: SourceLocation,
        statements: Vec<ASTNode>,
    },
    Break {
        node_id: i32,
        src_location: SourceLocation,
    },
    Continue {
        node_id: i32,
        src_location: SourceLocation,
    },
    DoWhileStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    EmitStatement {
        node_id: i32,
        src_location: SourceLocation,
        event_call: Box<ASTNode>,
    },
    ExpressionStatement {
        node_id: i32,
        src_location: SourceLocation,
        expression: Box<ASTNode>,
    },
    ForStatement {
        node_id: i32,
        src_location: SourceLocation,
        body: Box<ASTNode>,
        condition: Box<ASTNode>,
        initialization_expression: Box<ASTNode>,
        is_simple_counter_loop: bool,
        loop_expression: Box<ASTNode>,
    },
    IfStatement {
        node_id: i32,
        src_location: SourceLocation,
        condition: Box<ASTNode>,
        true_body: Box<ASTNode>,
        false_body: Option<Box<ASTNode>>,
    },
    InlineAssembly {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    PlaceholderStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    Return {
        node_id: i32,
        src_location: SourceLocation,
        expression: Option<Box<ASTNode>>,
        function_return_parameters: i32,
    },
    RevertStatement {
        node_id: i32,
        src_location: SourceLocation,
        error_call: Box<ASTNode>,
    },
    TryStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    UncheckedBlock {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    VariableDeclarationStatement {
        node_id: i32,
        src_location: SourceLocation,
        declarations: Vec<ASTNode>,
        initial_value: Option<Box<ASTNode>>,
    },
    VariableDeclaration {
        node_id: i32,
        src_location: SourceLocation,
        constant: bool,
        function_selector: Option<String>,
        mutability: VariableMutability,
        name: String,
        name_location: SourceLocation,
        scope: i32,
        state_variable: bool,
        storage_location: StorageLocation,
        type_name: Box<ASTNode>,
        value: Option<Box<ASTNode>>,
        visibility: VariableVisibility,
    },
    WhileStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },

    // Definition nodes
    ContractDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        abstract_: bool,
        base_contracts: Vec<ASTNode>,
        name: String,
        name_location: SourceLocation,
        contract_kind: ContractKind,
    },
    FunctionDefinition {
        node_id: i32,
        src_location: SourceLocation,
        body: Option<Box<ASTNode>>,
        documentation: Option<Box<ASTNode>>,
        implemented: bool,
        kind: FunctionKind,
        modifiers: Vec<ASTNode>,
        name: String,
        name_location: SourceLocation,
        parameters: Box<ASTNode>,
        return_parameters: Box<ASTNode>,
        scope: i32,
        state_mutability: FunctionStateMutability,
        virtual_: bool,
        visibility: FunctionVisibility,
    },
    EventDefinition {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
        name_location: SourceLocation,
        parameters: Box<ASTNode>,
    },
    ErrorDefinition {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
        name_location: SourceLocation,
        parameters: Box<ASTNode>,
    },
    ModifierDefinition {
        node_id: i32,
        src_location: SourceLocation,
        body: Box<ASTNode>,
        documentation: Option<Box<ASTNode>>,
        name: String,
        name_location: SourceLocation,
        parameters: Box<ASTNode>,
        virtual_: bool,
        visibility: FunctionVisibility,
    },
    StructDefinition {
        node_id: i32,
        src_location: SourceLocation,
        members: Vec<ASTNode>,
        canonical_name: String,
        name: String,
        name_location: SourceLocation,
        visibility: VariableVisibility,
    },
    EnumDefinition {
        node_id: i32,
        src_location: SourceLocation,
        members: Vec<ASTNode>,
        canonical_name: String,
        name: String,
        name_location: SourceLocation,
    },
    UserDefinedValueTypeDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },

    // Directive nodes
    PragmaDirective {
        node_id: i32,
        src_location: SourceLocation,
        literals: Vec<String>,
    },
    ImportDirective {
        node_id: i32,
        src_location: SourceLocation,
        absolute_path: String,
        file: String,
        source_unit: i32,
    },
    UsingForDirective {
        node_id: i32,
        src_location: SourceLocation,
        global: bool,
        library_name: Box<ASTNode>,
        type_name: Box<ASTNode>,
    },

    // Other nodes
    SourceUnit {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
    },
    InheritanceSpecifier {
        node_id: i32,
        src_location: SourceLocation,
        base_name: Box<ASTNode>,
    },
    ElementaryTypeName {
        node_id: i32,
        src_location: SourceLocation,
        name: String,
    },
    FunctionTypeName {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ParameterList {
        node_id: i32,
        src_location: SourceLocation,
        parameters: Vec<ASTNode>,
    },
    TryCatchClause {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ModifierInvocation {
        node_id: i32,
        src_location: SourceLocation,
        modifier_name: Box<ASTNode>,
        arguments: Option<Vec<ASTNode>>,
    },
    UserDefinedTypeName {
        node_id: i32,
        src_location: SourceLocation,
        path_node: Box<ASTNode>,
        referenced_declaration: i32,
    },
    ArrayTypeName {
        node_id: i32,
        src_location: SourceLocation,
        base_type: Box<ASTNode>,
    },
    Mapping {
        node_id: i32,
        src_location: SourceLocation,
        key_name: Option<String>,
        key_name_location: SourceLocation,
        key_type: Box<ASTNode>,
        value_name: Option<String>,
        value_name_location: SourceLocation,
        value_type: Box<ASTNode>,
    },

    StructuredDocumentation {
        node_id: i32,
        src_location: SourceLocation,
        text: String,
    },

    // Catch-all for unknown node types
    Other {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
        node_type: String,
    },
}

impl ASTNode {
    pub fn node_id(&self) -> i32 {
        match self {
            ASTNode::Assignment { node_id, .. } => *node_id,
            ASTNode::BinaryOperation { node_id, .. } => *node_id,
            ASTNode::Conditional { node_id, .. } => *node_id,
            ASTNode::ElementaryTypeNameExpression { node_id, .. } => *node_id,
            ASTNode::FunctionCall { node_id, .. } => *node_id,
            ASTNode::FunctionCallOptions { node_id, .. } => *node_id,
            ASTNode::Identifier { node_id, .. } => *node_id,
            ASTNode::IdentifierPath { node_id, .. } => *node_id,
            ASTNode::IndexAccess { node_id, .. } => *node_id,
            ASTNode::IndexRangeAccess { node_id, .. } => *node_id,
            ASTNode::Literal { node_id, .. } => *node_id,
            ASTNode::MemberAccess { node_id, .. } => *node_id,
            ASTNode::NewExpression { node_id, .. } => *node_id,
            ASTNode::TupleExpression { node_id, .. } => *node_id,
            ASTNode::UnaryOperation { node_id, .. } => *node_id,
            ASTNode::EnumValue { node_id, .. } => *node_id,
            ASTNode::Block { node_id, .. } => *node_id,
            ASTNode::Break { node_id, .. } => *node_id,
            ASTNode::Continue { node_id, .. } => *node_id,
            ASTNode::DoWhileStatement { node_id, .. } => *node_id,
            ASTNode::EmitStatement { node_id, .. } => *node_id,
            ASTNode::ExpressionStatement { node_id, .. } => *node_id,
            ASTNode::ForStatement { node_id, .. } => *node_id,
            ASTNode::IfStatement { node_id, .. } => *node_id,
            ASTNode::InlineAssembly { node_id, .. } => *node_id,
            ASTNode::PlaceholderStatement { node_id, .. } => *node_id,
            ASTNode::Return { node_id, .. } => *node_id,
            ASTNode::RevertStatement { node_id, .. } => *node_id,
            ASTNode::TryStatement { node_id, .. } => *node_id,
            ASTNode::UncheckedBlock { node_id, .. } => *node_id,
            ASTNode::VariableDeclarationStatement { node_id, .. } => *node_id,
            ASTNode::VariableDeclaration { node_id, .. } => *node_id,
            ASTNode::WhileStatement { node_id, .. } => *node_id,
            ASTNode::ContractDefinition { node_id, .. } => *node_id,
            ASTNode::FunctionDefinition { node_id, .. } => *node_id,
            ASTNode::EventDefinition { node_id, .. } => *node_id,
            ASTNode::ErrorDefinition { node_id, .. } => *node_id,
            ASTNode::ModifierDefinition { node_id, .. } => *node_id,
            ASTNode::StructDefinition { node_id, .. } => *node_id,
            ASTNode::EnumDefinition { node_id, .. } => *node_id,
            ASTNode::UserDefinedValueTypeDefinition { node_id, .. } => *node_id,
            ASTNode::PragmaDirective { node_id, .. } => *node_id,
            ASTNode::ImportDirective { node_id, .. } => *node_id,
            ASTNode::UsingForDirective { node_id, .. } => *node_id,
            ASTNode::SourceUnit { node_id, .. } => *node_id,
            ASTNode::InheritanceSpecifier { node_id, .. } => *node_id,
            ASTNode::ElementaryTypeName { node_id, .. } => *node_id,
            ASTNode::FunctionTypeName { node_id, .. } => *node_id,
            ASTNode::ParameterList { node_id, .. } => *node_id,
            ASTNode::TryCatchClause { node_id, .. } => *node_id,
            ASTNode::ModifierInvocation { node_id, .. } => *node_id,
            ASTNode::UserDefinedTypeName { node_id, .. } => *node_id,
            ASTNode::ArrayTypeName { node_id, .. } => *node_id,
            ASTNode::Mapping { node_id, .. } => *node_id,
            ASTNode::StructuredDocumentation { node_id, .. } => *node_id,
            ASTNode::Other { node_id, .. } => *node_id,
        }
    }

    pub fn src_location(&self) -> &SourceLocation {
        match self {
            ASTNode::Assignment { src_location, .. } => src_location,
            ASTNode::BinaryOperation { src_location, .. } => src_location,
            ASTNode::Conditional { src_location, .. } => src_location,
            ASTNode::ElementaryTypeNameExpression { src_location, .. } => src_location,
            ASTNode::FunctionCall { src_location, .. } => src_location,
            ASTNode::FunctionCallOptions { src_location, .. } => src_location,
            ASTNode::Identifier { src_location, .. } => src_location,
            ASTNode::IdentifierPath { src_location, .. } => src_location,
            ASTNode::IndexAccess { src_location, .. } => src_location,
            ASTNode::IndexRangeAccess { src_location, .. } => src_location,
            ASTNode::Literal { src_location, .. } => src_location,
            ASTNode::MemberAccess { src_location, .. } => src_location,
            ASTNode::NewExpression { src_location, .. } => src_location,
            ASTNode::TupleExpression { src_location, .. } => src_location,
            ASTNode::UnaryOperation { src_location, .. } => src_location,
            ASTNode::EnumValue { src_location, .. } => src_location,
            ASTNode::Block { src_location, .. } => src_location,
            ASTNode::Break { src_location, .. } => src_location,
            ASTNode::Continue { src_location, .. } => src_location,
            ASTNode::DoWhileStatement { src_location, .. } => src_location,
            ASTNode::EmitStatement { src_location, .. } => src_location,
            ASTNode::ExpressionStatement { src_location, .. } => src_location,
            ASTNode::ForStatement { src_location, .. } => src_location,
            ASTNode::IfStatement { src_location, .. } => src_location,
            ASTNode::InlineAssembly { src_location, .. } => src_location,
            ASTNode::PlaceholderStatement { src_location, .. } => src_location,
            ASTNode::Return { src_location, .. } => src_location,
            ASTNode::RevertStatement { src_location, .. } => src_location,
            ASTNode::TryStatement { src_location, .. } => src_location,
            ASTNode::UncheckedBlock { src_location, .. } => src_location,
            ASTNode::VariableDeclarationStatement { src_location, .. } => src_location,
            ASTNode::VariableDeclaration { src_location, .. } => src_location,
            ASTNode::WhileStatement { src_location, .. } => src_location,
            ASTNode::ContractDefinition { src_location, .. } => src_location,
            ASTNode::FunctionDefinition { src_location, .. } => src_location,
            ASTNode::EventDefinition { src_location, .. } => src_location,
            ASTNode::ErrorDefinition { src_location, .. } => src_location,
            ASTNode::ModifierDefinition { src_location, .. } => src_location,
            ASTNode::StructDefinition { src_location, .. } => src_location,
            ASTNode::EnumDefinition { src_location, .. } => src_location,
            ASTNode::UserDefinedValueTypeDefinition { src_location, .. } => src_location,
            ASTNode::PragmaDirective { src_location, .. } => src_location,
            ASTNode::ImportDirective { src_location, .. } => src_location,
            ASTNode::UsingForDirective { src_location, .. } => src_location,
            ASTNode::SourceUnit { src_location, .. } => src_location,
            ASTNode::InheritanceSpecifier { src_location, .. } => src_location,
            ASTNode::ElementaryTypeName { src_location, .. } => src_location,
            ASTNode::FunctionTypeName { src_location, .. } => src_location,
            ASTNode::ParameterList { src_location, .. } => src_location,
            ASTNode::TryCatchClause { src_location, .. } => src_location,
            ASTNode::ModifierInvocation { src_location, .. } => src_location,
            ASTNode::UserDefinedTypeName { src_location, .. } => src_location,
            ASTNode::ArrayTypeName { src_location, .. } => src_location,
            ASTNode::Mapping { src_location, .. } => src_location,
            ASTNode::StructuredDocumentation { src_location, .. } => src_location,
            ASTNode::Other { src_location, .. } => src_location,
        }
    }

    pub fn nodes(&self) -> Vec<&ASTNode> {
        match self {
            ASTNode::Assignment {
                right_and_side,
                left_hand_side,
                ..
            } => vec![right_and_side, left_hand_side],
            ASTNode::BinaryOperation {
                left_expression,
                right_expression,
                ..
            } => vec![left_expression, right_expression],
            ASTNode::Conditional {
                condition,
                true_expression,
                false_expression,
                ..
            } => match false_expression {
                Some(false_expr) => vec![condition, true_expression, false_expr],
                None => vec![condition, true_expression],
            },
            ASTNode::ElementaryTypeNameExpression { .. } => vec![],
            ASTNode::FunctionCall {
                arguments,
                expression,
                ..
            } => {
                let mut result = vec![&**expression];
                for item in arguments {
                    result.push(item);
                }
                result
            }
            ASTNode::FunctionCallOptions {
                expression,
                options,
                ..
            } => {
                let mut result = vec![&**expression];
                for item in options {
                    result.push(item);
                }
                result
            }
            ASTNode::Identifier { .. } => vec![],
            ASTNode::IdentifierPath { .. } => vec![],
            ASTNode::IndexAccess {
                base_expression,
                index_expression,
                ..
            } => vec![base_expression, index_expression],
            ASTNode::IndexRangeAccess { .. } => panic!("IndexRangeAccess not implemented"),
            ASTNode::Literal { .. } => vec![],
            ASTNode::MemberAccess { expression, .. } => vec![expression],
            ASTNode::NewExpression { .. } => panic!("New expression not implemented"),
            ASTNode::TupleExpression { components, .. } => {
                let mut result = vec![];
                for item in components {
                    result.push(item);
                }
                result
            }
            ASTNode::UnaryOperation { sub_expression, .. } => vec![sub_expression],
            ASTNode::EnumValue { .. } => vec![],
            ASTNode::Block { statements, .. } => {
                let mut result = vec![];
                for item in statements {
                    result.push(item);
                }
                result
            }
            ASTNode::Break { .. } => vec![],
            ASTNode::Continue { .. } => vec![],
            ASTNode::DoWhileStatement { .. } => panic!("DoWhileStatement not implemented"),
            ASTNode::EmitStatement { event_call, .. } => vec![event_call],
            ASTNode::ExpressionStatement { expression, .. } => vec![expression],
            ASTNode::ForStatement {
                body,
                condition,
                initialization_expression,
                loop_expression,
                ..
            } => vec![body, condition, initialization_expression, loop_expression],
            ASTNode::IfStatement {
                condition,
                true_body,
                false_body,
                ..
            } => match false_body {
                Some(false_body) => vec![condition, true_body, false_body],
                None => vec![condition, true_body],
            },
            ASTNode::InlineAssembly { .. } => vec![],
            ASTNode::PlaceholderStatement { .. } => vec![],
            ASTNode::Return { expression, .. } => match expression {
                Some(expr) => vec![expr],
                None => vec![],
            },
            ASTNode::RevertStatement { error_call, .. } => vec![error_call],
            ASTNode::TryStatement { .. } => panic!("TryStatement not implemented"),
            ASTNode::UncheckedBlock { .. } => panic!("UncheckedBlock not implemented"),
            ASTNode::VariableDeclarationStatement {
                declarations,
                initial_value,
                ..
            } => {
                let mut result = vec![];
                for item in declarations {
                    result.push(item);
                }
                match initial_value {
                    Some(value) => result.push(value),
                    None => {}
                }
                result
            }
            ASTNode::VariableDeclaration { value, .. } => match value {
                Some(val) => vec![val],
                None => vec![],
            },
            ASTNode::WhileStatement { .. } => panic!("WhileStatement not implemented"),
            ASTNode::ContractDefinition {
                nodes,
                base_contracts,
                ..
            } => {
                let mut result = vec![];
                for item in nodes {
                    result.push(item);
                }
                for item in base_contracts {
                    result.push(item);
                }
                result
            }
            ASTNode::FunctionDefinition {
                body,
                documentation,
                modifiers,
                parameters,
                return_parameters,
                ..
            } => {
                let mut result = vec![];
                match body {
                    Some(body_node) => result.push(&**body_node),
                    None => {}
                }
                match documentation {
                    Some(doc) => result.push(&**doc),
                    None => {}
                }
                for item in modifiers {
                    result.push(item);
                }
                result.push(&**parameters);
                result.push(&**return_parameters);
                result
            }
            ASTNode::EventDefinition { parameters, .. } => vec![parameters],
            ASTNode::ErrorDefinition { parameters, .. } => vec![parameters],
            ASTNode::ModifierDefinition {
                body,
                documentation,
                parameters,
                ..
            } => {
                let mut result = vec![&**body, &**parameters];
                match documentation {
                    Some(doc) => result.push(doc),
                    None => {}
                }
                result
            }
            ASTNode::StructDefinition { members, .. } => {
                let mut result = vec![];
                for item in members {
                    result.push(item);
                }
                result
            }
            ASTNode::EnumDefinition { members, .. } => {
                let mut result = vec![];
                for item in members {
                    result.push(item);
                }
                result
            }
            ASTNode::UserDefinedValueTypeDefinition { .. } => {
                panic!("UserDefinedValueTypeDefinition")
            }
            ASTNode::PragmaDirective { .. } => vec![],
            ASTNode::ImportDirective { .. } => vec![],
            ASTNode::UsingForDirective {
                library_name,
                type_name,
                ..
            } => vec![library_name, type_name],
            ASTNode::SourceUnit { nodes, .. } => {
                let mut result = vec![];
                for item in nodes {
                    result.push(item);
                }
                result
            }
            ASTNode::InheritanceSpecifier { base_name, .. } => vec![base_name],
            ASTNode::ElementaryTypeName { .. } => vec![],
            ASTNode::FunctionTypeName { .. } => panic!("FunctionTypeName not implemented"),
            ASTNode::ParameterList { parameters, .. } => {
                let mut result = vec![];
                for item in parameters {
                    result.push(item);
                }
                result
            }
            ASTNode::TryCatchClause { .. } => panic!("TryCatchClause not implemented"),
            ASTNode::ModifierInvocation {
                modifier_name,
                arguments,
                ..
            } => {
                let mut result = vec![&**modifier_name];
                match arguments {
                    Some(args) => {
                        for item in args {
                            result.push(item);
                        }
                    }
                    None => {}
                }
                result
            }
            ASTNode::UserDefinedTypeName { path_node, .. } => vec![path_node],
            ASTNode::ArrayTypeName { base_type, .. } => vec![base_type],
            ASTNode::Mapping {
                key_type,
                value_type,
                ..
            } => vec![key_type, value_type],
            ASTNode::StructuredDocumentation { .. } => vec![],
            ASTNode::Other { nodes, body, .. } => {
                let mut result = vec![];
                for item in nodes {
                    result.push(item);
                }
                match body {
                    Some(body_node) => result.push(body_node),
                    None => {}
                }
                result
            }
        }
    }
}

pub fn node_from_json(val: &serde_json::Value) -> Result<ASTNode, String> {
    let node_type_str = val
        .get("nodeType")
        .and_then(|v| v.as_str())
        .ok_or_else(|| format!("Missing nodeType field: {:?}", val))?;

    let src_location = val
        .get("src")
        .and_then(|v| v.as_str())
        .ok_or_else(|| format!("Missing src field: {:?}", val))
        .and_then(|v| SourceLocation::from_str(v))?;

    match node_type_str {
        "Assignment" => todo!(),
        "BinaryOperation" => todo!(),
        "Conditional" => todo!(),
        "ElementaryTypeNameExpression" => todo!(),
        "FunctionCall" => todo!(),
        "FunctionCallOptions" => todo!(),
        "Identifier" => todo!(),
        "IdentifierPath" => todo!(),
        "IndexAccess" => todo!(),
        "IndexRangeAccess" => todo!(),
        "Literal" => todo!(),
        "MemberAccess" => todo!(),
        "NewExpression" => todo!(),
        "TupleExpression" => todo!(),
        "UnaryOperation" => todo!(),
        "EnumValue" => todo!(),
        "Block" => todo!(),
        "Break" => todo!(),
        "Continue" => todo!(),
        "DoWhileStatement" => todo!(),
        "EmitStatement" => todo!(),
        "ExpressionStatement" => todo!(),
        "ForStatement" => todo!(),
        "IfStatement" => todo!(),
        "InlineAssembly" => todo!(),
        "PlaceholderStatement" => todo!(),
        "Return" => todo!(),
        "RevertStatement" => todo!(),
        "TryStatement" => todo!(),
        "UncheckedBlock" => todo!(),
        "VariableDeclarationStatement" => todo!(),
        "VariableDeclaration" => todo!(),
        "WhileStatement" => todo!(),
        "ContractDefinition" => todo!(),
        "FunctionDefinition" => todo!(),
        "EventDefinition" => todo!(),
        "ErrorDefinition" => todo!(),
        "ModifierDefinition" => todo!(),
        "StructDefinition" => todo!(),
        "EnumDefinition" => todo!(),
        "UserDefinedValueTypeDefinition" => todo!(),
        "PragmaDirective" => todo!(),
        "ImportDirective" => todo!(),
        "UsingForDirective" => todo!(),
        "SourceUnit" => todo!(),
        "InheritanceSpecifier" => todo!(),
        "ElementaryTypeName" => todo!(),
        "FunctionTypeName" => todo!(),
        "ParameterList" => todo!(),
        "TryCatchClause" => todo!(),
        "ModifierInvocation" => todo!(),
        "UserDefinedTypeName" => todo!(),
        "ArrayTypeName" => todo!(),
        "Mapping" => todo!(),
        "StructuredDocumentation" => todo!(),
        // Other node type
        _ => todo!(),
    }
}

impl ASTNode {
    // The foundry parser does not parse IdentifierPath nodes
    pub fn try_from_value(value: &serde_json::Value) -> Result<Self, String> {
        // Create a temporary Node-like structure from the Value
        let node_type_str = value
            .get("nodeType")
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("Missing nodeType field: {:?}", value))?;

        // Handle node type conversion
        let node_type = match node_type_str {
            "IdentifierPath" => NodeType::Other("IdentifierPath".to_string()),
            "StructuredDocumentation" => NodeType::Other("StructuredDocumentation".to_string()),
            _ => {
                // Try to parse as standard NodeType, but fall back to Other if needed
                serde_json::from_str(&format!("\"{}\"", node_type_str))
                    .unwrap_or_else(|_| NodeType::Other(node_type_str.to_string()))
            }
        };

        let node_id = value.get("id").and_then(|v| v.as_i64()).unwrap_or(0) as i32;

        let src_location = value
            .get("src")
            .and_then(|v| v.as_str())
            .and_then(|s| SourceLocation::from_str(s).ok())
            .unwrap_or(SourceLocation {
                start: None,
                length: None,
                index: None,
            });

        // Handle the conversion based on node type
        match node_type {
            NodeType::Other(ref node_type_str) if node_type_str == "IdentifierPath" => {
                let name = value
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("IdentifierPath missing name: {:?}", value))?
                    .to_string();

                let name_locations: Result<Vec<SourceLocation>, String> = value
                    .get("nameLocations")
                    .and_then(|v| v.as_array())
                    .ok_or_else(|| format!("IdentifierPath missing nameLocations: {:?}", value))?
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .ok_or_else(|| "Invalid nameLocation".to_string())
                            .and_then(|s| SourceLocation::from_str(s).map_err(|e| e.to_string()))
                    })
                    .collect();

                let referenced_declaration = value
                    .get("referencedDeclaration")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0) as i32;

                Ok(ASTNode::IdentifierPath {
                    node_id,
                    src_location,
                    name,
                    name_locations: name_locations?,
                    referenced_declaration,
                })
            }
            NodeType::Other(ref node_type_str) if node_type_str == "StructuredDocumentation" => {
                let text = value
                    .get("text")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();

                Ok(ASTNode::StructuredDocumentation {
                    node_id,
                    src_location,
                    text,
                })
            }
            _ => {
                // For other node types, fall back to the Node conversion
                let node: Node = serde_json::from_value(value.clone())
                    .map_err(|e| format!("Failed to convert Value to Node: {}", e))?;
                ASTNode::try_from_node(&node)
            }
        }
    }

    pub fn try_from_node(node: &Node) -> Result<Self, String> {
        let node_id = node.id.map(|id| id as i32).unwrap_or(0);
        let src_location = SourceLocation::from(&node.src);
        let nodes: Result<Vec<ASTNode>, String> = node
            .nodes
            .iter()
            .map(|n| ASTNode::try_from_node(n))
            .collect();
        let nodes = nodes?;
        let body = if let Some(b) = &node.body {
            Some(Box::new(ASTNode::try_from_node(b.as_ref())?))
        } else {
            None
        };

        Ok(match node.node_type {
            NodeType::Assignment => {
                let operator_str = node
                    .other
                    .get("operator")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("Assignment missing operator: {:?}", node.other))?;
                let operator = AssignmentOperator::from_str(operator_str)
                    .map_err(|e| format!("Invalid assignment operator: {}", e))?;

                let left_hand_side = node
                    .other
                    .get("leftHandSide")
                    .ok_or_else(|| format!("Assignment missing leftExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid leftExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let right_and_side = node
                    .other
                    .get("rightHandSide")
                    .ok_or_else(|| format!("Assignment missing rightExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid rightExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::Assignment {
                    node_id,
                    src_location,
                    operator,
                    right_and_side,
                    left_hand_side,
                }
            }
            NodeType::BinaryOperation => {
                let left_expression = node
                    .other
                    .get("leftExpression")
                    .ok_or_else(|| {
                        format!("BinaryOperation missing leftExpression: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid leftExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let operator_str = node
                    .other
                    .get("operator")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("BinaryOperation missing operator: {:?}", node.other))?;
                let operator = BinaryOperator::from_str(operator_str)
                    .map_err(|e| format!("Invalid binary operator: {}", e))?;

                let right_expression = node
                    .other
                    .get("rightExpression")
                    .ok_or_else(|| {
                        format!("BinaryOperation missing rightExpression: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid rightExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let type_descriptions = node
                    .other
                    .get("typeDescriptions")
                    .ok_or_else(|| {
                        format!("BinaryOperation missing typeDescriptions: {:?}", node.other)
                    })
                    .and_then(|v| TypeDescriptions::from_json(v))?;

                ASTNode::BinaryOperation {
                    node_id,
                    src_location,
                    left_expression,
                    operator,
                    right_expression,
                    type_descriptions,
                }
            }
            NodeType::Conditional => {
                let condition = node
                    .other
                    .get("condition")
                    .ok_or_else(|| format!("Conditional missing condition: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid condition: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let true_expression = node
                    .other
                    .get("trueExpression")
                    .ok_or_else(|| format!("Conditional missing trueExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid trueExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let false_expression = node
                    .other
                    .get("falseExpression")
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid falseExpression: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))
                            .unwrap()
                    })
                    .map(|v| Option::Some(v))
                    .unwrap_or(Option::None);

                ASTNode::Conditional {
                    node_id,
                    src_location,
                    condition,
                    true_expression,
                    false_expression,
                }
            }
            NodeType::ElementaryTypeNameExpression => {
                let argument_types = node
                    .other
                    .get("argumentTypes")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| ArgumentType::from_json(v).ok())
                            .collect()
                    })
                    .unwrap_or_default();
                let type_descriptions = node
                    .other
                    .get("typeDescriptions")
                    .ok_or_else(|| {
                        "ElementaryTypeNameExpression missing typeDescriptions".to_string()
                    })
                    .and_then(|v| TypeDescriptions::from_json(v))?;
                let type_name = node
                    .other
                    .get("typeName")
                    .ok_or_else(|| {
                        format!(
                            "ElementaryTypeNameExpression missing typeName: {:?}",
                            node.other
                        )
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid typeName: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::ElementaryTypeNameExpression {
                    node_id,
                    src_location,
                    argument_types,
                    type_descriptions,
                    type_name,
                }
            }
            NodeType::FunctionCall => {
                let empty_vec = vec![];
                let arguments_json = node
                    .other
                    .get("arguments")
                    .and_then(|v| v.as_array())
                    .unwrap_or(&empty_vec);
                let arguments: Result<Vec<ASTNode>, String> = arguments_json
                    .iter()
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid argument: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n))
                    })
                    .collect();
                let arguments = arguments?;

                let expression = node
                    .other
                    .get("expression")
                    .ok_or_else(|| format!("FunctionCall missing expression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid expression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let kind_str = node
                    .other
                    .get("kind")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("FunctionCall missing kind: {:?}", node.other))?;
                let kind = FunctionCallKind::from_str(kind_str)
                    .map_err(|e| format!("Invalid function call kind: {}", e))?;

                let name_locations = node
                    .other
                    .get("nameLocations")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                v.as_str().and_then(|s| SourceLocation::from_str(s).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                let names = node
                    .other
                    .get("names")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| v.as_str().map(|s| s.to_string()))
                            .collect()
                    })
                    .unwrap_or_default();

                let try_call = node
                    .other
                    .get("tryCall")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| format!("FunctionCall missing tryCall: {:?}", node.other))?;

                let type_descriptions = node
                    .other
                    .get("typeDescriptions")
                    .ok_or_else(|| {
                        format!("FunctionCall missing typeDescriptions: {:?}", node.other)
                    })
                    .and_then(|v| TypeDescriptions::from_json(v))?;

                ASTNode::FunctionCall {
                    node_id,
                    src_location,
                    arguments,
                    expression,
                    kind,
                    name_locations,
                    names,
                    try_call,
                    type_descriptions,
                }
            }
            NodeType::FunctionCallOptions => {
                let expression = node
                    .other
                    .get("expression")
                    .ok_or_else(|| {
                        format!("FunctionCallOptions missing expression: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid expression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let options = node
                    .other
                    .get("options")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ASTNode::FunctionCallOptions {
                    node_id,
                    src_location,
                    expression,
                    options,
                }
            }
            NodeType::Identifier => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("Identifier missing name: {:?}", node.other))?
                    .to_string();

                let overloaded_declarations = node
                    .other
                    .get("overloadedDeclarations")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| v.as_i64().map(|n| n as i32))
                            .collect()
                    })
                    .unwrap_or_default();

                let referenced_declaration = node
                    .other
                    .get("referencedDeclaration")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| {
                        format!("Identifier missing referencedDeclaration {:?}", node.other)
                    })? as i32;

                let type_descriptions = node
                    .other
                    .get("typeDescriptions")
                    .ok_or_else(|| format!("Identifier missing typeDescriptions: {:?}", node.other))
                    .and_then(|v| TypeDescriptions::from_json(v))?;

                ASTNode::Identifier {
                    node_id,
                    src_location,
                    name,
                    overloaded_declarations,
                    referenced_declaration,
                    type_descriptions,
                }
            }
            NodeType::Other(ref node_type_str) if node_type_str == "IdentifierPath" => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("IdentifierPath missing name: {:?}", node.other))?
                    .to_string();

                let name_locations: Result<Vec<SourceLocation>, String> = node
                    .other
                    .get("nameLocations")
                    .and_then(|v| v.as_array())
                    .ok_or_else(|| {
                        format!("IdentifierPath missing nameLocations: {:?}", node.other)
                    })?
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .ok_or_else(|| format!("Invalid nameLocation: {:?}", node.other))
                            .and_then(|s| {
                                SourceLocation::from_str(s).map_err(|e| {
                                    format!(
                                        "Invalid nameLocation format: {}, node: {:?}",
                                        e, node.other
                                    )
                                })
                            })
                    })
                    .collect();

                let name_locations = name_locations?;

                let referenced_declaration = node
                    .other
                    .get("referencedDeclaration")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| {
                        format!(
                            "IdentifierPath missing referencedDeclaration: {:?}",
                            node.other
                        )
                    })? as i32;

                ASTNode::IdentifierPath {
                    node_id,
                    src_location,
                    name,
                    name_locations,
                    referenced_declaration,
                }
            }
            NodeType::IndexAccess => {
                let base_expression = node
                    .other
                    .get("baseExpression")
                    .ok_or_else(|| format!("IndexAccess missing baseExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid baseExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let index_expression = node
                    .other
                    .get("indexExpression")
                    .ok_or_else(|| format!("IndexAccess missing indexExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid indexExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::IndexAccess {
                    node_id,
                    src_location,
                    base_expression,
                    index_expression,
                }
            }
            NodeType::IndexRangeAccess => panic!("IndexRangeAccess not implemented"),
            NodeType::Literal => {
                let hex_value = node
                    .other
                    .get("hexValue")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("Literal missing hexValue: {:?}", node.other))?
                    .to_string();
                let kind_str = node
                    .other
                    .get("kind")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("Literal missing kind: {:?}", node.other))?;
                let kind = LiteralKind::from_str(kind_str)
                    .map_err(|e| format!("Invalid literal kind: {}", e))?;
                let type_descriptions = node
                    .other
                    .get("typeDescriptions")
                    .ok_or_else(|| format!("Literal missing typeDescriptions: {:?}", node.other))
                    .and_then(|v| TypeDescriptions::from_json(v))?;
                let value = node
                    .other
                    .get("value")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("Literal missing value: {:?}", node.other))?
                    .to_string();

                ASTNode::Literal {
                    node_id,
                    src_location,
                    hex_value,
                    kind,
                    type_descriptions,
                    value,
                }
            }
            NodeType::MemberAccess => {
                let expression = node
                    .other
                    .get("expression")
                    .ok_or_else(|| format!("MemberAccess missing expression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid expression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let member_name = node
                    .other
                    .get("memberName")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("MemberAccess missing memberName: {:?}", node.other))?
                    .to_string();

                let member_location = node
                    .other
                    .get("memberLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                ASTNode::MemberAccess {
                    node_id,
                    src_location,
                    expression,
                    member_location,
                    member_name,
                }
            }
            NodeType::NewExpression => ASTNode::NewExpression {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::TupleExpression => {
                let components = node
                    .other
                    .get("components")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ASTNode::TupleExpression {
                    node_id,
                    src_location,
                    components,
                }
            }
            NodeType::UnaryOperation => {
                let prefix = node
                    .other
                    .get("prefix")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(true);

                let operator_str = node
                    .other
                    .get("operator")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("UnaryOperation missing operator: {:?}", node.other))?;
                let operator = UnaryOperator::from_str(operator_str)
                    .map_err(|e| format!("Invalid unary operator: {}", e))?;

                let sub_expression = node
                    .other
                    .get("subExpression")
                    .ok_or_else(|| {
                        format!("UnaryOperation missing subExpression: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid subExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::UnaryOperation {
                    node_id,
                    src_location,
                    prefix,
                    operator,
                    sub_expression,
                }
            }
            NodeType::Block => {
                let empty_vec = vec![];
                let statements_json = node
                    .other
                    .get("statements")
                    .and_then(|v| v.as_array())
                    .unwrap_or(&empty_vec);
                let statements: Result<Vec<ASTNode>, String> = statements_json
                    .iter()
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid statement in Block: {:?}", node.other))
                    })
                    .map(|node_result| node_result.and_then(|n| ASTNode::try_from_node(&n)))
                    .collect();
                let statements = statements?;

                ASTNode::Block {
                    node_id,
                    src_location,
                    statements,
                }
            }
            NodeType::Break => ASTNode::Break {
                node_id,
                src_location,
            },
            NodeType::Continue => ASTNode::Continue {
                node_id,
                src_location,
            },
            NodeType::DoWhileStatement => ASTNode::DoWhileStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::EmitStatement => {
                let event_call = node
                    .other
                    .get("eventCall")
                    .ok_or_else(|| format!("EmitStatement missing eventCall: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid eventCall: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::EmitStatement {
                    node_id,
                    src_location,
                    event_call,
                }
            }
            NodeType::ExpressionStatement => {
                let expression = node
                    .other
                    .get("expression")
                    .ok_or_else(|| {
                        format!("ExpressionStatement missing expression: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid expression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::ExpressionStatement {
                    node_id,
                    src_location,
                    expression,
                }
            }
            NodeType::ForStatement => {
                let body = match body {
                    Option::Some(b) => Ok(b),
                    Option::None => Err(format!("ForStatement missing body: {:?}", node.other)),
                }?;

                let condition = node
                    .other
                    .get("condition")
                    .ok_or_else(|| format!("ForStatement missing condition: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid condition: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let initialization_expression = node
                    .other
                    .get("initializationExpression")
                    .ok_or_else(|| {
                        format!(
                            "ForStatement missing initializationExpression: {:?}",
                            node.other
                        )
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone()).map_err(|_| {
                            format!("Invalid initializationExpression: {:?}", node.other)
                        })
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let loop_expression = node
                    .other
                    .get("loopExpression")
                    .ok_or_else(|| format!("ForStatement missing loopExpression: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid loopExpression: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let is_simple_counter_loop = node
                    .other
                    .get("isSimpleCounterLoop")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);

                ASTNode::ForStatement {
                    node_id,
                    src_location,
                    body,
                    condition,
                    initialization_expression,
                    is_simple_counter_loop,
                    loop_expression,
                }
            }
            NodeType::IfStatement => {
                let condition = node
                    .other
                    .get("condition")
                    .ok_or_else(|| format!("IfStatement missing condition: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid condition: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let true_body = node
                    .other
                    .get("trueBody")
                    .ok_or_else(|| format!("IfStatement missing trueBody: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid trueBody: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let false_body = node
                    .other
                    .get("falseBody")
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid falseBody: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))
                    })
                    .transpose()?;

                ASTNode::IfStatement {
                    node_id,
                    src_location,
                    condition,
                    true_body,
                    false_body,
                }
            }
            NodeType::InlineAssembly => ASTNode::InlineAssembly {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::PlaceholderStatement => ASTNode::PlaceholderStatement {
                node_id,
                src_location,
            },
            NodeType::Return => {
                let expression = node
                    .other
                    .get("expression")
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| "Invalid expression".to_string())
                            .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))
                    })
                    .transpose()?;

                let function_return_parameters = node
                    .other
                    .get("functionReturnParameters")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0) as i32;

                ASTNode::Return {
                    node_id,
                    src_location,
                    expression,
                    function_return_parameters,
                }
            }
            NodeType::RevertStatement => {
                let error_call = node
                    .other
                    .get("errorCall")
                    .ok_or_else(|| format!("RevertStatement missing errorCall: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid errorCall: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::RevertStatement {
                    node_id,
                    src_location,
                    error_call,
                }
            }
            NodeType::TryStatement => ASTNode::TryStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::UncheckedBlock => ASTNode::UncheckedBlock {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::VariableDeclarationStatement => {
                let declarations = node
                    .other
                    .get("declarations")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                let initial_value = node
                    .other
                    .get("initialValue")
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid initialValue: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))
                    })
                    .transpose()?;

                ASTNode::VariableDeclarationStatement {
                    node_id,
                    src_location,
                    declarations,
                    initial_value,
                }
            }
            NodeType::VariableDeclaration => {
                let constant = node
                    .other
                    .get("constant")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!("VariableDeclaration missing constant: {:?}", node.other)
                    })?;
                let function_selector = node
                    .other
                    .get("functionSelector")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());
                let mutability_str = node
                    .other
                    .get("mutability")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("VariableDeclaration missing mutability: {:?}", node.other)
                    })?;
                let mutability = VariableMutability::from_str(mutability_str)
                    .map_err(|e| format!("Invalid mutability: {}", e))?;
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("VariableDeclaration missing name: {:?}", node.other))?
                    .to_string();
                let name_location_str = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("VariableDeclaration missing nameLocation: {:?}", node.other)
                    })?;
                let name_location = SourceLocation::from_str(name_location_str)
                    .map_err(|e| format!("Invalid nameLocation: {}", e))?;
                let scope = node
                    .other
                    .get("scope")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| format!("VariableDeclaration missing scope: {:?}", node.other))?
                    as i32;
                let state_variable = node
                    .other
                    .get("stateVariable")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!(
                            "VariableDeclaration missing stateVariable: {:?}",
                            node.other
                        )
                    })?;
                let storage_location_str = node
                    .other
                    .get("storageLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!(
                            "VariableDeclaration missing storageLocation: {:?}",
                            node.other
                        )
                    })?;
                let storage_location = StorageLocation::from_str(storage_location_str)
                    .map_err(|e| format!("Invalid storageLocation: {}", e))?;
                let type_name = node
                    .other
                    .get("typeName")
                    .ok_or_else(|| {
                        format!("VariableDeclaration missing typeName: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid typeName: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let value = node
                    .other
                    .get("value")
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid value: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))
                    })
                    .transpose()?;
                let visibility_str = node
                    .other
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("VariableDeclaration missing visibility: {:?}", node.other)
                    })?;
                let visibility = VariableVisibility::from_str(visibility_str)
                    .map_err(|e| format!("Invalid visibility: {}", e))?;

                ASTNode::VariableDeclaration {
                    node_id,
                    src_location,
                    constant,
                    function_selector,
                    mutability,
                    name,
                    name_location,
                    scope,
                    state_variable,
                    storage_location,
                    type_name,
                    value,
                    visibility,
                }
            }
            NodeType::WhileStatement => ASTNode::WhileStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::YulAssignment => return Err("Yul nodes not supported".to_string()),
            NodeType::YulBlock => return Err("Yul nodes not supported".to_string()),
            NodeType::YulBreak => return Err("Yul nodes not supported".to_string()),
            NodeType::YulCase => return Err("Yul nodes not supported".to_string()),
            NodeType::YulContinue => return Err("Yul nodes not supported".to_string()),
            NodeType::YulExpressionStatement => return Err("Yul nodes not supported".to_string()),
            NodeType::YulLeave => return Err("Yul nodes not supported".to_string()),
            NodeType::YulForLoop => return Err("Yul nodes not supported".to_string()),
            NodeType::YulFunctionDefinition => return Err("Yul nodes not supported".to_string()),
            NodeType::YulIf => return Err("Yul nodes not supported".to_string()),
            NodeType::YulSwitch => return Err("Yul nodes not supported".to_string()),
            NodeType::YulVariableDeclaration => return Err("Yul nodes not supported".to_string()),
            NodeType::YulFunctionCall => return Err("Yul nodes not supported".to_string()),
            NodeType::YulIdentifier => return Err("Yul nodes not supported".to_string()),
            NodeType::YulLiteral => return Err("Yul nodes not supported".to_string()),
            NodeType::YulLiteralValue => return Err("Yul nodes not supported".to_string()),
            NodeType::YulHexValue => return Err("Yul nodes not supported".to_string()),
            NodeType::YulTypedName => return Err("Yul nodes not supported".to_string()),
            NodeType::ContractDefinition => {
                let abstract_ = node
                    .other
                    .get("abstract")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!("ContractDefinition missing abstract: {:?}", node.other)
                    })?;

                let empty_vec = vec![];
                let base_contracts_json = node
                    .other
                    .get("baseContracts")
                    .and_then(|v| v.as_array())
                    .unwrap_or(&empty_vec);
                let base_contracts: Result<Vec<ASTNode>, String> = base_contracts_json
                    .iter()
                    .map(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid baseContract: {:?}", node.other))
                            .and_then(|n| ASTNode::try_from_node(&n))
                    })
                    .collect();
                let base_contracts = base_contracts?;

                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("ContractDefinition missing name: {:?}", node.other))?
                    .to_string();

                let name_location_str = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("ContractDefinition missing nameLocation: {:?}", node.other)
                    })?;
                let name_location = SourceLocation::from_str(name_location_str)
                    .map_err(|e| format!("Invalid nameLocation: {}", e))?;

                let contract_kind_str = node
                    .other
                    .get("contractKind")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("ContractDefinition missing contractKind: {:?}", node.other)
                    })?;
                let contract_kind = ContractKind::from_str(contract_kind_str)
                    .map_err(|e| format!("Invalid contract kind: {}", e))?;

                ASTNode::ContractDefinition {
                    node_id,
                    src_location,
                    nodes,
                    abstract_,
                    base_contracts,
                    name,
                    name_location,
                    contract_kind,
                }
            }
            NodeType::FunctionDefinition => {
                let implemented = node
                    .other
                    .get("implemented")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!("FunctionDefinition missing implemented: {:?}", node.other)
                    })?;
                let kind_str = node
                    .other
                    .get("kind")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("FunctionDefinition missing kind: {:?}", node.other))?;
                let kind = FunctionKind::from_str(kind_str)
                    .map_err(|e| format!("Invalid function kind: {}", e))?;
                let modifiers = node
                    .other
                    .get("modifiers")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                // Convert JSON modifier to Node and then to ASTNode
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("FunctionDefinition missing name: {:?}", node.other))?
                    .to_string();
                let name_location_str = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("FunctionDefinition missing nameLocation: {:?}", node.other)
                    })?;
                let name_location = SourceLocation::from_str(name_location_str)
                    .map_err(|e| format!("Invalid nameLocation: {}", e))?;
                let parameters = node
                    .other
                    .get("parameters")
                    .ok_or_else(|| {
                        format!("FunctionDefinition missing parameters: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid parameters: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let return_parameters = node
                    .other
                    .get("returnParameters")
                    .ok_or_else(|| {
                        format!(
                            "FunctionDefinition missing returnParameters: {:?}",
                            node.other
                        )
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid returnParameters: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let scope = node
                    .other
                    .get("scope")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| format!("FunctionDefinition missing scope: {:?}", node.other))?
                    as i32;
                let state_mutability_str = node
                    .other
                    .get("stateMutability")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!(
                            "FunctionDefinition missing stateMutability: {:?}",
                            node.other
                        )
                    })?;
                let state_mutability = FunctionStateMutability::from_str(state_mutability_str)
                    .map_err(|e| format!("Invalid state mutability: {}", e))?;
                let virtual_ = node
                    .other
                    .get("virtual")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!("FunctionDefinition missing virtual: {:?}", node.other)
                    })?;
                let visibility_str = node
                    .other
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("FunctionDefinition missing visibility: {:?}", node.other)
                    })?;
                let visibility = FunctionVisibility::from_str(visibility_str)
                    .map_err(|e| format!("Invalid visibility: {}", e))?;
                let documentation = node
                    .other
                    .get("documentation")
                    .map(|d| {
                        ASTNode::try_from_value(d)
                            .map_err(|e| format!("Invalid documentation: {}", d))
                            .map(Box::new)
                    })
                    .transpose()?;

                ASTNode::FunctionDefinition {
                    node_id,
                    src_location,
                    body,
                    documentation,
                    implemented,
                    kind,
                    modifiers,
                    name,
                    name_location,
                    parameters,
                    return_parameters,
                    scope,
                    state_mutability,
                    virtual_,
                    visibility,
                }
            }
            NodeType::EventDefinition => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("EventDefinition missing name: {:?}", node.other))?
                    .to_string();

                let name_location = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                let parameters = node
                    .other
                    .get("parameters")
                    .ok_or_else(|| format!("EventDefinition missing parameters: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid parameters: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::EventDefinition {
                    node_id,
                    src_location,
                    name,
                    name_location,
                    parameters,
                }
            }
            NodeType::ErrorDefinition => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("ErrorDefinition missing name: {:?}", node.other))?
                    .to_string();

                let name_location = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                let parameters = node
                    .other
                    .get("parameters")
                    .ok_or_else(|| format!("ParameterList missing parameters: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid parameters: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::ErrorDefinition {
                    node_id,
                    src_location,
                    name,
                    name_location,
                    parameters,
                }
            }
            NodeType::ModifierDefinition => {
                let body = body
                    .ok_or_else(|| format!("ModifierDefinition missing body: {:?}", node.other))?;
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("ModifierDefinition missing name: {:?}", node.other))?
                    .to_string();
                let name_location_str = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("ModifierDefinition missing nameLocation: {:?}", node.other)
                    })?;
                let name_location = SourceLocation::from_str(name_location_str)
                    .map_err(|e| format!("Invalid nameLocation: {}", e))?;
                let parameters = node
                    .other
                    .get("parameters")
                    .ok_or_else(|| {
                        format!("ModifierDefinition missing parameters: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid parameters: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let virtual_ = node
                    .other
                    .get("virtual")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| {
                        format!("ModifierDefinition missing virtual: {:?}", node.other)
                    })?;
                let visibility_str = node
                    .other
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        format!("ModifierDefinition missing visibility: {:?}", node.other)
                    })?;
                let visibility = FunctionVisibility::from_str(visibility_str)
                    .map_err(|e| format!("Invalid visibility: {}", e))?;
                let documentation = node.other.get("documentation").and_then(|v| {
                    serde_json::from_value::<Node>(v.clone())
                        .ok()
                        .and_then(|n| ASTNode::try_from_node(&n).map(Box::new).ok())
                });

                ASTNode::ModifierDefinition {
                    node_id,
                    src_location,
                    body,
                    documentation,
                    name,
                    name_location,
                    parameters,
                    virtual_,
                    visibility,
                }
            }
            NodeType::StructDefinition => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("StructDefinition missing name: {:?}", node.other))?
                    .to_string();

                let name_location = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                let canonical_name = node
                    .other
                    .get("canonicalName")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| name.clone());

                let visibility = node
                    .other
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .map(|s| VariableVisibility::from_str(s).unwrap_or(VariableVisibility::Public))
                    .unwrap_or(VariableVisibility::Public);

                let members = node
                    .other
                    .get("members")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ASTNode::StructDefinition {
                    node_id,
                    src_location,
                    members,
                    canonical_name,
                    name,
                    name_location,
                    visibility,
                }
            }
            NodeType::EnumDefinition => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("EnumDefinition missing name: {:?}", node.other))?
                    .to_string();

                let name_location = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                let canonical_name = node
                    .other
                    .get("canonicalName")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| name.clone());

                let members = node
                    .other
                    .get("members")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ASTNode::EnumDefinition {
                    node_id,
                    src_location,
                    members,
                    canonical_name,
                    name,
                    name_location,
                }
            }
            NodeType::UserDefinedValueTypeDefinition => ASTNode::UserDefinedValueTypeDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::PragmaDirective => {
                let literals = node
                    .other
                    .get("literals")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().map(|v| v.to_string()).collect())
                    .unwrap_or_default();

                ASTNode::PragmaDirective {
                    node_id,
                    src_location,
                    literals,
                }
            }
            NodeType::ImportDirective => {
                let absolute_path = node
                    .other
                    .get("absolutePath")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();

                let file = node
                    .other
                    .get("file")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();

                let source_unit = node
                    .other
                    .get("sourceUnit")
                    .and_then(|v| v.as_i64())
                    .map(|n| n as i32)
                    .unwrap_or(0);

                ASTNode::ImportDirective {
                    node_id,
                    src_location,
                    absolute_path,
                    file,
                    source_unit,
                }
            }
            NodeType::UsingForDirective => {
                let global = node
                    .other
                    .get("global")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| format!("UsingForDirective missing global: {:?}", node.other))?;

                let library_name = node
                    .other
                    .get("libraryName")
                    .ok_or_else(|| {
                        format!("UsingForDirective missing libraryName: {:?}", node.other)
                    })
                    .and_then(|v| {
                        ASTNode::try_from_value(v).map_err(|e| format!("Invalid pathNode: {}", e))
                    })
                    .map(Box::new)?;

                let type_name = node
                    .other
                    .get("typeName")
                    .ok_or_else(|| format!("UsingForDirective missing typeName: {:?}", node.other))
                    .and_then(|v| {
                        ASTNode::try_from_value(v).map_err(|e| format!("Invalid pathNode: {}", e))
                    })
                    .map(Box::new)?;

                ASTNode::UsingForDirective {
                    node_id,
                    src_location,
                    global,
                    library_name,
                    type_name,
                }
            }
            NodeType::SourceUnit => ASTNode::SourceUnit {
                node_id,
                src_location,
                nodes,
            },
            NodeType::InheritanceSpecifier => {
                let base_name = node
                    .other
                    .get("baseName")
                    .ok_or_else(|| {
                        format!("InheritanceSpecifier missing baseName: {:?}", node.other)
                    })
                    .and_then(|v| {
                        ASTNode::try_from_value(v).map_err(|e| format!("Invalid baseName: {}", e))
                    })
                    .map(Box::new)?;

                ASTNode::InheritanceSpecifier {
                    node_id,
                    src_location,
                    base_name,
                }
            }
            NodeType::ElementaryTypeName => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| format!("ElementaryTypeName missing name: {:?}", node.other))?
                    .to_string();

                ASTNode::ElementaryTypeName {
                    node_id,
                    src_location,
                    name,
                }
            }
            NodeType::FunctionTypeName => ASTNode::FunctionTypeName {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ParameterList => {
                let parameters = node
                    .other
                    .get("parameters")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| {
                                serde_json::from_value::<Node>(v.clone())
                                    .ok()
                                    .and_then(|n| ASTNode::try_from_node(&n).ok())
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ASTNode::ParameterList {
                    node_id,
                    src_location,
                    parameters,
                }
            }
            NodeType::TryCatchClause => ASTNode::TryCatchClause {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ModifierInvocation => {
                let modifier_name = node
                    .other
                    .get("modifierName")
                    .ok_or_else(|| {
                        format!("ModifierInvocation missing modifierName: {:?}", node.other)
                    })
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid modifier_name: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let arguments = node
                    .other
                    .get("arguments")
                    .and_then(|v| v.as_array())
                    .map(|v| {
                        v.iter()
                            .map(|i| {
                                serde_json::from_value::<Node>(i.clone())
                                    .map_err(|_| format!("Invalid argument: {:?}", node.other))
                                    .and_then(|n| ASTNode::try_from_node(&n))
                            })
                            .collect()
                    })
                    .transpose()?;

                ASTNode::ModifierInvocation {
                    node_id,
                    src_location,
                    modifier_name,
                    arguments,
                }
            }
            NodeType::UserDefinedTypeName => {
                let path_node = node
                    .other
                    .get("pathNode")
                    .ok_or_else(|| {
                        format!("UserDefinedTypeName missing pathNode: {:?}", node.other)
                    })
                    .and_then(|v| {
                        ASTNode::try_from_value(v).map_err(|e| format!("Invalid pathNode: {}", e))
                    })
                    .map(Box::new)?;

                let referenced_declaration = node
                    .other
                    .get("referencedDeclaration")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0) as i32;

                ASTNode::UserDefinedTypeName {
                    node_id,
                    src_location,
                    path_node,
                    referenced_declaration,
                }
            }
            NodeType::ArrayTypeName => {
                let base_type = node
                    .other
                    .get("baseType")
                    .ok_or_else(|| format!("ArrayTypeName missing baseType: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid baseType: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                ASTNode::ArrayTypeName {
                    node_id,
                    src_location,
                    base_type,
                }
            }
            NodeType::Mapping => {
                let key_type = node
                    .other
                    .get("keyType")
                    .ok_or_else(|| format!("Mapping missing keyType: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid keyType: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let value_type = node
                    .other
                    .get("valueType")
                    .ok_or_else(|| format!("Mapping missing valueType: {:?}", node.other))
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| format!("Invalid valueType: {:?}", node.other))
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;

                let key_name = node
                    .other
                    .get("keyName")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());

                let value_name = node
                    .other
                    .get("valueName")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());

                let key_name_location = node
                    .other
                    .get("keyNameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                let value_name_location = node
                    .other
                    .get("valueNameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                ASTNode::Mapping {
                    node_id,
                    src_location,
                    key_name,
                    key_name_location,
                    key_type,
                    value_name,
                    value_name_location,
                    value_type,
                }
            }
            NodeType::Other(ref node_type_str) if node_type_str == "StructuredDocumentation" => {
                let text = node
                    .other
                    .get("text")
                    .map(|s| s.to_string())
                    .unwrap_or_default();

                ASTNode::StructuredDocumentation {
                    node_id,
                    src_location,
                    text,
                }
            }
            NodeType::Other(ref node_type_str) if node_type_str == "EnumValue" => {
                let name = node
                    .other
                    .get("name")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_default();

                let name_location = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .map(|s| SourceLocation::from_str(s).unwrap_or_default())
                    .unwrap_or_default();

                ASTNode::EnumValue {
                    node_id,
                    src_location,
                    name,
                    name_location,
                }
            }
            NodeType::Other(ref node_type_str) => ASTNode::Other {
                node_id,
                src_location,
                nodes,
                body,
                node_type: node_type_str.clone(),
            },
        })
    }
}

impl From<&Node> for ASTNode {
    fn from(node: &Node) -> Self {
        ASTNode::try_from_node(node).unwrap_or_else(|_| ASTNode::Other {
            node_id: node.id.map(|id| id as i32).unwrap_or(0),
            src_location: SourceLocation::from(&node.src),
            nodes: node
                .nodes
                .iter()
                .filter_map(|n| ASTNode::try_from_node(n).ok())
                .collect(),
            body: node
                .body
                .as_ref()
                .and_then(|b| ASTNode::try_from_node(b.as_ref()).ok().map(Box::new)),
            node_type: format!("{:?}", node.node_type),
        })
    }
}

/// Recursively traverses an AST starting from the given node, applying a closure to each node.
///
/// This function performs a depth-first traversal of the AST, visiting the current node first,
/// then recursively visiting all child nodes obtained through the `nodes()` method.
///
/// # Arguments
/// * `node` - The root AST node to start traversal from
/// * `accumulator` - The initial accumulator value
/// * `f` - A closure that takes an ASTNode reference and the current accumulator, returning a new accumulator
///
/// # Returns
/// The final accumulator value after traversing all nodes
///
/// # Examples
///
/// Count the total number of nodes in an AST:
/// ```rust
/// # use crate::solidity::parser::{traverse_nodes, ASTNode};
/// let count = traverse_nodes(&ast_node, 0, |_node, acc| acc + 1);
/// ```
///
/// Collect all function names in a contract:
/// ```rust
/// # use crate::solidity::parser::{traverse_nodes, ASTNode};
/// let function_names = traverse_nodes(&contract_node, Vec::new(), |node, mut acc| {
///     if let ASTNode::FunctionDefinition { name, .. } = node {
///         acc.push(name.clone());
///     }
///     acc
/// });
/// ```
///
/// Find the maximum nesting depth of blocks:
/// ```rust
/// # use crate::solidity::parser::{traverse_nodes, ASTNode};
/// let max_depth = traverse_nodes(&ast_node, (0, 0), |node, (current_depth, max_depth)| {
///     let new_depth = match node {
///         ASTNode::Block { .. } => current_depth + 1,
///         _ => current_depth,
///     };
///     (new_depth, max_depth.max(new_depth))
/// }).1;
/// ```
pub fn traverse_nodes<T, F>(node: &ASTNode, accumulator: T, mut f: F) -> T
where
    F: FnMut(&ASTNode, T) -> T,
{
    // Apply the closure to the current node
    let new_accumulator = f(node, accumulator);

    // Recursively traverse all child nodes
    node.nodes()
        .iter()
        .fold(new_accumulator, |acc, child_node| {
            traverse_nodes(child_node, acc, &mut f)
        })
}

pub fn ast_from_json_file(file_path: &str) -> Result<ASTNode, String> {
    let json =
        std::fs::read_to_string(file_path).map_err(|e| format!("Failed to read file: {}", e))?;
    todo!()
}
