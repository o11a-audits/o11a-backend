use foundry_compilers::artifacts::ast::{LowFidelitySourceLocation, Node};
use foundry_compilers_artifacts::NodeType;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub start: usize,
    pub length: Option<usize>,
    pub index: Option<usize>,
}

impl From<&LowFidelitySourceLocation> for SourceLocation {
    fn from(low_fidelity: &LowFidelitySourceLocation) -> Self {
        SourceLocation {
            start: low_fidelity.start,
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
            .parse::<usize>()
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

pub enum ContractVariableVisibility {
    Public,
    Private,
}

pub enum ContractVariableMutability {
    Constant,
    Mutable,
    Immutable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    // Expression nodes
    Assignment {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    BinaryOperation {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Conditional {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ElementaryTypeNameExpression {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    FunctionCall {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    FunctionCallOptions {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Identifier {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    IndexAccess {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    MemberAccess {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    UnaryOperation {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },

    // Statement nodes
    Block {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Break {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Continue {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ExpressionStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ForStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    IfStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Return {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    RevertStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    VariableDeclaration {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    WhileStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },

    // Yul nodes
    YulAssignment {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulBlock {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulBreak {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulCase {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulContinue {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulExpressionStatement {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulLeave {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulForLoop {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulFunctionDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulIf {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulSwitch {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulVariableDeclaration {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulFunctionCall {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulIdentifier {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulLiteral {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulLiteralValue {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulHexValue {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    YulTypedName {
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
        body: Option<Box<ASTNode>>,
    },
    FunctionDefinition {
        node_id: i32,
        src_location: SourceLocation,
        body: Box<ASTNode>,
        function_selector: String,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ErrorDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ModifierDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    StructDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    EnumDefinition {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ImportDirective {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    UsingForDirective {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },

    // Other nodes
    SourceUnit {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    InheritanceSpecifier {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ElementaryTypeName {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    UserDefinedTypeName {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    ArrayTypeName {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
    },
    Mapping {
        node_id: i32,
        src_location: SourceLocation,
        nodes: Vec<ASTNode>,
        body: Option<Box<ASTNode>>,
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
            ASTNode::IndexAccess { node_id, .. } => *node_id,
            ASTNode::IndexRangeAccess { node_id, .. } => *node_id,
            ASTNode::Literal { node_id, .. } => *node_id,
            ASTNode::MemberAccess { node_id, .. } => *node_id,
            ASTNode::NewExpression { node_id, .. } => *node_id,
            ASTNode::TupleExpression { node_id, .. } => *node_id,
            ASTNode::UnaryOperation { node_id, .. } => *node_id,
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
            ASTNode::YulAssignment { node_id, .. } => *node_id,
            ASTNode::YulBlock { node_id, .. } => *node_id,
            ASTNode::YulBreak { node_id, .. } => *node_id,
            ASTNode::YulCase { node_id, .. } => *node_id,
            ASTNode::YulContinue { node_id, .. } => *node_id,
            ASTNode::YulExpressionStatement { node_id, .. } => *node_id,
            ASTNode::YulLeave { node_id, .. } => *node_id,
            ASTNode::YulForLoop { node_id, .. } => *node_id,
            ASTNode::YulFunctionDefinition { node_id, .. } => *node_id,
            ASTNode::YulIf { node_id, .. } => *node_id,
            ASTNode::YulSwitch { node_id, .. } => *node_id,
            ASTNode::YulVariableDeclaration { node_id, .. } => *node_id,
            ASTNode::YulFunctionCall { node_id, .. } => *node_id,
            ASTNode::YulIdentifier { node_id, .. } => *node_id,
            ASTNode::YulLiteral { node_id, .. } => *node_id,
            ASTNode::YulLiteralValue { node_id, .. } => *node_id,
            ASTNode::YulHexValue { node_id, .. } => *node_id,
            ASTNode::YulTypedName { node_id, .. } => *node_id,
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
            ASTNode::IndexAccess { src_location, .. } => src_location,
            ASTNode::IndexRangeAccess { src_location, .. } => src_location,
            ASTNode::Literal { src_location, .. } => src_location,
            ASTNode::MemberAccess { src_location, .. } => src_location,
            ASTNode::NewExpression { src_location, .. } => src_location,
            ASTNode::TupleExpression { src_location, .. } => src_location,
            ASTNode::UnaryOperation { src_location, .. } => src_location,
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
            ASTNode::YulAssignment { src_location, .. } => src_location,
            ASTNode::YulBlock { src_location, .. } => src_location,
            ASTNode::YulBreak { src_location, .. } => src_location,
            ASTNode::YulCase { src_location, .. } => src_location,
            ASTNode::YulContinue { src_location, .. } => src_location,
            ASTNode::YulExpressionStatement { src_location, .. } => src_location,
            ASTNode::YulLeave { src_location, .. } => src_location,
            ASTNode::YulForLoop { src_location, .. } => src_location,
            ASTNode::YulFunctionDefinition { src_location, .. } => src_location,
            ASTNode::YulIf { src_location, .. } => src_location,
            ASTNode::YulSwitch { src_location, .. } => src_location,
            ASTNode::YulVariableDeclaration { src_location, .. } => src_location,
            ASTNode::YulFunctionCall { src_location, .. } => src_location,
            ASTNode::YulIdentifier { src_location, .. } => src_location,
            ASTNode::YulLiteral { src_location, .. } => src_location,
            ASTNode::YulLiteralValue { src_location, .. } => src_location,
            ASTNode::YulHexValue { src_location, .. } => src_location,
            ASTNode::YulTypedName { src_location, .. } => src_location,
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
            ASTNode::Other { src_location, .. } => src_location,
        }
    }
}

impl ASTNode {
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
            NodeType::Assignment => ASTNode::Assignment {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::BinaryOperation => ASTNode::BinaryOperation {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Conditional => ASTNode::Conditional {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ElementaryTypeNameExpression => ASTNode::ElementaryTypeNameExpression {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::FunctionCall => ASTNode::FunctionCall {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::FunctionCallOptions => ASTNode::FunctionCallOptions {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Identifier => ASTNode::Identifier {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::IndexAccess => ASTNode::IndexAccess {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::IndexRangeAccess => ASTNode::IndexRangeAccess {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Literal => ASTNode::Literal {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::MemberAccess => ASTNode::MemberAccess {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::NewExpression => ASTNode::NewExpression {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::TupleExpression => ASTNode::TupleExpression {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::UnaryOperation => ASTNode::UnaryOperation {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Block => ASTNode::Block {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Break => ASTNode::Break {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Continue => ASTNode::Continue {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::DoWhileStatement => ASTNode::DoWhileStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::EmitStatement => ASTNode::EmitStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ExpressionStatement => ASTNode::ExpressionStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ForStatement => ASTNode::ForStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::IfStatement => ASTNode::IfStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::InlineAssembly => ASTNode::InlineAssembly {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::PlaceholderStatement => ASTNode::PlaceholderStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Return => ASTNode::Return {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::RevertStatement => ASTNode::RevertStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
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
            NodeType::VariableDeclarationStatement => ASTNode::VariableDeclarationStatement {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::VariableDeclaration => ASTNode::VariableDeclaration {
                node_id,
                src_location,
                nodes,
                body,
            },
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
            NodeType::ContractDefinition => ASTNode::ContractDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::FunctionDefinition => {
                let body = body.ok_or_else(|| "FunctionDefinition missing body".to_string())?;
                let function_selector = node
                    .other
                    .get("functionSelector")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| "FunctionDefinition missing functionSelector".to_string())?
                    .to_string();
                let implemented = node
                    .other
                    .get("implemented")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| "FunctionDefinition missing implemented".to_string())?;
                let kind_str = node
                    .other
                    .get("kind")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| "FunctionDefinition missing kind".to_string())?;
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
                    .ok_or_else(|| "FunctionDefinition missing name".to_string())?
                    .to_string();
                let name_location_str = node
                    .other
                    .get("nameLocation")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| "FunctionDefinition missing nameLocation".to_string())?;
                let name_location = SourceLocation::from_str(name_location_str)
                    .map_err(|e| format!("Invalid nameLocation: {}", e))?;
                let parameters = node
                    .other
                    .get("parameters")
                    .ok_or_else(|| "FunctionDefinition missing parameters".to_string())
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| "Invalid parameters".to_string())
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let return_parameters = node
                    .other
                    .get("returnParameters")
                    .ok_or_else(|| "FunctionDefinition missing returnParameters".to_string())
                    .and_then(|v| {
                        serde_json::from_value::<Node>(v.clone())
                            .map_err(|_| "Invalid returnParameters".to_string())
                    })
                    .and_then(|n| ASTNode::try_from_node(&n).map(Box::new))?;
                let scope = node
                    .other
                    .get("scope")
                    .and_then(|v| v.as_u64())
                    .ok_or_else(|| "FunctionDefinition missing scope".to_string())?
                    as i32;
                let state_mutability_str = node
                    .other
                    .get("stateMutability")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| "FunctionDefinition missing stateMutability".to_string())?;
                let state_mutability = FunctionStateMutability::from_str(state_mutability_str)
                    .map_err(|e| format!("Invalid state mutability: {}", e))?;
                let virtual_ = node
                    .other
                    .get("virtual")
                    .and_then(|v| v.as_bool())
                    .ok_or_else(|| "FunctionDefinition missing virtual".to_string())?;
                let visibility_str = node
                    .other
                    .get("visibility")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| "FunctionDefinition missing visibility".to_string())?;
                let visibility = FunctionVisibility::from_str(visibility_str)
                    .map_err(|e| format!("Invalid visibility: {}", e))?;

                ASTNode::FunctionDefinition {
                    node_id,
                    src_location,
                    body,
                    function_selector,
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
            NodeType::EventDefinition => ASTNode::EventDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ErrorDefinition => ASTNode::ErrorDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ModifierDefinition => ASTNode::ModifierDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::StructDefinition => ASTNode::StructDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::EnumDefinition => ASTNode::EnumDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::UserDefinedValueTypeDefinition => ASTNode::UserDefinedValueTypeDefinition {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::PragmaDirective => ASTNode::PragmaDirective {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ImportDirective => ASTNode::ImportDirective {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::UsingForDirective => ASTNode::UsingForDirective {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::SourceUnit => ASTNode::SourceUnit {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::InheritanceSpecifier => ASTNode::InheritanceSpecifier {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ElementaryTypeName => ASTNode::ElementaryTypeName {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::FunctionTypeName => ASTNode::FunctionTypeName {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ParameterList => ASTNode::ParameterList {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::TryCatchClause => ASTNode::TryCatchClause {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ModifierInvocation => ASTNode::ModifierInvocation {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::UserDefinedTypeName => ASTNode::UserDefinedTypeName {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::ArrayTypeName => ASTNode::ArrayTypeName {
                node_id,
                src_location,
                nodes,
                body,
            },
            NodeType::Mapping => ASTNode::Mapping {
                node_id,
                src_location,
                nodes,
                body,
            },
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
