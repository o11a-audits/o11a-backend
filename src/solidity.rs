use foundry_compilers::artifacts::ast::{LowFidelitySourceLocation, Node};
use foundry_compilers_artifacts::NodeType;

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

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    // Expression nodes
    Assignment {
        node_id: i32,
        src_location: SourceLocation,
    },
    BinaryOperation {
        node_id: i32,
        src_location: SourceLocation,
    },
    Conditional {
        node_id: i32,
        src_location: SourceLocation,
    },
    ElementaryTypeNameExpression {
        node_id: i32,
        src_location: SourceLocation,
    },
    FunctionCall {
        node_id: i32,
        src_location: SourceLocation,
    },
    FunctionCallOptions {
        node_id: i32,
        src_location: SourceLocation,
    },
    Identifier {
        node_id: i32,
        src_location: SourceLocation,
    },
    IndexAccess {
        node_id: i32,
        src_location: SourceLocation,
    },
    IndexRangeAccess {
        node_id: i32,
        src_location: SourceLocation,
    },
    Literal {
        node_id: i32,
        src_location: SourceLocation,
    },
    MemberAccess {
        node_id: i32,
        src_location: SourceLocation,
    },
    NewExpression {
        node_id: i32,
        src_location: SourceLocation,
    },
    TupleExpression {
        node_id: i32,
        src_location: SourceLocation,
    },
    UnaryOperation {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Statement nodes
    Block {
        node_id: i32,
        src_location: SourceLocation,
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
    },
    EmitStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    ExpressionStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    ForStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    IfStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    InlineAssembly {
        node_id: i32,
        src_location: SourceLocation,
    },
    PlaceholderStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    Return {
        node_id: i32,
        src_location: SourceLocation,
    },
    RevertStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    TryStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    UncheckedBlock {
        node_id: i32,
        src_location: SourceLocation,
    },
    VariableDeclarationStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    VariableDeclaration {
        node_id: i32,
        src_location: SourceLocation,
    },
    WhileStatement {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Yul nodes
    YulAssignment {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulBlock {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulBreak {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulCase {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulContinue {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulExpressionStatement {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulLeave {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulForLoop {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulFunctionDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulIf {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulSwitch {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulVariableDeclaration {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulFunctionCall {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulIdentifier {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulLiteral {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulLiteralValue {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulHexValue {
        node_id: i32,
        src_location: SourceLocation,
    },
    YulTypedName {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Definition nodes
    ContractDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    FunctionDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    EventDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    ErrorDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    ModifierDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    StructDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    EnumDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },
    UserDefinedValueTypeDefinition {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Directive nodes
    PragmaDirective {
        node_id: i32,
        src_location: SourceLocation,
    },
    ImportDirective {
        node_id: i32,
        src_location: SourceLocation,
    },
    UsingForDirective {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Other nodes
    SourceUnit {
        node_id: i32,
        src_location: SourceLocation,
    },
    InheritanceSpecifier {
        node_id: i32,
        src_location: SourceLocation,
    },
    ElementaryTypeName {
        node_id: i32,
        src_location: SourceLocation,
    },
    FunctionTypeName {
        node_id: i32,
        src_location: SourceLocation,
    },
    ParameterList {
        node_id: i32,
        src_location: SourceLocation,
    },
    TryCatchClause {
        node_id: i32,
        src_location: SourceLocation,
    },
    ModifierInvocation {
        node_id: i32,
        src_location: SourceLocation,
    },
    UserDefinedTypeName {
        node_id: i32,
        src_location: SourceLocation,
    },
    ArrayTypeName {
        node_id: i32,
        src_location: SourceLocation,
    },
    Mapping {
        node_id: i32,
        src_location: SourceLocation,
    },

    // Catch-all for unknown node types
    Other {
        node_id: i32,
        src_location: SourceLocation,
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

impl From<&Node> for ASTNode {
    fn from(node: &Node) -> Self {
        let node_id = node.id.map(|id| id as i32).unwrap_or(0);
        let src_location = SourceLocation::from(&node.src);

        match node.node_type {
            NodeType::Assignment => ASTNode::Assignment {
                node_id,
                src_location,
            },
            NodeType::BinaryOperation => ASTNode::BinaryOperation {
                node_id,
                src_location,
            },
            NodeType::Conditional => ASTNode::Conditional {
                node_id,
                src_location,
            },
            NodeType::ElementaryTypeNameExpression => ASTNode::ElementaryTypeNameExpression {
                node_id,
                src_location,
            },
            NodeType::FunctionCall => ASTNode::FunctionCall {
                node_id,
                src_location,
            },
            NodeType::FunctionCallOptions => ASTNode::FunctionCallOptions {
                node_id,
                src_location,
            },
            NodeType::Identifier => ASTNode::Identifier {
                node_id,
                src_location,
            },
            NodeType::IndexAccess => ASTNode::IndexAccess {
                node_id,
                src_location,
            },
            NodeType::IndexRangeAccess => ASTNode::IndexRangeAccess {
                node_id,
                src_location,
            },
            NodeType::Literal => ASTNode::Literal {
                node_id,
                src_location,
            },
            NodeType::MemberAccess => ASTNode::MemberAccess {
                node_id,
                src_location,
            },
            NodeType::NewExpression => ASTNode::NewExpression {
                node_id,
                src_location,
            },
            NodeType::TupleExpression => ASTNode::TupleExpression {
                node_id,
                src_location,
            },
            NodeType::UnaryOperation => ASTNode::UnaryOperation {
                node_id,
                src_location,
            },
            NodeType::Block => ASTNode::Block {
                node_id,
                src_location,
            },
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
            },
            NodeType::EmitStatement => ASTNode::EmitStatement {
                node_id,
                src_location,
            },
            NodeType::ExpressionStatement => ASTNode::ExpressionStatement {
                node_id,
                src_location,
            },
            NodeType::ForStatement => ASTNode::ForStatement {
                node_id,
                src_location,
            },
            NodeType::IfStatement => ASTNode::IfStatement {
                node_id,
                src_location,
            },
            NodeType::InlineAssembly => ASTNode::InlineAssembly {
                node_id,
                src_location,
            },
            NodeType::PlaceholderStatement => ASTNode::PlaceholderStatement {
                node_id,
                src_location,
            },
            NodeType::Return => ASTNode::Return {
                node_id,
                src_location,
            },
            NodeType::RevertStatement => ASTNode::RevertStatement {
                node_id,
                src_location,
            },
            NodeType::TryStatement => ASTNode::TryStatement {
                node_id,
                src_location,
            },
            NodeType::UncheckedBlock => ASTNode::UncheckedBlock {
                node_id,
                src_location,
            },
            NodeType::VariableDeclarationStatement => ASTNode::VariableDeclarationStatement {
                node_id,
                src_location,
            },
            NodeType::VariableDeclaration => ASTNode::VariableDeclaration {
                node_id,
                src_location,
            },
            NodeType::WhileStatement => ASTNode::WhileStatement {
                node_id,
                src_location,
            },
            NodeType::YulAssignment => ASTNode::YulAssignment {
                node_id,
                src_location,
            },
            NodeType::YulBlock => ASTNode::YulBlock {
                node_id,
                src_location,
            },
            NodeType::YulBreak => ASTNode::YulBreak {
                node_id,
                src_location,
            },
            NodeType::YulCase => ASTNode::YulCase {
                node_id,
                src_location,
            },
            NodeType::YulContinue => ASTNode::YulContinue {
                node_id,
                src_location,
            },
            NodeType::YulExpressionStatement => ASTNode::YulExpressionStatement {
                node_id,
                src_location,
            },
            NodeType::YulLeave => ASTNode::YulLeave {
                node_id,
                src_location,
            },
            NodeType::YulForLoop => ASTNode::YulForLoop {
                node_id,
                src_location,
            },
            NodeType::YulFunctionDefinition => ASTNode::YulFunctionDefinition {
                node_id,
                src_location,
            },
            NodeType::YulIf => ASTNode::YulIf {
                node_id,
                src_location,
            },
            NodeType::YulSwitch => ASTNode::YulSwitch {
                node_id,
                src_location,
            },
            NodeType::YulVariableDeclaration => ASTNode::YulVariableDeclaration {
                node_id,
                src_location,
            },
            NodeType::YulFunctionCall => ASTNode::YulFunctionCall {
                node_id,
                src_location,
            },
            NodeType::YulIdentifier => ASTNode::YulIdentifier {
                node_id,
                src_location,
            },
            NodeType::YulLiteral => ASTNode::YulLiteral {
                node_id,
                src_location,
            },
            NodeType::YulLiteralValue => ASTNode::YulLiteralValue {
                node_id,
                src_location,
            },
            NodeType::YulHexValue => ASTNode::YulHexValue {
                node_id,
                src_location,
            },
            NodeType::YulTypedName => ASTNode::YulTypedName {
                node_id,
                src_location,
            },
            NodeType::ContractDefinition => ASTNode::ContractDefinition {
                node_id,
                src_location,
            },
            NodeType::FunctionDefinition => ASTNode::FunctionDefinition {
                node_id,
                src_location,
            },
            NodeType::EventDefinition => ASTNode::EventDefinition {
                node_id,
                src_location,
            },
            NodeType::ErrorDefinition => ASTNode::ErrorDefinition {
                node_id,
                src_location,
            },
            NodeType::ModifierDefinition => ASTNode::ModifierDefinition {
                node_id,
                src_location,
            },
            NodeType::StructDefinition => ASTNode::StructDefinition {
                node_id,
                src_location,
            },
            NodeType::EnumDefinition => ASTNode::EnumDefinition {
                node_id,
                src_location,
            },
            NodeType::UserDefinedValueTypeDefinition => ASTNode::UserDefinedValueTypeDefinition {
                node_id,
                src_location,
            },
            NodeType::PragmaDirective => ASTNode::PragmaDirective {
                node_id,
                src_location,
            },
            NodeType::ImportDirective => ASTNode::ImportDirective {
                node_id,
                src_location,
            },
            NodeType::UsingForDirective => ASTNode::UsingForDirective {
                node_id,
                src_location,
            },
            NodeType::SourceUnit => ASTNode::SourceUnit {
                node_id,
                src_location,
            },
            NodeType::InheritanceSpecifier => ASTNode::InheritanceSpecifier {
                node_id,
                src_location,
            },
            NodeType::ElementaryTypeName => ASTNode::ElementaryTypeName {
                node_id,
                src_location,
            },
            NodeType::FunctionTypeName => ASTNode::FunctionTypeName {
                node_id,
                src_location,
            },
            NodeType::ParameterList => ASTNode::ParameterList {
                node_id,
                src_location,
            },
            NodeType::TryCatchClause => ASTNode::TryCatchClause {
                node_id,
                src_location,
            },
            NodeType::ModifierInvocation => ASTNode::ModifierInvocation {
                node_id,
                src_location,
            },
            NodeType::UserDefinedTypeName => ASTNode::UserDefinedTypeName {
                node_id,
                src_location,
            },
            NodeType::ArrayTypeName => ASTNode::ArrayTypeName {
                node_id,
                src_location,
            },
            NodeType::Mapping => ASTNode::Mapping {
                node_id,
                src_location,
            },
            NodeType::Other(ref node_type_str) => ASTNode::Other {
                node_id,
                src_location,
                node_type: node_type_str.clone(),
            },
        }
    }
}
