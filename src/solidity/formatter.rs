use crate::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, ContractKind, FunctionKind, FunctionStateMutability,
  FunctionVisibility, LiteralKind, StorageLocation, UnaryOperator, VariableMutability,
  VariableVisibility,
};

/// Converts an AST node to HTML with syntax highlighting and proper indentation
pub fn node_to_html(node: &ASTNode) -> String {
  node_to_html_with_indent(node, 0)
}

/// Internal recursive function that handles indentation
fn node_to_html_with_indent(node: &ASTNode, indent_level: usize) -> String {
  let indent = create_indent_div(indent_level);

  match node {
    // Expression nodes
    ASTNode::Assignment {
      operator,
      left_hand_side,
      right_and_side,
      ..
    } => {
      format!(
        "{}{}{}\n{}{}",
        indent,
        node_to_html_with_indent(left_hand_side, 0),
        format_assignment_operator(operator),
        create_indent_div(indent_level + 1),
        node_to_html_with_indent(right_and_side, 0)
      )
    }

    ASTNode::BinaryOperation {
      left_expression,
      operator,
      right_expression,
      ..
    } => {
      format!(
        "{}{} {}\n{}{}",
        indent,
        node_to_html_with_indent(left_expression, 0),
        format_binary_operator(operator),
        create_indent_div(indent_level + 1),
        node_to_html_with_indent(right_expression, 0)
      )
    }

    ASTNode::Conditional {
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let mut result = format!(
        "{}{} <span class=\"keyword\">?</span>\n{}{}",
        indent,
        node_to_html_with_indent(condition, 0),
        create_indent_div(indent_level + 1),
        node_to_html_with_indent(true_expression, 0)
      );
      if let Some(false_expr) = false_expression {
        result.push_str(&format!(
          "\n{}<span class=\"keyword\">:</span>\n{}{}",
          create_indent_div(indent_level),
          create_indent_div(indent_level + 1),
          node_to_html_with_indent(false_expr, 0)
        ));
      }
      result
    }

    ASTNode::ElementaryTypeNameExpression { type_name, .. } => {
      format!("{}{}", indent, node_to_html_with_indent(type_name, 0))
    }

    ASTNode::FunctionCall {
      expression,
      arguments,
      ..
    } => {
      let mut result = format!("{}{}(", indent, node_to_html_with_indent(expression, 0));
      for (i, arg) in arguments.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(arg, 0));
      }
      result.push(')');
      result
    }

    ASTNode::FunctionCallOptions {
      expression,
      options,
      ..
    } => {
      let mut result = format!("{}{}{{", indent, node_to_html_with_indent(expression, 0));
      for (i, option) in options.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(option, 0));
      }
      result.push('}');
      result
    }

    ASTNode::Identifier { name, .. } => {
      format!(
        "{}<span class=\"identifier\">{}</span>",
        indent,
        escape_html(name)
      )
    }

    ASTNode::IdentifierPath { name, .. } => {
      format!(
        "{}<span class=\"identifier\">{}</span>",
        indent,
        escape_html(name)
      )
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let mut result = format!(
        "{}{}[",
        indent,
        node_to_html_with_indent(base_expression, 0)
      );
      if let Some(index) = index_expression {
        result.push_str(&node_to_html_with_indent(index, 0));
      }
      result.push(']');
      result
    }

    ASTNode::IndexRangeAccess { nodes, body, .. } => {
      let mut result = format!("{}[", indent);
      for (i, node) in nodes.iter().enumerate() {
        if i > 0 {
          result.push(':');
        }
        result.push_str(&node_to_html_with_indent(node, 0));
      }
      result.push(']');
      if let Some(body_node) = body {
        result.push_str(&format!(" {}", node_to_html_with_indent(body_node, 0)));
      }
      result
    }

    ASTNode::Literal { value, kind, .. } => {
      let class = match kind {
        LiteralKind::Number => "number",
        LiteralKind::Bool => "boolean",
        LiteralKind::String => "string",
        LiteralKind::HexString => "hex-string",
      };
      let default_value = String::new();
      let display_value = value.as_ref().unwrap_or(&default_value);
      format!(
        "{}<span class=\"{}\">{}</span>",
        indent,
        class,
        escape_html(display_value)
      )
    }

    ASTNode::MemberAccess {
      expression,
      member_name,
      ..
    } => {
      format!(
        "{}{}.{}",
        indent,
        node_to_html_with_indent(expression, 0),
        format!("<span class=\"member\">{}</span>", escape_html(member_name))
      )
    }

    ASTNode::NewExpression { type_name, .. } => {
      format!(
        "{}<span class=\"keyword\">new</span> {}",
        indent,
        node_to_html_with_indent(type_name, 0)
      )
    }

    ASTNode::TupleExpression { components, .. } => {
      let mut result = format!("{}(", indent);
      for (i, component) in components.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(component, 0));
      }
      result.push(')');
      result
    }

    ASTNode::UnaryOperation {
      prefix,
      operator,
      sub_expression,
      ..
    } => {
      if *prefix {
        format!(
          "{}{}{}",
          indent,
          format_unary_operator(operator),
          node_to_html_with_indent(sub_expression, 0)
        )
      } else {
        format!(
          "{}{}{}",
          indent,
          node_to_html_with_indent(sub_expression, 0),
          format_unary_operator(operator)
        )
      }
    }

    ASTNode::EnumValue { name, .. } => {
      format!(
        "{}<span class=\"enum-value\">{}</span>",
        indent,
        escape_html(name)
      )
    }

    // Statement nodes
    ASTNode::Block { statements, .. } => format_block(statements, indent_level),

    ASTNode::SemanticBlock {
      statements,
      documentation,
      ..
    } => {
      let mut result = String::new();
      if let Some(doc) = documentation {
        result.push_str(&format!(
          "{}<span class=\"comment\">{}</span>\n",
          indent,
          escape_html(doc)
        ));
      }
      result.push_str(&format_block(statements, indent_level));
      result
    }

    ASTNode::Break { .. } => {
      format!("{}<span class=\"keyword\">break</span>;", indent)
    }

    ASTNode::Continue { .. } => {
      format!("{}<span class=\"keyword\">continue</span>;", indent)
    }

    ASTNode::DoWhileStatement { body, nodes, .. } => {
      let mut result = format!("{}<span class=\"keyword\">do</span>\n", indent);
      if let Some(body_node) = body {
        result.push_str(&node_to_html_with_indent(body_node, indent_level + 1));
      }
      result.push_str(&format!(
        "\n{}<span class=\"keyword\">while</span> (",
        create_indent_div(indent_level)
      ));
      for (i, node) in nodes.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(node, 0));
      }
      result.push_str(");");
      result
    }

    ASTNode::EmitStatement { event_call, .. } => {
      format!(
        "{}<span class=\"keyword\">emit</span> {};",
        indent,
        node_to_html_with_indent(event_call, 0)
      )
    }

    ASTNode::ExpressionStatement { expression, .. } => {
      format!("{}{};", indent, node_to_html_with_indent(expression, 0))
    }

    ASTNode::ForStatement {
      initialization_expression,
      condition,
      loop_expression,
      body,
      ..
    } => {
      let mut result = format!("{}<span class=\"keyword\">for</span> (", indent);
      if let Some(init) = initialization_expression {
        result.push_str(&node_to_html_with_indent(init, 0));
      }
      result.push_str("; ");
      if let Some(cond) = condition {
        result.push_str(&node_to_html_with_indent(cond, 0));
      }
      result.push_str("; ");
      if let Some(loop_expr) = loop_expression {
        result.push_str(&node_to_html_with_indent(loop_expr, 0));
      }
      result.push_str(")\n");
      result.push_str(&node_to_html_with_indent(body, indent_level + 1));
      result
    }

    ASTNode::IfStatement {
      condition,
      true_body,
      false_body,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">if</span> ({})\n{}",
        indent,
        node_to_html_with_indent(condition, 0),
        node_to_html_with_indent(true_body, indent_level + 1)
      );
      if let Some(false_branch) = false_body {
        result.push_str(&format!(
          "\n{}<span class=\"keyword\">else</span>\n{}",
          create_indent_div(indent_level),
          node_to_html_with_indent(false_branch, indent_level + 1)
        ));
      }
      result
    }

    ASTNode::InlineAssembly { .. } => {
      format!(
        "{}<span class=\"keyword\">assembly</span> {{ /* inline assembly */ }}",
        indent
      )
    }

    ASTNode::PlaceholderStatement { .. } => {
      format!("{}<span class=\"keyword\">_</span>;", indent)
    }

    ASTNode::Return { expression, .. } => {
      let mut result = format!("{}<span class=\"keyword\">return</span>", indent);
      if let Some(expr) = expression {
        result.push_str(&format!(" {}", node_to_html_with_indent(expr, 0)));
      }
      result.push(';');
      result
    }

    ASTNode::RevertStatement { error_call, .. } => {
      format!(
        "{}<span class=\"keyword\">revert</span> {};",
        indent,
        node_to_html_with_indent(error_call, 0)
      )
    }

    ASTNode::TryStatement {
      external_call,
      clauses,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">try</span> {}\n",
        indent,
        node_to_html_with_indent(external_call, 0)
      );
      for clause in clauses {
        result.push_str(&node_to_html_with_indent(clause, indent_level));
        result.push('\n');
      }
      result
    }

    ASTNode::UncheckedBlock { statements, .. } => {
      let mut result = format!("{}<span class=\"keyword\">unchecked</span> ", indent);
      result.push_str(&format_block(statements, indent_level));
      result
    }

    ASTNode::VariableDeclarationStatement {
      declarations,
      initial_value,
      ..
    } => {
      let mut result = String::new();
      for (i, decl) in declarations.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(decl, 0));
      }
      if let Some(value) = initial_value {
        result.push_str(&format!(" = {}", node_to_html_with_indent(value, 0)));
      }
      result.push(';');
      format!("{}{}", indent, result)
    }

    ASTNode::VariableDeclaration {
      type_name,
      name,
      visibility,
      mutability,
      storage_location,
      constant,
      value,
      ..
    } => {
      let mut result = String::new();

      if *constant {
        result.push_str("<span class=\"keyword\">constant</span> ");
      }

      result.push_str(&format_variable_mutability(mutability));
      result.push_str(&format_storage_location(storage_location));
      result.push_str(&format_variable_visibility(visibility));
      result.push_str(&node_to_html_with_indent(type_name, 0));
      result.push_str(&format!(
        " <span class=\"variable\">{}</span>",
        escape_html(name)
      ));

      if let Some(val) = value {
        result.push_str(&format!(" = {}", node_to_html_with_indent(val, 0)));
      }

      format!("{}{}", indent, result)
    }

    ASTNode::WhileStatement {
      condition, body, ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">while</span> ({})",
        indent,
        node_to_html_with_indent(condition, 0)
      );
      if let Some(body_node) = body {
        result.push_str(&format!(
          "\n{}",
          node_to_html_with_indent(body_node, indent_level + 1)
        ));
      }
      result
    }

    // Definition nodes
    ASTNode::ContractDefinition {
      name,
      contract_kind,
      abstract_,
      base_contracts,
      nodes,
      ..
    } => {
      let mut result = String::new();
      if *abstract_ {
        result.push_str("<span class=\"keyword\">abstract</span> ");
      }
      result.push_str(&format_contract_kind(contract_kind));
      result.push_str(&format!(
        " <span class=\"contract\">{}</span>",
        escape_html(name)
      ));

      if !base_contracts.is_empty() {
        result.push_str(" <span class=\"keyword\">is</span> ");
        for (i, base) in base_contracts.iter().enumerate() {
          if i > 0 {
            result.push_str(", ");
          }
          result.push_str(&node_to_html_with_indent(base, 0));
        }
      }

      result.push_str(" {\n");
      for node in nodes {
        result.push_str(&node_to_html_with_indent(node, indent_level + 1));
        result.push_str("\n");
      }
      result.push('}');

      format!("{}{}", indent, result)
    }

    ASTNode::FunctionDefinition {
      kind,
      name,
      visibility,
      state_mutability,
      virtual_,
      parameters,
      return_parameters,
      modifiers,
      body,
      ..
    } => {
      let mut result = String::new();

      result.push_str(&format_function_kind(kind));
      if !name.is_empty() {
        result.push_str(&format!(
          " <span class=\"function\">{}</span>",
          escape_html(name)
        ));
      }

      result.push_str(&format!("({})", node_to_html_with_indent(parameters, 0)));
      result.push_str(&format_function_visibility(visibility));

      if *virtual_ {
        result.push_str(" <span class=\"keyword\">virtual</span>");
      }

      result.push_str(&format_function_state_mutability(state_mutability));

      for modifier in modifiers {
        result.push_str(&format!(" {}", node_to_html_with_indent(modifier, 0)));
      }

      // Check if return_parameters has actual parameters
      if let ASTNode::ParameterList {
        parameters: return_params,
        ..
      } = return_parameters.as_ref()
      {
        if !return_params.is_empty() {
          result.push_str(&format!(
            " <span class=\"keyword\">returns</span> ({})",
            node_to_html_with_indent(return_parameters, 0)
          ));
        }
      }

      if let Some(body_node) = body {
        result.push_str(" ");
        result.push_str(&node_to_html_with_indent(body_node, indent_level));
      } else {
        result.push(';');
      }

      format!("{}{}", indent, result)
    }

    ASTNode::EventDefinition {
      name, parameters, ..
    } => {
      format!(
        "{}<span class=\"keyword\">event</span> <span class=\"event\">{}</span>({})",
        indent,
        escape_html(name),
        node_to_html_with_indent(parameters, 0)
      )
    }

    ASTNode::ErrorDefinition {
      name, parameters, ..
    } => {
      format!(
        "{}<span class=\"keyword\">error</span> <span class=\"error\">{}</span>({})",
        indent,
        escape_html(name),
        node_to_html_with_indent(parameters, 0)
      )
    }

    ASTNode::ModifierDefinition {
      name,
      parameters,
      body,
      virtual_,
      visibility,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">modifier</span> <span class=\"modifier\">{}</span>({})",
        indent,
        escape_html(name),
        node_to_html_with_indent(parameters, 0)
      );

      result.push_str(&format_function_visibility(visibility));

      if *virtual_ {
        result.push_str(" <span class=\"keyword\">virtual</span>");
      }

      result.push_str(&format!(
        " {}",
        node_to_html_with_indent(body, indent_level)
      ));
      result
    }

    ASTNode::StructDefinition {
      name,
      members,
      visibility,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">struct</span> <span class=\"struct\">{}</span>",
        indent,
        escape_html(name)
      );
      result.push_str(&format_variable_visibility(visibility));
      result.push_str(" {\n");
      for member in members {
        result.push_str(&node_to_html_with_indent(member, indent_level + 1));
        result.push_str(";\n");
      }
      result.push('}');
      result
    }

    ASTNode::EnumDefinition { name, members, .. } => {
      let mut result = format!(
        "{}<span class=\"keyword\">enum</span> <span class=\"enum\">{}</span> {{",
        indent,
        escape_html(name)
      );
      for (i, member) in members.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(member, 0));
      }
      result.push('}');
      result
    }

    ASTNode::UserDefinedValueTypeDefinition { nodes, body, .. } => {
      let mut result = format!("{}<span class=\"keyword\">type</span> ", indent);
      for node in nodes {
        result.push_str(&node_to_html_with_indent(node, 0));
      }
      if let Some(body_node) = body {
        result.push_str(&format!(" {}", node_to_html_with_indent(body_node, 0)));
      }
      result
    }

    // Directive nodes
    ASTNode::PragmaDirective { literals, .. } => {
      let mut result = format!("{}<span class=\"keyword\">pragma</span>", indent);
      for literal in literals {
        result.push_str(&format!(
          " <span class=\"pragma\">{}</span>",
          escape_html(literal)
        ));
      }
      result.push(';');
      result
    }

    ASTNode::ImportDirective { file, .. } => {
      format!(
        "{}<span class=\"keyword\">import</span> <span class=\"string\">\"{}\"</span>;",
        indent,
        escape_html(file)
      )
    }

    ASTNode::UsingForDirective {
      library_name,
      type_name,
      global,
      ..
    } => {
      let mut result = format!("{}<span class=\"keyword\">using</span> ", indent);
      if let Some(lib) = library_name {
        result.push_str(&node_to_html_with_indent(lib, 0));
      }
      result.push_str(" <span class=\"keyword\">for</span> ");
      if let Some(type_node) = type_name {
        result.push_str(&node_to_html_with_indent(type_node, 0));
      }
      if *global {
        result.push_str(" <span class=\"keyword\">global</span>");
      }
      result.push(';');
      result
    }

    // Other nodes
    ASTNode::SourceUnit { nodes, .. } => {
      let mut result = String::new();
      for (i, node) in nodes.iter().enumerate() {
        if i > 0 {
          result.push_str("\n\n");
        }
        result.push_str(&node_to_html_with_indent(node, indent_level));
      }
      result
    }

    ASTNode::InheritanceSpecifier { base_name, .. } => node_to_html_with_indent(base_name, 0),

    ASTNode::ElementaryTypeName { name, .. } => {
      format!(
        "{}<span class=\"type\">{}</span>",
        indent,
        escape_html(name)
      )
    }

    ASTNode::FunctionTypeName {
      parameter_types,
      return_parameter_types,
      state_mutability,
      visibility,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">function</span>({})",
        indent,
        node_to_html_with_indent(parameter_types, 0)
      );
      result.push_str(&format_function_visibility(visibility));
      result.push_str(&format_function_state_mutability(state_mutability));

      if let ASTNode::ParameterList {
        parameters: return_params,
        ..
      } = return_parameter_types.as_ref()
      {
        if !return_params.is_empty() {
          result.push_str(&format!(
            " <span class=\"keyword\">returns</span> ({})",
            node_to_html_with_indent(return_parameter_types, 0)
          ));
        }
      }
      result
    }

    ASTNode::ParameterList { parameters, .. } => {
      let mut result = String::new();
      for (i, param) in parameters.iter().enumerate() {
        if i > 0 {
          result.push_str(", ");
        }
        result.push_str(&node_to_html_with_indent(param, 0));
      }
      result
    }

    ASTNode::TryCatchClause {
      error_name,
      block,
      parameters,
      ..
    } => {
      let mut result = format!("{}<span class=\"keyword\">catch</span>", indent);
      if !error_name.is_empty() {
        result.push_str(&format!(
          " <span class=\"identifier\">{}</span>",
          escape_html(error_name)
        ));
      }
      if let Some(params) = parameters {
        result.push_str(&format!(" ({})", node_to_html_with_indent(params, 0)));
      }
      result.push_str(&format!(
        " {}",
        node_to_html_with_indent(block, indent_level)
      ));
      result
    }

    ASTNode::ModifierInvocation {
      modifier_name,
      arguments,
      ..
    } => {
      let mut result = node_to_html_with_indent(modifier_name, 0);
      if let Some(args) = arguments {
        result.push('(');
        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            result.push_str(", ");
          }
          result.push_str(&node_to_html_with_indent(arg, 0));
        }
        result.push(')');
      }
      format!("{}{}", indent, result)
    }

    ASTNode::UserDefinedTypeName { path_node, .. } => {
      format!("{}{}", indent, node_to_html_with_indent(path_node, 0))
    }

    ASTNode::ArrayTypeName { base_type, .. } => {
      format!("{}{}[]", indent, node_to_html_with_indent(base_type, 0))
    }

    ASTNode::Mapping {
      key_type,
      value_type,
      key_name,
      value_name,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"keyword\">mapping</span>({} ",
        indent,
        node_to_html_with_indent(key_type, 0)
      );
      if let Some(key_name_str) = key_name {
        result.push_str(&format!(
          "<span class=\"variable\">{}</span> ",
          escape_html(key_name_str)
        ));
      }
      result.push_str("=> ");
      result.push_str(&node_to_html_with_indent(value_type, 0));
      if let Some(value_name_str) = value_name {
        result.push_str(&format!(
          " <span class=\"variable\">{}</span>",
          escape_html(value_name_str)
        ));
      }
      result.push(')');
      result
    }

    ASTNode::StructuredDocumentation { text, .. } => {
      format!(
        "{}<span class=\"comment\">{}</span>",
        indent,
        escape_html(text)
      )
    }

    ASTNode::Other {
      node_type,
      nodes,
      body,
      ..
    } => {
      let mut result = format!(
        "{}<span class=\"other\">/* {} */</span>",
        indent,
        escape_html(node_type)
      );
      for node in nodes {
        result.push_str(&format!(" {}", node_to_html_with_indent(node, 0)));
      }
      if let Some(body_node) = body {
        result.push_str(&format!(" {}", node_to_html_with_indent(body_node, 0)));
      }
      result
    }
  }
}

// Helper functions for formatting

fn create_indent_div(level: usize) -> String {
  if level == 0 {
    String::new()
  } else {
    format!("<div class=\"indent-{}\">", level)
  }
}

fn format_block(statements: &[ASTNode], indent_level: usize) -> String {
  let mut result = String::from("{\n");
  for statement in statements {
    result.push_str(&node_to_html_with_indent(statement, indent_level + 1));
    result.push('\n');
  }
  result.push_str(&format!("{}}}", create_indent_div(indent_level)));
  result
}

fn format_binary_operator(op: &BinaryOperator) -> String {
  let op_str = match op {
    BinaryOperator::Add => "+",
    BinaryOperator::Subtract => "-",
    BinaryOperator::Multiply => "*",
    BinaryOperator::Divide => "/",
    BinaryOperator::Modulo => "%",
    BinaryOperator::Power => "**",
    BinaryOperator::Equal => "==",
    BinaryOperator::NotEqual => "!=",
    BinaryOperator::LessThan => "<",
    BinaryOperator::LessThanOrEqual => "<=",
    BinaryOperator::GreaterThan => ">",
    BinaryOperator::GreaterThanOrEqual => ">=",
    BinaryOperator::And => "&&",
    BinaryOperator::Or => "||",
    BinaryOperator::BitwiseAnd => "&",
    BinaryOperator::BitwiseOr => "|",
    BinaryOperator::BitwiseXor => "^",
    BinaryOperator::LeftShift => "<<",
    BinaryOperator::RightShift => ">>",
  };
  format!("<span class=\"operator\">{}</span>", op_str)
}

fn format_assignment_operator(op: &AssignmentOperator) -> String {
  let op_str = match op {
    AssignmentOperator::Assign => "=",
    AssignmentOperator::AddAssign => "+=",
    AssignmentOperator::SubtractAssign => "-=",
    AssignmentOperator::MultiplyAssign => "*=",
    AssignmentOperator::DivideAssign => "/=",
    AssignmentOperator::ModuloAssign => "%=",
    AssignmentOperator::BitwiseAndAssign => "&=",
    AssignmentOperator::BitwiseOrAssign => "|=",
    AssignmentOperator::BitwiseXorAssign => "^=",
    AssignmentOperator::LeftShiftAssign => "<<=",
    AssignmentOperator::RightShiftAssign => ">>=",
  };
  format!(" <span class=\"operator\">{}</span>", op_str)
}

fn format_unary_operator(op: &UnaryOperator) -> String {
  let op_str = match op {
    UnaryOperator::Increment => "++",
    UnaryOperator::Decrement => "--",
    UnaryOperator::Plus => "+",
    UnaryOperator::Minus => "-",
    UnaryOperator::BitwiseNot => "~",
    UnaryOperator::Not => "!",
    UnaryOperator::Delete => "delete",
  };
  format!("<span class=\"operator\">{}</span>", op_str)
}

fn format_function_kind(kind: &FunctionKind) -> String {
  match kind {
    FunctionKind::Constructor => "<span class=\"keyword\">constructor</span>".to_string(),
    FunctionKind::Function => "<span class=\"keyword\">function</span>".to_string(),
    FunctionKind::Fallback => "<span class=\"keyword\">fallback</span>".to_string(),
    FunctionKind::Receive => "<span class=\"keyword\">receive</span>".to_string(),
    FunctionKind::FreeFunction => "<span class=\"keyword\">function</span>".to_string(),
  }
}

fn format_contract_kind(kind: &ContractKind) -> String {
  match kind {
    ContractKind::Contract => "<span class=\"keyword\">contract</span>".to_string(),
    ContractKind::Library => "<span class=\"keyword\">library</span>".to_string(),
    ContractKind::Abstract => "<span class=\"keyword\">abstract contract</span>".to_string(),
    ContractKind::Interface => "<span class=\"keyword\">interface</span>".to_string(),
  }
}

fn format_function_visibility(visibility: &FunctionVisibility) -> String {
  let vis_str = match visibility {
    FunctionVisibility::Public => " <span class=\"keyword\">public</span>",
    FunctionVisibility::Private => " <span class=\"keyword\">private</span>",
    FunctionVisibility::Internal => " <span class=\"keyword\">internal</span>",
    FunctionVisibility::External => " <span class=\"keyword\">external</span>",
  };
  vis_str.to_string()
}

fn format_variable_visibility(visibility: &VariableVisibility) -> String {
  let vis_str = match visibility {
    VariableVisibility::Public => " <span class=\"keyword\">public</span>",
    VariableVisibility::Private => " <span class=\"keyword\">private</span>",
    VariableVisibility::Internal => " <span class=\"keyword\">internal</span>",
  };
  vis_str.to_string()
}

fn format_function_state_mutability(mutability: &FunctionStateMutability) -> String {
  match mutability {
    FunctionStateMutability::Pure => " <span class=\"keyword\">pure</span>".to_string(),
    FunctionStateMutability::View => " <span class=\"keyword\">view</span>".to_string(),
    FunctionStateMutability::NonPayable => String::new(),
    FunctionStateMutability::Payable => " <span class=\"keyword\">payable</span>".to_string(),
  }
}

fn format_variable_mutability(mutability: &VariableMutability) -> String {
  match mutability {
    VariableMutability::Mutable => String::new(),
    VariableMutability::Immutable => "<span class=\"keyword\">immutable</span> ".to_string(),
    VariableMutability::Constant => "<span class=\"keyword\">constant</span> ".to_string(),
  }
}

fn format_storage_location(location: &StorageLocation) -> String {
  match location {
    StorageLocation::Default => String::new(),
    StorageLocation::Storage => "<span class=\"keyword\">storage</span> ".to_string(),
    StorageLocation::Memory => "<span class=\"keyword\">memory</span> ".to_string(),
    StorageLocation::Calldata => "<span class=\"keyword\">calldata</span> ".to_string(),
  }
}

fn escape_html(text: &str) -> String {
  text
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}
