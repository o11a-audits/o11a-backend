use crate::core::{ContractKind, FunctionKind};
use crate::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, FunctionStateMutability,
  FunctionVisibility, LiteralKind, StorageLocation, UnaryOperator,
  VariableMutability, VariableVisibility,
};

/// Converts an AST node and all its children to a formatted HTML string with syntax highlighting.
///
/// This function recursively processes nodes and uses div-based indentation with padding.
/// Binary operators are formatted with the left-hand side and operator on one line,
/// and the right-hand side indented on the next line.
pub fn node_to_source_text(node: &ASTNode) -> String {
  format!(
    "<pre><code>{}</code></pre>",
    do_node_to_source_text(node, 0)
  )
}

fn do_node_to_source_text(node: &ASTNode, indent_level: usize) -> String {
  let node_str = match node {
    ASTNode::Assignment {
      operator,
      left_hand_side,
      right_hand_side,
      ..
    } => {
      let lhs = do_node_to_source_text(left_hand_side, indent_level);
      let op = assignment_operator_to_string(operator);

      let indent_level = indent_level + 1;
      let rhs = do_node_to_source_text(right_hand_side, indent_level);
      format!(
        "{} {}{}",
        lhs,
        format_operator(&op),
        indent(&rhs, indent_level)
      )
    }

    ASTNode::BinaryOperation {
      left_expression,
      operator,
      right_expression,
      ..
    } => {
      let lhs = do_node_to_source_text(left_expression, indent_level);
      let op = binary_operator_to_string(operator);

      if is_boolean_and_or_operator(operator) || is_math_operator(operator) {
        // Boolean &&, ||, and math operators do not indent, but place the
        // operator and rhs on the next line with the same indentation level
        // of the lhs
        let rhs = do_node_to_source_text(right_expression, indent_level);
        format!("{}\n{}", lhs, format!("{} {}", format_operator(&op), &rhs),)
      } else {
        let indent_level = indent_level + 1;
        let rhs = do_node_to_source_text(right_expression, indent_level);
        format!(
          "{}{}",
          lhs,
          indent(&format!("{} {}", format_operator(&op), &rhs), indent_level)
        )
      }
    }

    ASTNode::Conditional {
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level);

      let indent_level = indent_level + 1;
      let part = if let Some(false_expr) = false_expression {
        format!(
          "\n{} {}\n{} {}",
          format_operator("?"),
          do_node_to_source_text(true_expression, indent_level),
          format_operator(":"),
          do_node_to_source_text(false_expr, indent_level),
        )
      } else {
        format!(
          "\n{} {}",
          format_operator("?"),
          do_node_to_source_text(true_expression, indent_level)
        )
      };

      format!("{}{}", cond, part)
    }

    ASTNode::ElementaryTypeNameExpression { type_name, .. } => {
      do_node_to_source_text(type_name, indent_level)
    }

    ASTNode::FunctionCall {
      expression,
      arguments,
      ..
    } => {
      let expr = do_node_to_source_text(expression, indent_level);

      let indent_level = indent_level + 1;
      let args = arguments
        .iter()
        .map(|arg| do_node_to_source_text(arg, indent_level))
        .collect::<Vec<_>>()
        .join("\n");

      if arguments.is_empty() {
        format!("{}()", expr)
      } else {
        format!("{}({})", expr, indent(&args, indent_level))
      }
    }

    ASTNode::FunctionCallOptions {
      expression,
      options,
      ..
    } => {
      let expr = do_node_to_source_text(expression, indent_level);

      let indent_level = indent_level + 1;
      let opts = options
        .iter()
        .map(|opt| do_node_to_source_text(opt, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !opts.is_empty() { "\n" } else { "" };
      format!(
        "{}{{{}{}}}",
        expr,
        indent(&opts, indent_level),
        trailing_newline
      )
    }

    ASTNode::Identifier { name, .. } => format_identifier(name),

    ASTNode::IdentifierPath {
      name,
      referenced_declaration,
      ..
    } => {
      if referenced_declaration < &0 {
        format_global(&name)
      } else {
        format_identifier(name)
      }
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let base = do_node_to_source_text(base_expression, indent_level);
      if let Some(idx) = index_expression {
        format!("{}[{}]", base, do_node_to_source_text(idx, indent_level))
      } else {
        base
      }
    }

    ASTNode::IndexRangeAccess { .. } => {
      "IndexRangeAccess placeholder".to_owned()
    }

    ASTNode::Literal {
      kind,
      value,
      hex_value,
      ..
    } => match kind {
      LiteralKind::String => format_string(&format!(
        "\"{}\"",
        html_escape(value.as_ref().unwrap_or(&hex_value))
      )),
      LiteralKind::HexString => {
        format_number(&format!("0x{}", value.as_ref().unwrap_or(&hex_value)))
      }
      LiteralKind::Number | LiteralKind::Bool => {
        format_number(&value.as_ref().unwrap_or(&hex_value))
      }
    },

    ASTNode::MemberAccess {
      expression,
      member_name,
      ..
    } => {
      let expr = do_node_to_source_text(expression, indent_level);

      let indent_level = indent_level + 1;
      let member =
        format!("{}{}", format_operator("."), format_member(&member_name));
      format!("{}{}", expr, indent(&member, indent_level),)
    }

    ASTNode::NewExpression { type_name, .. } => {
      format!(
        "{} {}",
        format_keyword("new"),
        do_node_to_source_text(type_name, indent_level)
      )
    }

    ASTNode::TupleExpression { components, .. } => {
      let indent_level = indent_level + 1;
      let comps = components
        .iter()
        .map(|c| do_node_to_source_text(c, indent_level))
        .collect::<Vec<_>>()
        .join(",\n");
      let trailing_newline = if !comps.is_empty() { "\n" } else { "" };
      format!(
        "{}{}{}{}",
        format_brace("(", indent_level),
        indent(&comps, indent_level),
        trailing_newline,
        format_brace(")", indent_level)
      )
    }

    ASTNode::UnaryOperation {
      prefix,
      operator,
      sub_expression,
      ..
    } => {
      let op = unary_operator_to_string(operator);
      let expr = do_node_to_source_text(sub_expression, indent_level);
      if *prefix {
        format!("{}{}", format_operator(&op), expr)
      } else {
        format!("{}{}", expr, format_operator(&op))
      }
    }

    ASTNode::EnumValue { name, .. } => format_enum_value(&html_escape(name)),

    ASTNode::Block { statements, .. } => {
      if statements.is_empty() {
        format_brace("{}", indent_level)
      } else {
        let indent_level = indent_level + 1;
        let stmts = statements
          .iter()
          .map(|s| do_node_to_source_text(s, indent_level))
          .collect::<Vec<_>>()
          .join("\n\n");
        let trailing_newline = if !stmts.is_empty() { "\n" } else { "" };
        format!(
          "{}{}{}{}",
          format_brace("{", indent_level),
          indent(&stmts, indent_level),
          trailing_newline,
          format_brace("}", indent_level),
        )
      }
    }

    ASTNode::SemanticBlock { statements, .. } => statements
      .iter()
      .map(|s| do_node_to_source_text(s, indent_level))
      .collect::<Vec<_>>()
      .join("\n"),

    ASTNode::Break { .. } => format_keyword("break"),

    ASTNode::Continue { .. } => format_keyword("continue"),

    ASTNode::DoWhileStatement { nodes, body, .. } => {
      let body_str = if let Some(b) = body {
        do_node_to_source_text(b, indent_level)
      } else {
        String::new()
      };
      let condition = if !nodes.is_empty() {
        do_node_to_source_text(&nodes[0], indent_level)
      } else {
        String::new()
      };
      format!(
        "{} {} {} ({})",
        format_keyword("do"),
        body_str,
        format_keyword("while"),
        condition
      )
    }

    ASTNode::EmitStatement { event_call, .. } => {
      format!(
        "{} {}",
        format_keyword("emit"),
        do_node_to_source_text(event_call, indent_level)
      )
    }

    ASTNode::ExpressionStatement { expression, .. } => {
      do_node_to_source_text(expression, indent_level)
    }

    ASTNode::ForStatement {
      initialization_expression,
      condition,
      loop_expression,
      body,
      ..
    } => {
      let init = if let Some(init_expr) = initialization_expression {
        do_node_to_source_text(init_expr, indent_level)
      } else {
        String::new()
      };
      let cond = if let Some(cond_expr) = condition {
        do_node_to_source_text(cond_expr, indent_level)
      } else {
        String::new()
      };
      let loop_expr = if let Some(l_expr) = loop_expression {
        do_node_to_source_text(l_expr, indent_level)
      } else {
        String::new()
      };
      let body_str = do_node_to_source_text(body, indent_level);
      format!(
        "{} ({}; {}; {}) {}",
        format_keyword("for"),
        init,
        cond,
        loop_expr,
        body_str
      )
    }

    ASTNode::IfStatement {
      condition,
      true_body,
      false_body,
      ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level);
      let true_b = do_node_to_source_text(true_body, indent_level);
      let false_part = if let Some(false_b) = false_body {
        format!(
          " {} {}",
          format_keyword("else"),
          do_node_to_source_text(false_b, indent_level)
        )
      } else {
        String::new()
      };
      format!(
        "{} ({}) {}{}",
        format_keyword("if"),
        indent(&cond, indent_level + 1),
        true_b,
        false_part
      )
    }

    ASTNode::InlineAssembly { .. } => format_keyword("assembly"),

    ASTNode::PlaceholderStatement { .. } => format_keyword("placeholder"),

    ASTNode::Return { expression, .. } => {
      if let Some(expr) = expression {
        format!(
          "{} {}",
          format_keyword("return"),
          do_node_to_source_text(expr, indent_level)
        )
      } else {
        format_keyword("return")
      }
    }

    ASTNode::RevertStatement { error_call, .. } => {
      format!(
        "{} {}",
        format_keyword("revert"),
        do_node_to_source_text(error_call, indent_level)
      )
    }

    ASTNode::TryStatement { .. } => "TryStatement placeholder".to_owned(),

    ASTNode::UncheckedBlock { statements, .. } => {
      let indent_level = indent_level + 1;

      let stmts = statements
        .iter()
        .map(|s| do_node_to_source_text(s, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !stmts.is_empty() { "\n" } else { "" };
      format!(
        "{} {}{}{}{}",
        format_keyword("unchecked"),
        format_brace("{", indent_level),
        indent(&stmts, indent_level),
        trailing_newline,
        format_brace("}", indent_level)
      )
    }

    ASTNode::VariableDeclarationStatement {
      declarations,
      initial_value,
      ..
    } => {
      if let Some(init) = initial_value {
        declarations
          .iter()
          .map(|d| {
            format!(
              "{} {} {}",
              do_node_to_source_text(d, indent_level),
              format_operator("="),
              do_node_to_source_text(init, indent_level),
            )
          })
          .collect::<Vec<_>>()
          .join("\n")
      } else {
        declarations
          .iter()
          .map(|d| do_node_to_source_text(d, indent_level))
          .collect::<Vec<_>>()
          .join("\n")
      }
    }

    ASTNode::VariableDeclaration {
      type_name,
      storage_location,
      name,
      visibility,
      mutability,
      value,
      constant,
      ..
    } => {
      let type_str = do_node_to_source_text(type_name, indent_level);
      let storage = storage_location_to_string(storage_location);
      let visibility_str = variable_visibility_to_string(visibility);
      let mutability_str = variable_mutability_to_string(mutability);

      let mut parts = vec![type_str];
      if !storage.is_empty() {
        parts.push(format_keyword(&storage));
      }
      if !visibility_str.is_empty() {
        parts.push(format_keyword(&visibility_str));
      }
      if !mutability_str.is_empty() {
        parts.push(format_keyword(&mutability_str));
      }
      parts.push(format_identifier(name));

      let decl = parts.join(" ");
      if let Some(val) = value
        && !constant
      {
        let val = do_node_to_source_text(val, indent_level);
        format!(
          "{} {} {}",
          decl,
          format_operator("="),
          indent(&val, indent_level)
        )
      } else {
        decl
      }
    }

    ASTNode::WhileStatement {
      condition, body, ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level);
      let body_str = if let Some(b) = body {
        do_node_to_source_text(b, indent_level)
      } else {
        format_brace("{}", indent_level)
      };
      format!(
        "{} ({}) {}",
        format_keyword("while"),
        indent(&cond, indent_level),
        body_str
      )
    }

    ASTNode::ContractDefinition {
      contract_kind,
      name,
      base_contracts,
      nodes,
      abstract_,
      ..
    } => {
      let kind = contract_kind_to_string(contract_kind);
      let abstract_str = if *abstract_ {
        format!("{} ", format_keyword("abstract"))
      } else {
        String::new()
      };
      let bases = if !base_contracts.is_empty() {
        let base_list = base_contracts
          .iter()
          .map(|b| do_node_to_source_text(b, indent_level))
          .collect::<Vec<_>>()
          .join(", ");
        format!(" {} {}", format_keyword("is"), base_list)
      } else {
        String::new()
      };

      if nodes.is_empty() {
        format!(
          "{}{} {}{} {}",
          abstract_str,
          format_keyword(&html_escape(&kind)),
          format_type(&html_escape(name)),
          bases,
          format_brace("{}", indent_level)
        )
      } else {
        let members = nodes
          .iter()
          .map(|n| do_node_to_source_text(n, indent_level + 1))
          .collect::<Vec<_>>()
          .join("\n\n");
        let trailing_newline = if !members.is_empty() { "\n" } else { "" };
        format!(
          "{}{} {}{} {}{}{}{}",
          abstract_str,
          format_keyword(&html_escape(&kind)),
          format_type(&html_escape(name)),
          bases,
          format_brace("{", indent_level),
          indent(&members, indent_level + 1),
          trailing_newline,
          format_brace("}", indent_level)
        )
      }
    }

    ASTNode::FunctionDefinition {
      kind,
      name,
      parameters,
      return_parameters,
      modifiers,
      visibility,
      state_mutability,
      virtual_,
      body,
      ..
    } => {
      let virtual_str = if *virtual_ {
        format!("{} ", format_keyword("virtual"))
      } else {
        String::new()
      };
      let visibility =
        format_keyword(&function_visibility_to_string(visibility));
      let mutability = format!(
        " {}",
        format_keyword(&function_mutability_to_string(state_mutability))
      );
      let kind_str =
        format!(" {}", format_keyword(&function_kind_to_string(kind)));
      let name_str = if name.is_empty() {
        String::new()
      } else {
        format!(" {}", format_function_name(name))
      };
      let params =
        format!("{} ", do_node_to_source_text(parameters, indent_level));
      let modifiers = if !modifiers.is_empty() {
        let indent_level = indent_level + 1;
        let mods = modifiers
          .iter()
          .map(|m| do_node_to_source_text(m, indent_level))
          .collect::<Vec<_>>()
          .join("\n");
        indent(&mods, indent_level)
      } else {
        String::new()
      };
      let returns = format!(
        "{} {} ",
        format_keyword("returns"),
        do_node_to_source_text(return_parameters, indent_level)
      );
      let body = if let Some(b) = body {
        do_node_to_source_text(b, indent_level)
      } else {
        String::new()
      };

      format!(
        "{}{}{}{}{}{}{}{}{}",
        virtual_str,
        visibility,
        mutability,
        kind_str,
        name_str,
        params,
        modifiers,
        returns,
        body
      )
    }

    ASTNode::EventDefinition {
      name, parameters, ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level);
      format!(
        "{} {}{}",
        format_keyword("event"),
        format_user_defined_type(name),
        params
      )
    }

    ASTNode::ErrorDefinition {
      name, parameters, ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level);
      format!(
        "{} {}{}",
        format_keyword("error"),
        format_user_defined_type(name),
        params
      )
    }

    ASTNode::ModifierDefinition {
      name,
      parameters,
      virtual_,
      visibility,
      body,
      ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level);
      let virtual_str = if *virtual_ {
        format!("{} ", format_keyword("virtual"))
      } else {
        String::new()
      };
      let visibility_str =
        format_keyword(&function_visibility_to_string(&visibility));

      format!(
        "{}{} {} {}{} {}",
        virtual_str,
        visibility_str,
        format_keyword("mod"),
        format_function_name(name),
        params,
        do_node_to_source_text(body, indent_level)
      )
    }

    ASTNode::StructDefinition {
      name,
      members,
      visibility,
      ..
    } => {
      let visibility_str =
        format_keyword(&variable_visibility_to_string(&visibility));
      let indent_level = indent_level + 1;
      let members_str = members
        .iter()
        .map(|m| do_node_to_source_text(m, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !members_str.is_empty() { "\n" } else { "" };
      format!(
        "{} {} {} {{{}{}}}",
        visibility_str,
        format_keyword("struct"),
        format_user_defined_type(name),
        indent(&members_str, indent_level),
        trailing_newline
      )
    }

    ASTNode::EnumDefinition { name, members, .. } => {
      let indent_level = indent_level + 1;

      let members_str = members
        .iter()
        .map(|m| do_node_to_source_text(m, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !members_str.is_empty() { "\n" } else { "" };
      format!(
        "{} {} {{{}{}}}",
        format_keyword("enum"),
        format_user_defined_type(name),
        indent(&members_str, indent_level),
        trailing_newline
      )
    }

    ASTNode::UserDefinedValueTypeDefinition {
      name,
      underlying_type,
      ..
    } => {
      format!(
        "type {} is {}",
        format_user_defined_type(name),
        do_node_to_source_text(underlying_type, indent_level)
      )
    }

    ASTNode::PragmaDirective { literals, .. } => {
      format!(
        "{} {}",
        format_keyword("pragma"),
        format_number(&literals.join("."))
      )
    }

    ASTNode::ImportDirective { file, .. } => {
      format!("{} {}", format_keyword("import"), format_string(&file))
    }

    ASTNode::UsingForDirective {
      library_name,
      type_name,
      ..
    } => {
      let lib = if let Some(lib_node) = library_name {
        do_node_to_source_text(lib_node, indent_level)
      } else {
        String::new()
      };
      let type_str = if let Some(type_node) = type_name {
        format!(
          " {} {}",
          format_keyword("for"),
          do_node_to_source_text(type_node, indent_level)
        )
      } else {
        String::new()
      };
      format!("{} {}{}", format_keyword("using"), lib, type_str)
    }

    ASTNode::SourceUnit { nodes, .. } => nodes
      .iter()
      .map(|n| do_node_to_source_text(n, indent_level))
      .collect::<Vec<_>>()
      .join("\n\n"),

    ASTNode::InheritanceSpecifier { base_name, .. } => {
      do_node_to_source_text(base_name, indent_level)
    }

    ASTNode::ElementaryTypeName { name, .. } => format_type(name),

    ASTNode::FunctionTypeName {
      parameter_types,
      return_parameter_types,
      visibility,
      state_mutability,
      ..
    } => {
      let params = do_node_to_source_text(parameter_types, indent_level);
      let visibility_str = function_visibility_to_string(visibility);
      let mutability_str = function_mutability_to_string(state_mutability);
      let returns_str = format!(
        "{} {}",
        format_keyword("returns"),
        do_node_to_source_text(return_parameter_types, indent_level)
      );

      format!(
        "{}{}{}{}{}",
        format_keyword("fn"),
        params,
        format_keyword(&visibility_str),
        format_keyword(&mutability_str),
        returns_str
      )
    }

    ASTNode::ParameterList { parameters, .. } => {
      if parameters.is_empty() {
        String::from("()")
      } else {
        let indent_level = indent_level + 1;
        let params = parameters
          .iter()
          .map(|p| do_node_to_source_text(p, indent_level))
          .collect::<Vec<_>>()
          .join("\n");
        let trailing_newline = if !params.is_empty() { "\n" } else { "" };
        format!("({}{})", indent(&params, indent_level), trailing_newline)
      }
    }

    ASTNode::TryCatchClause { .. } => "TryCatchClause placeholder".to_owned(),

    ASTNode::ModifierInvocation {
      modifier_name,
      arguments,
      ..
    } => {
      let name = do_node_to_source_text(modifier_name, indent_level);
      if let Some(args) = arguments {
        if args.is_empty() {
          format!("{}()", name)
        } else {
          let args_str = args
            .iter()
            .map(|a| do_node_to_source_text(a, indent_level))
            .collect::<Vec<_>>()
            .join("\n");
          format!("{}({})", name, indent(&args_str, indent_level + 1))
        }
      } else {
        name
      }
    }

    ASTNode::UserDefinedTypeName { path_node, .. } => {
      do_node_to_source_text(path_node, indent_level)
    }

    ASTNode::ArrayTypeName { base_type, .. } => {
      format!("{}[]", do_node_to_source_text(base_type, indent_level))
    }

    ASTNode::Mapping {
      key_type,
      key_name,
      value_type,
      value_name,
      ..
    } => {
      let mut key = do_node_to_source_text(key_type, indent_level);
      if let Some(name) = key_name {
        key = format!("{} {}", key, name)
      }
      let mut value = do_node_to_source_text(value_type, indent_level);
      if let Some(name) = value_name {
        value = format!("{} {}", value, name)
      }
      format!(
        "{}({})",
        format_keyword("map"),
        indent(
          &format!("{} {} {}", key, format_operator("=>"), value),
          indent_level + 1
        )
      )
    }

    ASTNode::StructuredDocumentation { .. } => String::new(),

    ASTNode::Stub { .. } => String::from("NodeStub"),

    ASTNode::Other { .. } => format_comment("Unknown"),
  };

  format_node(&node_str, node.node_id(), "node")
}

fn format_token(token: &str, class: &str) -> String {
  format!("<span class=\"{}\">{}</span>", class, token)
}

fn format_node(node_str: &str, id: i32, class: &str) -> String {
  format!("<span class=\"{} {}\">{}</span>", class, id, node_str)
}

fn format_keyword(keyword: &str) -> String {
  format_token(keyword, "keyword")
}

fn format_function_name(name: &String) -> String {
  format_token(name, "function")
}

fn format_identifier(name: &String) -> String {
  format_token(name, "identifier")
}

fn format_user_defined_type(type_name: &String) -> String {
  format_token(type_name, "user-type")
}

fn format_type(type_name: &String) -> String {
  format_token(type_name, "type")
}

fn format_member(name: &str) -> String {
  format_token(name, "member")
}

fn format_global(name: &str) -> String {
  format_token(name, "global")
}

fn format_enum_value(name: &str) -> String {
  format_token(name, "enum-value")
}

fn format_comment(text: &str) -> String {
  format_token(text, "comment")
}

fn format_number(val: &str) -> String {
  format_token(val, "number")
}

fn format_string(val: &str) -> String {
  format_token(val, "string")
}

fn format_operator(op: &str) -> String {
  format_token(&html_escape(&op), "operator")
}

fn format_brace(brace: &str, indent_level: usize) -> String {
  format!(
    "<span class=\"brace indent-level-{}\">{}</span>",
    indent_level, brace
  )
}

/// Creates an indentation wrapper with padding
/// This CSS should create the nested indentation
/// .indent {
///   display: inline-block;
///   padding-left: 12px;
///   border-left: solid grey 1px;
/// }
fn indent(content: &str, indent_level: usize) -> String {
  format!(
    "\n<span class=\"indent indent-level-{}\">{}</span>",
    indent_level, content
  )
}

/// Escapes HTML special characters
fn html_escape(s: &str) -> String {
  s.replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}

fn binary_operator_to_string(op: &BinaryOperator) -> String {
  match op {
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
  }
  .to_string()
}

fn is_math_operator(op: &BinaryOperator) -> bool {
  matches!(
    op,
    BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Modulo
      | BinaryOperator::Power
      | BinaryOperator::BitwiseAnd
      | BinaryOperator::BitwiseOr
      | BinaryOperator::BitwiseXor
      | BinaryOperator::LeftShift
      | BinaryOperator::RightShift
  )
}

fn is_boolean_and_or_operator(op: &BinaryOperator) -> bool {
  matches!(op, BinaryOperator::And | BinaryOperator::Or)
}

fn assignment_operator_to_string(op: &AssignmentOperator) -> String {
  match op {
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
  }
  .to_string()
}

fn unary_operator_to_string(op: &UnaryOperator) -> String {
  match op {
    UnaryOperator::Increment => "++",
    UnaryOperator::Decrement => "--",
    UnaryOperator::Plus => "+",
    UnaryOperator::Minus => "-",
    UnaryOperator::BitwiseNot => "~",
    UnaryOperator::Not => "!",
    UnaryOperator::Delete => "del ",
  }
  .to_string()
}

fn contract_kind_to_string(kind: &ContractKind) -> String {
  match kind {
    ContractKind::Contract => "contract",
    ContractKind::Library => "library",
    ContractKind::Abstract => "abstract",
    ContractKind::Interface => "interface",
  }
  .to_string()
}

fn function_kind_to_string(kind: &FunctionKind) -> String {
  match kind {
    FunctionKind::Constructor => "constructor",
    FunctionKind::Function => "fn",
    FunctionKind::Fallback => "fallback",
    FunctionKind::Receive => "receive",
    FunctionKind::FreeFunction => "fn",
  }
  .to_string()
}

fn function_visibility_to_string(visibility: &FunctionVisibility) -> String {
  match visibility {
    FunctionVisibility::Public => "pub",
    FunctionVisibility::Private => "priv",
    FunctionVisibility::Internal => "int",
    FunctionVisibility::External => "ext",
  }
  .to_string()
}

fn variable_visibility_to_string(visibility: &VariableVisibility) -> String {
  match visibility {
    VariableVisibility::Public => "pub",
    VariableVisibility::Private => "priv",
    VariableVisibility::Internal => "int",
  }
  .to_string()
}

fn function_mutability_to_string(
  mutability: &FunctionStateMutability,
) -> String {
  match mutability {
    FunctionStateMutability::Pure => "pure",
    FunctionStateMutability::View => "view",
    FunctionStateMutability::NonPayable => "nonpay",
    FunctionStateMutability::Payable => "pay",
  }
  .to_string()
}

fn variable_mutability_to_string(mutability: &VariableMutability) -> String {
  match mutability {
    VariableMutability::Mutable => "mut",
    VariableMutability::Immutable => "immut",
    VariableMutability::Constant => "const",
  }
  .to_string()
}

fn storage_location_to_string(location: &StorageLocation) -> String {
  match location {
    StorageLocation::Default => "",
    StorageLocation::Storage => "storage",
    StorageLocation::Memory => "memory",
    StorageLocation::Calldata => "calldata",
  }
  .to_string()
}
