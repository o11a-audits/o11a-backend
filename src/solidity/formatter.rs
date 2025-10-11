use foundry_compilers_artifacts::Visibility;

use crate::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, ContractKind, FunctionKind,
  FunctionStateMutability, FunctionVisibility, LiteralKind, StorageLocation,
  UnaryOperator, VariableMutability, VariableVisibility,
};

pub fn main() {
  println!("Hello, world!");
}

/// Converts an AST node and all its children to a formatted HTML string with syntax highlighting.
///
/// This function recursively processes nodes and uses div-based indentation with padding.
/// Binary operators are formatted with the left-hand side and operator on one line,
/// and the right-hand side indented on the next line.
pub fn node_to_html(node: &ASTNode) -> String {
  do_node_to_html(node, 0)
}

fn do_node_to_html(node: &ASTNode, indent_level: usize) -> String {
  let node_str = match node {
    ASTNode::Assignment {
      operator,
      left_hand_side,
      right_and_side,
      ..
    } => {
      let lhs = do_node_to_html(left_hand_side, indent_level);
      let op = assignment_operator_to_string(operator);

      let indent_level = indent_level + 1;
      let rhs = do_node_to_html(right_and_side, indent_level);
      format!(
        "{} {}{}",
        lhs,
        format_operator(&op),
        indent_div(&rhs, indent_level)
      )
    }

    ASTNode::BinaryOperation {
      left_expression,
      operator,
      right_expression,
      ..
    } => {
      let lhs = do_node_to_html(left_expression, indent_level);
      let op = binary_operator_to_string(operator);

      let indent_level = indent_level + 1;
      let rhs = do_node_to_html(right_expression, indent_level);
      format!(
        "{} {}{}",
        lhs,
        format_operator(&op),
        indent_div(&rhs, indent_level)
      )
    }

    ASTNode::Conditional {
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let cond = do_node_to_html(condition, indent_level);

      let indent_level = indent_level + 1;
      let part = if let Some(false_expr) = false_expression {
        format!(
          "\n{} {}\n{} {}",
          format_operator("?"),
          do_node_to_html(true_expression, indent_level),
          format_operator(":"),
          do_node_to_html(false_expr, indent_level),
        )
      } else {
        format!(
          "\n{} {}",
          format_operator("?"),
          do_node_to_html(true_expression, indent_level)
        )
      };

      format!("{}{}", cond, indent_div(&part, indent_level),)
    }

    ASTNode::ElementaryTypeNameExpression { type_name, .. } => {
      do_node_to_html(type_name, indent_level)
    }

    ASTNode::FunctionCall {
      expression,
      arguments,
      kind,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);

      let indent_level = indent_level + 1;
      let args = arguments
        .iter()
        .map(|arg| do_node_to_html(arg, indent_level))
        .collect::<Vec<_>>()
        .join("\n");

      if arguments.is_empty() {
        format!("{}()", expr)
      } else {
        format!("{}({})", expr, indent_div(&args, indent_level))
      }
    }

    ASTNode::FunctionCallOptions {
      expression,
      options,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);

      let indent_level = indent_level + 1;
      let opts = options
        .iter()
        .map(|opt| do_node_to_html(opt, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      format!("{}{{{}}}", expr, indent_div(&opts, indent_level))
    }

    ASTNode::Identifier { name, .. } => {
      format!("<span class=\"identifier\">{}</span>", html_escape(name))
    }

    ASTNode::IdentifierPath {
      name,
      referenced_declaration,
      ..
    } => {
      if referenced_declaration < &0 {
        format!("<span class=\"global\">{}</span>", html_escape(name))
      } else {
        format!("<span class=\"identifier\">{}</span>", html_escape(name))
      }
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let base = do_node_to_html(base_expression, indent_level);
      if let Some(idx) = index_expression {
        format!("{}[{}]", base, do_node_to_html(idx, indent_level))
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
      LiteralKind::String => {
        format!(
          "<span class=\"string\">\"{}\"</span>",
          html_escape(value.as_ref().unwrap_or(&hex_value))
        )
      }
      LiteralKind::HexString => {
        format!(
          "<span class=\"number\">0x{}</span>",
          html_escape(value.as_ref().unwrap_or(&hex_value))
        )
      }
      LiteralKind::Number | LiteralKind::Bool => {
        format!(
          "<span class=\"number\">{}</span>",
          html_escape(value.as_ref().unwrap_or(&hex_value))
        )
      }
    },

    ASTNode::MemberAccess {
      expression,
      member_name,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);

      let indent_level = indent_level + 1;
      let member = format!(
        "<span class=\"operator\">.</span><span class=\"member\">{}</span>",
        html_escape(member_name)
      );
      format!("{}{}", expr, indent_div(&member, indent_level),)
    }

    ASTNode::NewExpression { type_name, .. } => {
      format!(
        "{} {}",
        format_keyword("new"),
        do_node_to_html(type_name, indent_level)
      )
    }

    ASTNode::TupleExpression { components, .. } => {
      let indent_level = indent_level + 1;
      let comps = components
        .iter()
        .map(|c| do_node_to_html(c, indent_level))
        .collect::<Vec<_>>()
        .join(",\n");
      format!(
        "{}{}{}",
        format_brace("(", indent_level),
        indent_div(&comps, indent_level),
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
      let expr = do_node_to_html(sub_expression, indent_level);
      if *prefix {
        format!("{}{}", format_operator(&op), expr)
      } else {
        format!("{}{}", expr, format_operator(&op))
      }
    }

    ASTNode::EnumValue { name, .. } => {
      format!("<span class=\"enum-value\">{}</span>", html_escape(name))
    }

    ASTNode::Block { statements, .. } => {
      if statements.is_empty() {
        format_brace("{}", indent_level)
      } else {
        let indent_level = indent_level + 1;
        let stmts = statements
          .iter()
          .map(|s| do_node_to_html(s, indent_level))
          .collect::<Vec<_>>()
          .join("\n\n");
        format!(
          "{}{}{}",
          format_brace("{", indent_level),
          indent_div(&stmts, indent_level),
          format_brace("}", indent_level),
        )
      }
    }

    ASTNode::SemanticBlock { statements, .. } => statements
      .iter()
      .map(|s| do_node_to_html(s, indent_level))
      .collect::<Vec<_>>()
      .join("\n"),

    ASTNode::Break { .. } => format_keyword("break"),

    ASTNode::Continue { .. } => format_keyword("continue"),

    ASTNode::DoWhileStatement { nodes, body, .. } => {
      let body_str = if let Some(b) = body {
        do_node_to_html(b, indent_level)
      } else {
        String::new()
      };
      let condition = if !nodes.is_empty() {
        do_node_to_html(&nodes[0], indent_level)
      } else {
        String::new()
      };
      format!(
        "<span class=\"keyword\">do</span> {} <span class=\"keyword\">while</span> ({})",
        body_str, condition
      )
    }

    ASTNode::EmitStatement { event_call, .. } => {
      format!(
        "<span class=\"keyword\">emit</span> {};",
        do_node_to_html(event_call, indent_level)
      )
    }

    ASTNode::ExpressionStatement { expression, .. } => {
      do_node_to_html(expression, indent_level)
    }

    ASTNode::ForStatement {
      initialization_expression,
      condition,
      loop_expression,
      body,
      ..
    } => {
      let init = if let Some(init_expr) = initialization_expression {
        do_node_to_html(init_expr, indent_level)
      } else {
        String::new()
      };
      let cond = if let Some(cond_expr) = condition {
        do_node_to_html(cond_expr, indent_level)
      } else {
        String::new()
      };
      let loop_expr = if let Some(l_expr) = loop_expression {
        do_node_to_html(l_expr, indent_level)
      } else {
        String::new()
      };
      let body_str = do_node_to_html(body, indent_level);
      format!(
        "<span class=\"keyword\">for</span> ({}; {}; {}) {}",
        init, cond, loop_expr, body_str
      )
    }

    ASTNode::IfStatement {
      condition,
      true_body,
      false_body,
      ..
    } => {
      let cond = do_node_to_html(condition, indent_level);
      let true_b = do_node_to_html(true_body, indent_level);
      let false_part = if let Some(false_b) = false_body {
        format!(
          " <span class=\"keyword\">else</span> {}",
          do_node_to_html(false_b, indent_level)
        )
      } else {
        String::new()
      };
      format!(
        "<span class=\"keyword\">if</span> ({}) {}{}",
        indent_div(&cond, indent_level + 1),
        true_b,
        false_part
      )
    }

    ASTNode::InlineAssembly { .. } => {
      format!(
        "<span class=\"keyword\">assembly</span> <span class=\"brace\">{{}}</span>"
      )
    }

    ASTNode::PlaceholderStatement { .. } => {
      format!("<span class=\"keyword\">placeholder</span>;")
    }

    ASTNode::Return { expression, .. } => {
      if let Some(expr) = expression {
        format!(
          "<span class=\"keyword\">return</span> {}",
          do_node_to_html(expr, indent_level)
        )
      } else {
        format!("<span class=\"keyword\">return</span>")
      }
    }

    ASTNode::RevertStatement { error_call, .. } => {
      format!(
        "<span class=\"keyword\">revert</span> {}",
        do_node_to_html(error_call, indent_level)
      )
    }

    ASTNode::TryStatement {
      external_call,
      clauses,
      ..
    } => {
      let call = do_node_to_html(external_call, indent_level);
      let clauses_str = clauses
        .iter()
        .map(|c| do_node_to_html(c, indent_level))
        .collect::<Vec<_>>()
        .join(" ");
      format!(
        "<span class=\"keyword\">try</span> {} {}",
        call, clauses_str
      )
    }

    ASTNode::UncheckedBlock { statements, .. } => {
      let indent_level = indent_level + 1;
      if statements.is_empty() {
        format!(
          "<span class=\"keyword\">unchecked</span> <span class=\"brace\">{{}}</span>"
        )
      } else {
        let stmts = statements
          .iter()
          .map(|s| do_node_to_html(s, indent_level))
          .collect::<Vec<_>>()
          .join("\n");
        format!(
          "<span class=\"keyword\">unchecked</span> <span class=\"brace\">{{</span>{}<span class=\"brace\">}}</span>",
          indent_div(&stmts, indent_level)
        )
      }
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
              "{} <span class=\"operator\">=</span> {}",
              do_node_to_html(d, indent_level),
              do_node_to_html(init, indent_level),
            )
          })
          .collect::<Vec<_>>()
          .join("\n")
      } else {
        declarations
          .iter()
          .map(|d| do_node_to_html(d, indent_level))
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
      ..
    } => {
      let type_str = do_node_to_html(type_name, indent_level);
      let storage = storage_location_to_string(storage_location);
      let visibility_str = variable_visibility_to_string(visibility);
      let mutability_str = variable_mutability_to_string(mutability);

      let mut parts = vec![type_str];
      if !storage.is_empty() {
        parts.push(format!(
          "<span class=\"keyword\">{}</span>",
          html_escape(&storage)
        ));
      }
      if !visibility_str.is_empty() {
        parts.push(format!(
          "<span class=\"keyword\">{}</span>",
          html_escape(&visibility_str)
        ));
      }
      if !mutability_str.is_empty() {
        parts.push(format!(
          "<span class=\"keyword\">{}</span>",
          html_escape(&mutability_str)
        ));
      }
      parts.push(format!(
        "<span class=\"identifier\">{}</span>",
        html_escape(name)
      ));

      let decl = parts.join(" ");
      if let Some(val) = value {
        let val = do_node_to_html(val, indent_level);
        format!(
          "{} <span class=\"operator\">=</span> {}",
          decl,
          indent_div(&val, indent_level)
        )
      } else {
        decl
      }
    }

    ASTNode::WhileStatement {
      condition, body, ..
    } => {
      let cond = do_node_to_html(condition, indent_level);
      let body_str = if let Some(b) = body {
        do_node_to_html(b, indent_level)
      } else {
        format!("<span class=\"brace\">{{}}</span>")
      };
      format!(
        "<span class=\"keyword\">while</span> ({}) {}",
        cond, body_str
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
        "<span class=\"keyword\">abstract</span> "
      } else {
        ""
      };
      let bases = if !base_contracts.is_empty() {
        let base_list = base_contracts
          .iter()
          .map(|b| do_node_to_html(b, indent_level))
          .collect::<Vec<_>>()
          .join(", ");
        format!(" <span class=\"keyword\">is</span> {}", base_list)
      } else {
        String::new()
      };

      if nodes.is_empty() {
        format!(
          "{}<span class=\"keyword\">{}</span> <span class=\"type\">{}</span>{} <span class=\"brace\">{{}}</span>",
          abstract_str,
          html_escape(&kind),
          html_escape(name),
          bases
        )
      } else {
        let members = nodes
          .iter()
          .map(|n| do_node_to_html(n, indent_level + 1))
          .collect::<Vec<_>>()
          .join("\n\n");
        format!(
          "{}<span class=\"keyword\">{}</span> <span class=\"type\">{}</span>{} <span class=\"brace\">{{</span>{}<span class=\"brace\">}}</span>",
          abstract_str,
          html_escape(&kind),
          html_escape(name),
          bases,
          indent_div(&members, indent_level + 1)
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
      let params = format!("{} ", do_node_to_html(parameters, indent_level));
      let modifiers = if !modifiers.is_empty() {
        let indent_level = indent_level + 1;
        let mods = modifiers
          .iter()
          .map(|m| do_node_to_html(m, indent_level))
          .collect::<Vec<_>>()
          .join("\n");
        indent_div(&mods, indent_level)
      } else {
        String::new()
      };
      let returns = format!(
        "{} {} ",
        format_keyword("returns"),
        do_node_to_html(return_parameters, indent_level)
      );
      let body = if let Some(b) = body {
        do_node_to_html(b, indent_level)
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
      let params = do_node_to_html(parameters, indent_level);
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
      let params = do_node_to_html(parameters, indent_level);
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
      let params = do_node_to_html(parameters, indent_level);
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
        do_node_to_html(body, indent_level)
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
        .map(|m| do_node_to_html(m, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      format!(
        "{} {} {} {{{}}}",
        visibility_str,
        format_keyword("struct"),
        format_user_defined_type(name),
        indent_div(&members_str, indent_level)
      )
    }

    ASTNode::EnumDefinition { name, members, .. } => {
      let indent_level = indent_level + 1;

      let members_str = members
        .iter()
        .map(|m| do_node_to_html(m, indent_level))
        .collect::<Vec<_>>()
        .join("\n");
      format!(
        "{} {} {{{}}}",
        format_keyword("enum"),
        format_user_defined_type(name),
        indent_div(&members_str, indent_level)
      )
    }

    ASTNode::UserDefinedValueTypeDefinition { .. } => {
      "UserDefinedValueTypeDefinition placeholder".to_owned()
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
        do_node_to_html(lib_node, indent_level)
      } else {
        String::new()
      };
      let type_str = if let Some(type_node) = type_name {
        format!(
          " {} {}",
          format_keyword("for"),
          do_node_to_html(type_node, indent_level)
        )
      } else {
        String::new()
      };
      format!("{} {}{}", format_keyword("using"), lib, type_str)
    }

    ASTNode::SourceUnit { nodes, .. } => nodes
      .iter()
      .map(|n| do_node_to_html(n, indent_level))
      .collect::<Vec<_>>()
      .join("\n\n"),

    ASTNode::InheritanceSpecifier { base_name, .. } => {
      do_node_to_html(base_name, indent_level)
    }

    ASTNode::ElementaryTypeName { name, .. } => format_type(name),

    ASTNode::FunctionTypeName {
      parameter_types,
      return_parameter_types,
      visibility,
      state_mutability,
      ..
    } => {
      let params = do_node_to_html(parameter_types, indent_level);
      let visibility_str = function_visibility_to_string(visibility);
      let mutability_str = function_mutability_to_string(state_mutability);
      let returns_str = format!(
        "{} {}",
        format_keyword("returns"),
        do_node_to_html(return_parameter_types, indent_level)
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
          .map(|p| do_node_to_html(p, indent_level))
          .collect::<Vec<_>>()
          .join("\n");
        format!("({})", indent_div(&params, indent_level))
      }
    }

    ASTNode::TryCatchClause { .. } => "TryCatchClause placeholder".to_owned(),

    ASTNode::ModifierInvocation {
      modifier_name,
      arguments,
      ..
    } => {
      let name = do_node_to_html(modifier_name, indent_level);
      if let Some(args) = arguments {
        if args.is_empty() {
          format!("{}()", name)
        } else {
          let args_str = args
            .iter()
            .map(|a| do_node_to_html(a, indent_level))
            .collect::<Vec<_>>()
            .join("\n");
          format!("{}({})", name, indent_div(&args_str, indent_level + 1))
        }
      } else {
        name
      }
    }

    ASTNode::UserDefinedTypeName { path_node, .. } => {
      do_node_to_html(path_node, indent_level)
    }

    ASTNode::ArrayTypeName { base_type, .. } => {
      format!("{}[]", do_node_to_html(base_type, indent_level))
    }

    ASTNode::Mapping {
      key_type,
      key_name,
      value_type,
      value_name,
      ..
    } => {
      let mut key = do_node_to_html(key_type, indent_level);
      if let Some(name) = key_name {
        key = format!("{} {}", key, name)
      }
      let mut value = do_node_to_html(value_type, indent_level);
      if let Some(name) = value_name {
        value = format!("{} {}", value, name)
      }
      format!(
        "{} ({} {} {})",
        format_keyword("map"),
        key,
        format_operator("=>"),
        value,
      )
    }

    ASTNode::StructuredDocumentation { .. } => String::new(),

    ASTNode::Other { .. } => {
      format!("<span class=\"comment\">/* unknown node */</span>")
    }
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

fn format_user_defined_type(type_name: &String) -> String {
  format_token(type_name, "user-type")
}

fn format_type(type_name: &String) -> String {
  format_token(type_name, "type")
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

/// Creates an indentation wrapper div with padding
/// This CSS should create the nested indentation
/// .indent {
///   display: inline-block;
///   padding-left: 12px;
///   border-left: solid grey 1px;
/// }
fn indent_div(content: &str, indent_level: usize) -> String {
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
