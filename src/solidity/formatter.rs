use crate::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, ContractKind, FunctionKind, FunctionStateMutability,
  FunctionVisibility, LiteralKind, StorageLocation, UnaryOperator, VariableMutability,
  VariableVisibility,
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
  match node {
    // ========== Expression Nodes ==========
    ASTNode::Assignment {
      operator,
      left_hand_side,
      right_and_side,
      ..
    } => {
      let lhs = do_node_to_html(left_hand_side, indent_level);
      let op = assignment_operator_to_string(operator);
      let rhs = do_node_to_html(right_and_side, indent_level + 1);
      format!(
        "{} <span class=\"operator\">{}</span>\n{}",
        lhs,
        html_escape(&op),
        indent_div(&rhs, indent_level + 1)
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
      let rhs = do_node_to_html(right_expression, indent_level + 1);
      format!(
        "{} <span class=\"operator\">{}</span>{}",
        lhs,
        html_escape(&op),
        indent_div(&rhs, indent_level + 1)
      )
    }

    ASTNode::Conditional {
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let cond = do_node_to_html(condition, indent_level);
      let true_expr = do_node_to_html(true_expression, indent_level + 1);
      let false_part = if let Some(false_expr) = false_expression {
        format!(
          "{}<span class=\"operator\">:</span>{}",
          indent_str(indent_level),
          indent_div(
            &do_node_to_html(false_expr, indent_level + 1),
            indent_level + 1
          )
        )
      } else {
        String::new()
      };
      format!(
        "{} <span class=\"operator\">?</span>{}{}",
        cond,
        indent_div(&true_expr, indent_level + 1),
        false_part
      )
    }

    ASTNode::ElementaryTypeNameExpression { type_name, .. } => {
      format!(
        "<span class=\"type\">{}</span>",
        do_node_to_html(type_name, indent_level)
      )
    }

    ASTNode::FunctionCall {
      expression,
      arguments,
      names,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);
      let args = if !names.is_empty() {
        // Named arguments
        arguments
          .iter()
          .zip(names.iter())
          .map(|(arg, name)| {
            format!(
              "{}<span class=\"parameter\">{}</span>: {}",
              indent_str(indent_level + 1),
              html_escape(name),
              do_node_to_html(arg, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join(",\n")
      } else {
        // Positional arguments
        arguments
          .iter()
          .map(|arg| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(arg, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join(",\n")
      };

      if arguments.is_empty() {
        format!("{}()", expr)
      } else {
        format!("{}(\n{}\n{})", expr, args, indent_str(indent_level))
      }
    }

    ASTNode::FunctionCallOptions {
      expression,
      options,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);
      let opts = options
        .iter()
        .map(|opt| do_node_to_html(opt, indent_level + 1))
        .collect::<Vec<_>>()
        .join(", ");
      format!("{}{{{}}}", expr, opts)
    }

    ASTNode::Identifier { name, .. } => {
      format!("<span class=\"identifier\">{}</span>", html_escape(name))
    }

    ASTNode::IdentifierPath { name, .. } => {
      format!("<span class=\"identifier\">{}</span>", html_escape(name))
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let base = do_node_to_html(base_expression, indent_level);
      let index = if let Some(idx) = index_expression {
        do_node_to_html(idx, indent_level)
      } else {
        String::new()
      };
      format!("{}[{}]", base, index)
    }

    ASTNode::IndexRangeAccess { nodes, body, .. } => {
      let nodes_str = nodes
        .iter()
        .map(|n| do_node_to_html(n, indent_level))
        .collect::<Vec<_>>()
        .join(":");
      if let Some(b) = body {
        format!("[{}:{}", nodes_str, do_node_to_html(b, indent_level))
      } else {
        format!("[{}]", nodes_str)
      }
    }

    ASTNode::Literal {
      kind,
      value,
      hex_value,
      ..
    } => {
      let display_value = match kind {
        LiteralKind::String => {
          format!("\"{}\"", html_escape(value.as_ref().unwrap_or(&hex_value)))
        }
        LiteralKind::HexString => {
          format!(
            "hex\"{}\"",
            html_escape(value.as_ref().unwrap_or(&hex_value))
          )
        }
        LiteralKind::Number => html_escape(value.as_ref().unwrap_or(&hex_value)),
        LiteralKind::Bool => html_escape(value.as_ref().unwrap_or(&hex_value)),
      };
      format!("<span class=\"literal\">{}</span>", display_value)
    }

    ASTNode::MemberAccess {
      expression,
      member_name,
      ..
    } => {
      let expr = do_node_to_html(expression, indent_level);
      format!(
        "{}<span class=\"operator\">.</span><span class=\"member\">{}</span>",
        expr,
        html_escape(member_name)
      )
    }

    ASTNode::NewExpression { type_name, .. } => {
      format!(
        "<span class=\"keyword\">new</span> {}",
        do_node_to_html(type_name, indent_level)
      )
    }

    ASTNode::TupleExpression { components, .. } => {
      let comps = components
        .iter()
        .map(|c| do_node_to_html(c, indent_level))
        .collect::<Vec<_>>()
        .join(", ");
      format!("({})", comps)
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
        format!(
          "<span class=\"operator\">{}</span>{}",
          html_escape(&op),
          expr
        )
      } else {
        format!(
          "{}<span class=\"operator\">{}</span>",
          expr,
          html_escape(&op)
        )
      }
    }

    ASTNode::EnumValue { name, .. } => {
      format!("<span class=\"enum-value\">{}</span>", html_escape(name))
    }

    // ========== Statement Nodes ==========
    ASTNode::Block { statements, .. } => {
      if statements.is_empty() {
        format!(
          "{}<span class=\"brace\">{{}}</span>",
          indent_str(indent_level)
        )
      } else {
        let stmts = statements
          .iter()
          .map(|s| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(s, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join("\n");
        format!(
          "{}<span class=\"brace\">{{</span>\n{}\n{}<span class=\"brace\">}}</span>",
          indent_str(indent_level),
          stmts,
          indent_str(indent_level)
        )
      }
    }

    ASTNode::SemanticBlock {
      documentation,
      statements,
      ..
    } => {
      let mut result = String::new();
      if let Some(doc) = documentation {
        result.push_str(&format!(
          "{}<span class=\"comment\">{}</span>\n",
          indent_str(indent_level),
          html_escape(doc)
        ));
      }
      if statements.is_empty() {
        result.push_str(&format!(
          "{}<span class=\"brace\">{{}}</span>",
          indent_str(indent_level)
        ));
      } else {
        let stmts = statements
          .iter()
          .map(|s| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(s, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join("\n");
        result.push_str(&format!(
          "{}<span class=\"brace\">{{</span>\n{}\n{}<span class=\"brace\">}}</span>",
          indent_str(indent_level),
          stmts,
          indent_str(indent_level)
        ));
      }
      result
    }

    ASTNode::Break { .. } => {
      format!("<span class=\"keyword\">break</span>;")
    }

    ASTNode::Continue { .. } => {
      format!("<span class=\"keyword\">continue</span>;")
    }

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
      format!("{};", do_node_to_html(expression, indent_level))
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
        cond, true_b, false_part
      )
    }

    ASTNode::InlineAssembly { .. } => {
      format!("<span class=\"keyword\">assembly</span> <span class=\"brace\">{{}}</span>")
    }

    ASTNode::PlaceholderStatement { .. } => {
      format!("<span class=\"keyword\">_</span>;")
    }

    ASTNode::Return { expression, .. } => {
      if let Some(expr) = expression {
        format!(
          "<span class=\"keyword\">return</span> {};",
          do_node_to_html(expr, indent_level)
        )
      } else {
        format!("<span class=\"keyword\">return</span>;")
      }
    }

    ASTNode::RevertStatement { error_call, .. } => {
      format!(
        "<span class=\"keyword\">revert</span> {};",
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
      if statements.is_empty() {
        format!("<span class=\"keyword\">unchecked</span> <span class=\"brace\">{{}}</span>")
      } else {
        let stmts = statements
          .iter()
          .map(|s| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(s, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join("\n");
        format!(
          "<span class=\"keyword\">unchecked</span> <span class=\"brace\">{{</span>\n{}\n{}<span class=\"brace\">}}</span>",
          stmts,
          indent_str(indent_level)
        )
      }
    }

    ASTNode::VariableDeclarationStatement {
      declarations,
      initial_value,
      ..
    } => {
      let decls = declarations
        .iter()
        .map(|d| do_node_to_html(d, indent_level))
        .collect::<Vec<_>>()
        .join(", ");
      if let Some(init) = initial_value {
        format!(
          "{} <span class=\"operator\">=</span> {};",
          decls,
          do_node_to_html(init, indent_level)
        )
      } else {
        format!("{};", decls)
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
        format!(
          "{} <span class=\"operator\">=</span> {}",
          decl,
          do_node_to_html(val, indent_level)
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

    // ========== Definition Nodes ==========
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
          .map(|n| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(n, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join("\n");
        format!(
          "{}<span class=\"keyword\">{}</span> <span class=\"type\">{}</span>{} <span class=\"brace\">{{</span>\n{}\n<span class=\"brace\">}}</span>",
          abstract_str,
          html_escape(&kind),
          html_escape(name),
          bases,
          members
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
      let kind_str = function_kind_to_string(kind);
      let name_str = if name.is_empty() {
        String::new()
      } else {
        format!(
          " <span class=\"function-name\">{}</span>",
          html_escape(name)
        )
      };
      let params = do_node_to_html(parameters, indent_level);
      let visibility_str = function_visibility_to_string(visibility);
      let mutability_str = function_mutability_to_string(state_mutability);
      let virtual_str = if *virtual_ {
        " <span class=\"keyword\">virtual</span>"
      } else {
        ""
      };
      let modifiers_str = if !modifiers.is_empty() {
        let mods = modifiers
          .iter()
          .map(|m| do_node_to_html(m, indent_level))
          .collect::<Vec<_>>()
          .join(" ");
        format!(" {}", mods)
      } else {
        String::new()
      };
      let returns = do_node_to_html(return_parameters, indent_level);
      let returns_str = if returns.contains("()") {
        String::new()
      } else {
        format!(" <span class=\"keyword\">returns</span> {}", returns)
      };

      let signature = format!(
        "<span class=\"keyword\">{}</span>{}{}{}{}{}{}{}",
        html_escape(&kind_str),
        name_str,
        params,
        if visibility_str.is_empty() {
          String::new()
        } else {
          format!(
            " <span class=\"keyword\">{}</span>",
            html_escape(&visibility_str)
          )
        },
        if mutability_str.is_empty() {
          String::new()
        } else {
          format!(
            " <span class=\"keyword\">{}</span>",
            html_escape(&mutability_str)
          )
        },
        virtual_str,
        modifiers_str,
        returns_str
      );

      if let Some(b) = body {
        format!("{} {}", signature, do_node_to_html(b, indent_level))
      } else {
        format!("{};", signature)
      }
    }

    ASTNode::EventDefinition {
      name, parameters, ..
    } => {
      let params = do_node_to_html(parameters, indent_level);
      format!(
        "<span class=\"keyword\">event</span> <span class=\"type\">{}</span>{};",
        html_escape(name),
        params
      )
    }

    ASTNode::ErrorDefinition {
      name, parameters, ..
    } => {
      let params = do_node_to_html(parameters, indent_level);
      format!(
        "<span class=\"keyword\">error</span> <span class=\"type\">{}</span>{};",
        html_escape(name),
        params
      )
    }

    ASTNode::ModifierDefinition {
      name,
      parameters,
      virtual_,
      body,
      ..
    } => {
      let params = do_node_to_html(parameters, indent_level);
      let virtual_str = if *virtual_ {
        " <span class=\"keyword\">virtual</span>"
      } else {
        ""
      };
      let signature = format!(
        "<span class=\"keyword\">modifier</span> <span class=\"function-name\">{}</span>{}{}",
        html_escape(name),
        params,
        virtual_str,
      );

      format!("{} {}", signature, do_node_to_html(body, indent_level))
    }

    ASTNode::StructDefinition { name, members, .. } => {
      if members.is_empty() {
        format!(
          "<span class=\"keyword\">struct</span> <span class=\"type\">{}</span> <span class=\"brace\">{{}}</span>",
          html_escape(name)
        )
      } else {
        let members_str = members
          .iter()
          .map(|m| {
            format!(
              "{}{};",
              indent_str(indent_level + 1),
              do_node_to_html(m, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join("\n");
        format!(
          "<span class=\"keyword\">struct</span> <span class=\"type\">{}</span> <span class=\"brace\">{{</span>\n{}\n{}<span class=\"brace\">}}</span>",
          html_escape(name),
          members_str,
          indent_str(indent_level)
        )
      }
    }

    ASTNode::EnumDefinition { name, members, .. } => {
      if members.is_empty() {
        format!(
          "<span class=\"keyword\">enum</span> <span class=\"type\">{}</span> <span class=\"brace\">{{}}</span>",
          html_escape(name)
        )
      } else {
        let members_str = members
          .iter()
          .map(|m| {
            format!(
              "{}{}",
              indent_str(indent_level + 1),
              do_node_to_html(m, indent_level + 1)
            )
          })
          .collect::<Vec<_>>()
          .join(",\n");
        format!(
          "<span class=\"keyword\">enum</span> <span class=\"type\">{}</span> <span class=\"brace\">{{</span>\n{}\n{}<span class=\"brace\">}}</span>",
          html_escape(name),
          members_str,
          indent_str(indent_level)
        )
      }
    }

    ASTNode::UserDefinedValueTypeDefinition { .. } => {
      "UserDefinedValueTypeDefinition placeholder".to_owned()
    }

    // ========== Directive Nodes ==========
    ASTNode::PragmaDirective { literals, .. } => {
      let pragma_str = literals.join(" ");
      format!(
        "<span class=\"keyword\">pragma</span> <span class=\"pragma\">{}</span>;",
        html_escape(&pragma_str)
      )
    }

    ASTNode::ImportDirective { file, .. } => {
      format!(
        "<span class=\"keyword\">import</span> <span class=\"string\">\"{}\"</span>;",
        html_escape(file)
      )
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
          " <span class=\"keyword\">for</span> {}",
          do_node_to_html(type_node, indent_level)
        )
      } else {
        String::new()
      };
      format!("<span class=\"keyword\">using</span> {}{};", lib, type_str)
    }

    // ========== Other Nodes ==========
    ASTNode::SourceUnit { nodes, .. } => nodes
      .iter()
      .map(|n| do_node_to_html(n, indent_level))
      .collect::<Vec<_>>()
      .join("\n\n"),

    ASTNode::InheritanceSpecifier { base_name, .. } => do_node_to_html(base_name, indent_level),

    ASTNode::ElementaryTypeName { name, .. } => {
      format!("<span class=\"type\">{}</span>", html_escape(name))
    }

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
      let returns = do_node_to_html(return_parameter_types, indent_level);
      let returns_str = if returns.contains("()") {
        String::new()
      } else {
        format!(" <span class=\"keyword\">returns</span> {}", returns)
      };
      format!(
        "<span class=\"keyword\">function</span>{}{}{}{}",
        params,
        if visibility_str.is_empty() {
          String::new()
        } else {
          format!(
            " <span class=\"keyword\">{}</span>",
            html_escape(&visibility_str)
          )
        },
        if mutability_str.is_empty() {
          String::new()
        } else {
          format!(
            " <span class=\"keyword\">{}</span>",
            html_escape(&mutability_str)
          )
        },
        returns_str
      )
    }

    ASTNode::ParameterList { parameters, .. } => {
      if parameters.is_empty() {
        String::from("()")
      } else {
        let params = parameters
          .iter()
          .map(|p| do_node_to_html(p, indent_level))
          .collect::<Vec<_>>()
          .join(", ");
        format!("({})", params)
      }
    }

    ASTNode::TryCatchClause {
      block,
      error_name,
      parameters,
      ..
    } => {
      let params = if let Some(p) = parameters {
        do_node_to_html(p, indent_level)
      } else {
        String::new()
      };
      let name_str = if !error_name.is_empty() {
        format!(
          " <span class=\"identifier\">{}</span>",
          html_escape(error_name)
        )
      } else {
        String::new()
      };
      let block_str = do_node_to_html(block, indent_level);
      format!(
        "<span class=\"keyword\">catch</span>{}{} {}",
        name_str, params, block_str
      )
    }

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
            .join(", ");
          format!("{}({})", name, args_str)
        }
      } else {
        name
      }
    }

    ASTNode::UserDefinedTypeName { path_node, .. } => do_node_to_html(path_node, indent_level),

    ASTNode::ArrayTypeName { base_type, .. } => {
      let base = do_node_to_html(base_type, indent_level);
      format!("{}[]", base)
    }

    ASTNode::Mapping {
      key_type,
      value_type,
      ..
    } => {
      let key = do_node_to_html(key_type, indent_level);
      let value = do_node_to_html(value_type, indent_level);
      format!(
        "<span class=\"keyword\">mapping</span>({} <span class=\"operator\">=&gt;</span> {})",
        key, value
      )
    }

    ASTNode::StructuredDocumentation { text, .. } => {
      format!("<span class=\"comment\">{}</span>", html_escape(text))
    }

    ASTNode::Other { .. } => {
      format!("<span class=\"comment\">/* unknown node */</span>")
    }
  }
}

// ========== Helper Functions ==========

/// Creates an indentation wrapper div with padding
fn indent_div(content: &str, indent_level: usize) -> String {
  format!(
    "<div class=\"indent indent-level-{}\">{}</div>",
    indent_level, content
  )
}

/// Creates an indentation string using non-breaking spaces
fn indent_str(indent_level: usize) -> String {
  "&nbsp;".repeat(indent_level * 2)
}

/// Escapes HTML special characters
fn html_escape(s: &str) -> String {
  s.replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}

// ========== Operator and Enum to String Conversions ==========

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
    UnaryOperator::Delete => "delete",
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
    FunctionKind::Function => "function",
    FunctionKind::Fallback => "fallback",
    FunctionKind::Receive => "receive",
    FunctionKind::FreeFunction => "function",
  }
  .to_string()
}

fn function_visibility_to_string(visibility: &FunctionVisibility) -> String {
  match visibility {
    FunctionVisibility::Public => "public",
    FunctionVisibility::Private => "private",
    FunctionVisibility::Internal => "internal",
    FunctionVisibility::External => "external",
  }
  .to_string()
}

fn variable_visibility_to_string(visibility: &VariableVisibility) -> String {
  match visibility {
    VariableVisibility::Public => "public",
    VariableVisibility::Private => "private",
    VariableVisibility::Internal => "internal",
  }
  .to_string()
}

fn function_mutability_to_string(mutability: &FunctionStateMutability) -> String {
  match mutability {
    FunctionStateMutability::Pure => "pure",
    FunctionStateMutability::View => "view",
    FunctionStateMutability::NonPayable => "",
    FunctionStateMutability::Payable => "payable",
  }
  .to_string()
}

fn variable_mutability_to_string(mutability: &VariableMutability) -> String {
  match mutability {
    VariableMutability::Mutable => "",
    VariableMutability::Immutable => "immutable",
    VariableMutability::Constant => "constant",
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
