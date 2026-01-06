use crate::core;
use crate::core::topic::{self, new_node_topic};
use crate::core::{ContractKind, FunctionKind};
use crate::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, FunctionCallKind,
  FunctionStateMutability, FunctionVisibility, LiteralKind, StorageLocation,
  UnaryOperator, VariableMutability, VariableVisibility,
};
use std::collections::BTreeMap;

/// Converts an AST node and all its children to a formatted HTML string with syntax highlighting.
///
/// This function recursively processes nodes and uses div-based indentation with padding.
/// Binary operators are formatted with the left-hand side and operator on one line,
/// and the right-hand side indented on the next line.
pub fn node_to_source_text(
  node: &ASTNode,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  format!(
    "<pre><code>{}</code></pre>",
    do_node_to_source_text(node, 0, nodes_map)
  )
}

fn do_node_to_source_text(
  node: &ASTNode,
  indent_level: usize,
  nodes_map: &BTreeMap<topic::Topic, core::Node>,
) -> String {
  let node_str = match node.resolve(nodes_map) {
    ASTNode::Assignment {
      node_id,
      operator,
      left_hand_side,
      right_hand_side,
      ..
    } => {
      let lhs = do_node_to_source_text(left_hand_side, indent_level, nodes_map);
      let op = assignment_operator_to_string(operator);

      let indent_level = indent_level + 1;
      let rhs =
        do_node_to_source_text(right_hand_side, indent_level, nodes_map);
      format!(
        "{} {}{}",
        lhs,
        format_topic_operator(&op, &new_node_topic(node_id)),
        indent(&rhs, indent_level)
      )
    }

    ASTNode::BinaryOperation {
      node_id,
      left_expression,
      operator,
      right_expression,
      ..
    } => {
      let lhs =
        do_node_to_source_text(left_expression, indent_level, nodes_map);
      let op = binary_operator_to_string(operator);

      if is_boolean_and_or_operator(operator) || is_math_operator(operator) {
        // Boolean &&, ||, and math operators do not indent, but place the
        // operator and rhs on the next line with the same indentation level
        // of the lhs
        let rhs =
          do_node_to_source_text(right_expression, indent_level, nodes_map);
        format!(
          "{}\n{}",
          lhs,
          format!(
            "{} {}",
            format_topic_operator(&op, &new_node_topic(node_id)),
            &rhs
          ),
        )
      } else {
        let indent_level = indent_level + 1;
        let rhs =
          do_node_to_source_text(right_expression, indent_level, nodes_map);
        format!(
          "{}{}",
          lhs,
          indent(
            &format!(
              "{} {}",
              format_topic_operator(&op, &new_node_topic(node_id)),
              &rhs
            ),
            indent_level
          )
        )
      }
    }

    ASTNode::Conditional {
      node_id,
      condition,
      true_expression,
      false_expression,
      ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level, nodes_map);

      let indent_level = indent_level + 1;
      let part = if let Some(false_expr) = false_expression {
        format!(
          "\n{} {}\n{} {}",
          format_topic_operator("?", &new_node_topic(node_id)),
          do_node_to_source_text(true_expression, indent_level, nodes_map),
          format_operator(":"),
          do_node_to_source_text(false_expr, indent_level, nodes_map),
        )
      } else {
        format!(
          "\n{} {}",
          format_topic_operator("?", &new_node_topic(node_id)),
          do_node_to_source_text(true_expression, indent_level, nodes_map)
        )
      };

      format!("{}{}", cond, part)
    }

    ASTNode::ElementaryTypeNameExpression { type_name, .. } => {
      do_node_to_source_text(type_name, indent_level, nodes_map)
    }

    ASTNode::FunctionCall {
      node_id,
      expression,
      arguments,
      kind,
      ..
    } => {
      let expression = expression.resolve(nodes_map);

      // Check if this is address(0) - a type conversion to address with
      // literal value 0, or address(this)
      let special_case = if matches!(kind, FunctionCallKind::TypeConversion)
        && arguments.len() == 1
      {
        if let ASTNode::ElementaryTypeNameExpression { type_name, .. } =
          expression
        {
          let resolved_type_name = type_name.resolve(nodes_map);
          if matches!(resolved_type_name, ASTNode::ElementaryTypeName { name, .. } if name == "address")
          {
            let val = match arguments[0].resolve(nodes_map) {
              ASTNode::Identifier {
                name,
                referenced_declaration,
                ..
              } if name == "this" && *referenced_declaration == -28 => {
                format!(
                  "{}.{}",
                  format_global("this"),
                  format_global("address")
                )
              }
              n => format!(
                "{} {}",
                format_topic_number("address", &new_node_topic(&node_id)),
                do_node_to_source_text(n, indent_level, nodes_map)
              ),
            };

            Option::Some(val)
          } else {
            Option::None
          }
        } else {
          Option::None
        }
      } else {
        Option::None
      };

      if let Some(literal) = special_case {
        literal
      } else {
        let expr = do_node_to_source_text(expression, indent_level, nodes_map);

        let indent_level = indent_level + 1;
        let args = arguments
          .iter()
          .map(|arg| do_node_to_source_text(arg, indent_level, nodes_map))
          .collect::<Vec<_>>()
          .join("\n");

        if arguments.is_empty() {
          format!("{}()", expr)
        } else {
          let indented_args = match expression {
            // If the expression is a member access, indent the arguments twice to
            // match the member access indent
            ASTNode::MemberAccess { .. } => {
              format!(
                "({}",
                indent(
                  &format!("{}\n)", inline_indent(&args, indent_level)),
                  indent_level
                )
              )
            }
            _ => format!("({}\n)", indent(&args, indent_level)),
          };

          format!("{}{}", expr, indented_args)
        }
      }
    }

    ASTNode::FunctionCallOptions {
      expression,
      options,
      ..
    } => {
      let expr = do_node_to_source_text(expression, indent_level, nodes_map);

      let indent_level = indent_level + 1;
      let opts = options
        .iter()
        .map(|opt| do_node_to_source_text(opt, indent_level, nodes_map))
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

    ASTNode::Identifier { node_id, name, .. } => {
      format_identifier(name, &new_node_topic(node_id))
    }

    ASTNode::IdentifierPath {
      name,
      referenced_declaration,
      ..
    } => {
      if referenced_declaration < &0 {
        format_global(&name)
      } else {
        format_identifier(name, &new_node_topic(referenced_declaration))
      }
    }

    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      let base =
        do_node_to_source_text(base_expression, indent_level, nodes_map);
      if let Some(idx) = index_expression {
        format!(
          "{}[{}]",
          base,
          do_node_to_source_text(idx, indent_level, nodes_map)
        )
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
      referenced_declaration,
      ..
    } => {
      // Check if this is a special case like block.timestamp or msg.sender
      let resolved_expression = expression.resolve(nodes_map);
      let is_special_case =
        if let ASTNode::Identifier { name, .. } = resolved_expression {
          (name == "block" && member_name == "timestamp")
            || (name == "block" && member_name == "number")
            || (name == "block" && member_name == "prevrandao")
            || (name == "block" && member_name == "gaslimit")
            || (name == "block" && member_name == "difficulty")
            || (name == "block" && member_name == "coinbase")
            || (name == "block" && member_name == "chainid")
            || (name == "block" && member_name == "blobbasefee")
            || (name == "block" && member_name == "basefee")
            || (name == "msg" && member_name == "sender")
            || (name == "msg" && member_name == "value")
            || (name == "msg" && member_name == "data")
            || (name == "msg" && member_name == "sig")
            || (name == "tx" && member_name == "gasprice")
            || (name == "tx" && member_name == "origin")
        } else {
          false
        };

      if is_special_case {
        // Format as a single token with "global" class
        if let ASTNode::Identifier { name, .. } = resolved_expression {
          format!("{}.{}", format_global(name), format_global(member_name))
        } else {
          unreachable!()
        }
      } else {
        // Default formatting for other member accesses
        let expr = do_node_to_source_text(expression, indent_level, nodes_map);
        if let Some(member_node_id) = referenced_declaration {
          let member =
            format_identifier(&member_name, &new_node_topic(member_node_id));

          let indent_level = indent_level + 1;
          let member_expr = format!("{}{}", format_operator("."), member);
          format!("{}{}", expr, indent(&member_expr, indent_level),)
        } else {
          let member = format_member(&member_name);

          let member_expr = format!("{}{}", format_operator("."), member);
          format!("{}{}", expr, member_expr)
        }
      }
    }

    ASTNode::NewExpression {
      node_id, type_name, ..
    } => {
      format!(
        "{} {}",
        format_topic_keyword("new", &new_node_topic(node_id)),
        do_node_to_source_text(type_name, indent_level, nodes_map)
      )
    }

    ASTNode::TupleExpression { components, .. } => {
      let indent_level = indent_level + 1;
      let comps = components
        .iter()
        .map(|c| do_node_to_source_text(c, indent_level, nodes_map))
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
      node_id,
      prefix,
      operator,
      sub_expression,
      ..
    } => {
      let op = unary_operator_to_string(operator);
      let expr =
        do_node_to_source_text(sub_expression, indent_level, nodes_map);
      if *prefix {
        format!(
          "{}{}",
          format_topic_operator(&op, &new_node_topic(node_id)),
          expr
        )
      } else {
        format!(
          "{}{}",
          expr,
          format_topic_operator(&op, &new_node_topic(node_id))
        )
      }
    }

    ASTNode::EnumValue { node_id, name, .. } => {
      format_enum_value(&html_escape(name), &new_node_topic(node_id))
    }

    ASTNode::Block { statements, .. } => {
      if statements.is_empty() {
        format_brace("{}", indent_level)
      } else {
        let indent_level = indent_level + 1;
        let stmts = statements
          .iter()
          .map(|s| do_node_to_source_text(s, indent_level, nodes_map))
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
      .map(|s| do_node_to_source_text(s, indent_level, nodes_map))
      .collect::<Vec<_>>()
      .join("\n"),

    ASTNode::Break { .. } => format_keyword("break"),

    ASTNode::Continue { .. } => format_keyword("continue"),

    ASTNode::DoWhileStatement {
      node_id,
      nodes,
      body,
      ..
    } => {
      let body_str = if let Some(b) = body {
        do_node_to_source_text(b, indent_level, nodes_map)
      } else {
        String::new()
      };
      let condition = if !nodes.is_empty() {
        do_node_to_source_text(&nodes[0], indent_level, nodes_map)
      } else {
        String::new()
      };
      format!(
        "{} {} {} ({})",
        format_topic_keyword("do", &new_node_topic(node_id)),
        body_str,
        format_keyword("while"),
        condition
      )
    }

    ASTNode::EmitStatement {
      node_id,
      event_call,
      ..
    } => {
      format!(
        "{} {}",
        format_topic_keyword("emit", &new_node_topic(node_id)),
        do_node_to_source_text(event_call, indent_level, nodes_map)
      )
    }

    ASTNode::ExpressionStatement { expression, .. } => {
      do_node_to_source_text(expression, indent_level, nodes_map)
    }

    ASTNode::ForStatement {
      node_id,
      initialization_expression,
      condition,
      loop_expression,
      body,
      ..
    } => {
      let _init = if let Some(init_expr) = initialization_expression {
        do_node_to_source_text(init_expr, indent_level, nodes_map)
      } else {
        String::new()
      };
      let _cond = if let Some(cond_expr) = condition {
        do_node_to_source_text(cond_expr, indent_level, nodes_map)
      } else {
        String::new()
      };
      let _loop_expr = if let Some(l_expr) = loop_expression {
        do_node_to_source_text(l_expr, indent_level, nodes_map)
      } else {
        String::new()
      };
      let body_str = do_node_to_source_text(body, indent_level, nodes_map);
      format!(
        "{} (LoopExpr) {}",
        format_topic_keyword("for", &new_node_topic(node_id)),
        body_str
      )
    }

    ASTNode::IfStatement {
      node_id,
      condition,
      true_body,
      false_body,
      ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level, nodes_map);
      let true_b = do_node_to_source_text(true_body, indent_level, nodes_map);
      let false_part = if let Some(false_b) = false_body {
        format!(
          " {} {}",
          format_keyword("else"),
          do_node_to_source_text(false_b, indent_level, nodes_map)
        )
      } else {
        String::new()
      };
      format!(
        "{} ({}\n) {}{}",
        format_topic_keyword("if", &new_node_topic(node_id)),
        indent(&cond, indent_level + 1),
        true_b,
        false_part
      )
    }

    ASTNode::InlineAssembly { .. } => format_keyword("assembly"),

    ASTNode::PlaceholderStatement { node_id, .. } => {
      format_topic_keyword("placeholder", &new_node_topic(node_id))
    }

    ASTNode::Return {
      node_id,
      expression,
      ..
    } => {
      if let Some(expr) = expression {
        format!(
          "{} {}",
          format_topic_keyword("return", &new_node_topic(node_id)),
          do_node_to_source_text(expr, indent_level, nodes_map)
        )
      } else {
        format_topic_keyword("return", &new_node_topic(node_id))
      }
    }

    ASTNode::RevertStatement {
      node_id,
      error_call,
      ..
    } => {
      format!(
        "{} {}",
        format_topic_keyword("revert", &new_node_topic(node_id)),
        do_node_to_source_text(error_call, indent_level, nodes_map)
      )
    }

    ASTNode::TryStatement { .. } => "TryStatement placeholder".to_owned(),

    ASTNode::UncheckedBlock { statements, .. } => {
      let indent_level = indent_level + 1;

      let stmts = statements
        .iter()
        .map(|s| do_node_to_source_text(s, indent_level, nodes_map))
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
      let indent_level = indent_level + 1;

      if let Some(init) = initial_value {
        declarations
          .iter()
          .map(|d| {
            format!(
              "{} {}{}",
              do_node_to_source_text(d, indent_level, nodes_map),
              format_operator("="),
              indent(
                &do_node_to_source_text(init, indent_level, nodes_map),
                indent_level
              ),
            )
          })
          .collect::<Vec<_>>()
          .join("\n")
      } else {
        declarations
          .iter()
          .map(|d| do_node_to_source_text(d, indent_level, nodes_map))
          .collect::<Vec<_>>()
          .join("\n")
      }
    }

    ASTNode::VariableDeclaration {
      node_id,
      type_name,
      storage_location,
      name,
      visibility,
      mutability,
      value,
      constant,
      ..
    } => {
      let type_str = do_node_to_source_text(type_name, indent_level, nodes_map);
      let storage = storage_location_to_string(storage_location);
      let visibility_str = variable_visibility_to_string(visibility);
      let mutability_str = variable_mutability_to_string(mutability);

      let mut parts = vec![];
      if !visibility_str.is_empty() {
        parts.push(format_keyword(&visibility_str));
      }
      if !mutability_str.is_empty() {
        parts.push(format_keyword(&mutability_str));
      }
      if !storage.is_empty() {
        parts.push(format_keyword(&storage));
      }
      parts.push(type_str);
      parts.push(format_identifier(name, &new_node_topic(node_id)));

      let decl = parts.join(" ");
      if let Some(val) = value
        && !constant
      {
        let val = do_node_to_source_text(val, indent_level, nodes_map);
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
      node_id,
      condition,
      body,
      ..
    } => {
      let cond = do_node_to_source_text(condition, indent_level, nodes_map);
      let body_str = if let Some(b) = body {
        do_node_to_source_text(b, indent_level, nodes_map)
      } else {
        format_brace("{}", indent_level)
      };
      format!(
        "{} ({}) {}",
        format_topic_keyword("while", &new_node_topic(node_id)),
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
        let base_indent_level = indent_level + 1;
        let first_base = do_node_to_source_text(
          &base_contracts[0],
          base_indent_level,
          nodes_map,
        );
        let remaining_bases = if base_contracts.len() > 1 {
          let remaining = base_contracts[1..]
            .iter()
            .map(|b| do_node_to_source_text(b, base_indent_level, nodes_map))
            .collect::<Vec<_>>()
            .join("\n");
          format!("\n{}", inline_indent(&remaining, base_indent_level))
        } else {
          String::new()
        };
        format!(
          "{} {}{}",
          indent(&format_keyword("is"), base_indent_level),
          first_base,
          remaining_bases
        )
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
          .map(|n| do_node_to_source_text(n, indent_level + 1, nodes_map))
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
      node_id,
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
        format!(" {}", format_function_name(name, &new_node_topic(node_id)))
      };
      let params = format!(
        "{} ",
        do_node_to_source_text(parameters, indent_level, nodes_map)
      );
      let modifiers = if !modifiers.is_empty() {
        let indent_level = indent_level + 1;
        let mods = modifiers
          .iter()
          .map(|m| do_node_to_source_text(m, indent_level, nodes_map))
          .collect::<Vec<_>>()
          .join("\n");
        let trailing_newline = if !mods.is_empty() { "\n" } else { "" };

        format!("{}{}", indent(&mods, indent_level), trailing_newline)
      } else {
        String::new()
      };
      let returns = format!(
        "{} {} ",
        format_keyword("returns"),
        do_node_to_source_text(return_parameters, indent_level, nodes_map)
      );
      let body = if let Some(b) = body {
        do_node_to_source_text(b, indent_level, nodes_map)
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
      node_id,
      name,
      parameters,
      ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level, nodes_map);
      format!(
        "{} {}{}",
        format_keyword("event"),
        format_user_defined_type(name, &new_node_topic(node_id)),
        params
      )
    }

    ASTNode::ErrorDefinition {
      node_id,
      name,
      parameters,
      ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level, nodes_map);
      format!(
        "{} {}{}",
        format_keyword("error"),
        format_user_defined_type(name, &new_node_topic(node_id)),
        params
      )
    }

    ASTNode::ModifierDefinition {
      node_id,
      name,
      parameters,
      virtual_,
      visibility,
      body,
      ..
    } => {
      let params = do_node_to_source_text(parameters, indent_level, nodes_map);
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
        format_function_name(name, &new_node_topic(node_id)),
        params,
        do_node_to_source_text(body, indent_level, nodes_map)
      )
    }

    ASTNode::StructDefinition {
      node_id,
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
        .map(|m| do_node_to_source_text(m, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !members_str.is_empty() { "\n" } else { "" };
      format!(
        "{} {} {} {{{}{}}}",
        visibility_str,
        format_keyword("struct"),
        format_user_defined_type(name, &new_node_topic(node_id)),
        indent(&members_str, indent_level),
        trailing_newline
      )
    }

    ASTNode::EnumDefinition {
      node_id,
      name,
      members,
      ..
    } => {
      let indent_level = indent_level + 1;

      let members_str = members
        .iter()
        .map(|m| do_node_to_source_text(m, indent_level, nodes_map))
        .collect::<Vec<_>>()
        .join("\n");
      let trailing_newline = if !members_str.is_empty() { "\n" } else { "" };
      format!(
        "{} {} {{{}{}}}",
        format_keyword("enum"),
        format_user_defined_type(name, &new_node_topic(node_id)),
        indent(&members_str, indent_level),
        trailing_newline
      )
    }

    ASTNode::UserDefinedValueTypeDefinition {
      node_id,
      name,
      underlying_type,
      ..
    } => {
      format!(
        "type {} is {}",
        format_user_defined_type(name, &new_node_topic(node_id)),
        do_node_to_source_text(underlying_type, indent_level, nodes_map)
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
        do_node_to_source_text(lib_node, indent_level, nodes_map)
      } else {
        String::new()
      };
      let type_str = if let Some(type_node) = type_name {
        let for_indent_level = indent_level + 1;
        format!(
          "{} {}",
          indent(&format_keyword("for"), for_indent_level),
          do_node_to_source_text(type_node, for_indent_level, nodes_map)
        )
      } else {
        String::new()
      };
      format!("{} {}{}", format_keyword("using"), lib, type_str)
    }

    ASTNode::SourceUnit { nodes, .. } => nodes
      .iter()
      .map(|n| do_node_to_source_text(n, indent_level, nodes_map))
      .collect::<Vec<_>>()
      .join("\n\n"),

    ASTNode::InheritanceSpecifier { base_name, .. } => {
      do_node_to_source_text(base_name, indent_level, nodes_map)
    }

    ASTNode::ElementaryTypeName { name, .. } => format_type(name),

    ASTNode::FunctionTypeName {
      parameter_types,
      return_parameter_types,
      visibility,
      state_mutability,
      ..
    } => {
      let params =
        do_node_to_source_text(parameter_types, indent_level, nodes_map);
      let visibility_str = function_visibility_to_string(visibility);
      let mutability_str = function_mutability_to_string(state_mutability);
      let returns_str = format!(
        "{} {}",
        format_keyword("returns"),
        do_node_to_source_text(return_parameter_types, indent_level, nodes_map)
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
          .map(|p| do_node_to_source_text(p, indent_level, nodes_map))
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
      let name = do_node_to_source_text(modifier_name, indent_level, nodes_map);
      if let Some(args) = arguments {
        if args.is_empty() {
          format!("{}()", name)
        } else {
          let args_str = args
            .iter()
            .map(|a| do_node_to_source_text(a, indent_level, nodes_map))
            .collect::<Vec<_>>()
            .join("\n");
          format!("{}({}\n)", name, indent(&args_str, indent_level + 1))
        }
      } else {
        name
      }
    }

    ASTNode::UserDefinedTypeName { path_node, .. } => {
      do_node_to_source_text(path_node, indent_level, nodes_map)
    }

    ASTNode::ArrayTypeName { base_type, .. } => {
      format!(
        "{}[]",
        do_node_to_source_text(base_type, indent_level, nodes_map)
      )
    }

    ASTNode::Mapping {
      key_type,
      key_name,
      value_type,
      value_name,
      ..
    } => {
      let mut key = do_node_to_source_text(key_type, indent_level, nodes_map);
      if let Some(name) = key_name {
        key = format!("{} {}", key, name)
      }
      let mut value =
        do_node_to_source_text(value_type, indent_level, nodes_map);
      if let Some(name) = value_name {
        value = format!("{} {}", value, name)
      }
      format!(
        "{} ({}\n)",
        format_keyword("map"),
        indent(
          &format!("{} {} {}", key, format_operator("=>"), value),
          indent_level + 1
        )
      )
    }

    ASTNode::StructuredDocumentation { .. } => String::new(),

    ASTNode::Stub { topic, .. } => format!("NodeStub-{}", topic.id),

    ASTNode::Other { .. } => format_comment("Unknown"),
  };

  format_node(&node_str, node.node_id(), "node")
}

fn format_token(token: &str, class: &str) -> String {
  format!("<span class=\"{}\">{}</span>", class, token)
}

fn format_topic_token(
  token: &str,
  class: &str,
  topic: &topic::Topic,
) -> String {
  format!(
    "<span class=\"{}\" data-topic=\"{}\" tabindex=\"0\">{}</span>",
    class,
    topic.id(),
    token
  )
}

fn format_node(node_str: &str, id: i32, class: &str) -> String {
  format!("<span class=\"{} {}\">{}</span>", class, id, node_str)
}

fn format_keyword(keyword: &str) -> String {
  format_token(keyword, "keyword")
}

fn format_topic_keyword(keyword: &str, topic: &topic::Topic) -> String {
  format_topic_token(keyword, "keyword", topic)
}

fn format_function_name(name: &String, topic: &topic::Topic) -> String {
  format_topic_token(name, "function", topic)
}

fn format_identifier(name: &String, topic: &topic::Topic) -> String {
  format_topic_token(name, "identifier", topic)
}

fn format_user_defined_type(
  type_name: &String,
  topic: &topic::Topic,
) -> String {
  format_topic_token(type_name, "user-type", topic)
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

fn format_enum_value(name: &str, topic: &topic::Topic) -> String {
  format_topic_token(name, "enum-value", topic)
}

fn format_comment(text: &str) -> String {
  format_token(text, "comment")
}

fn format_number(val: &str) -> String {
  format_token(val, "number")
}

fn format_topic_number(val: &str, topic: &topic::Topic) -> String {
  format_topic_token(val, "number", topic)
}

fn format_string(val: &str) -> String {
  format_token(val, "string")
}

fn format_operator(op: &str) -> String {
  format_token(&html_escape(&op), "operator")
}

fn format_topic_operator(op: &str, topic: &topic::Topic) -> String {
  format_topic_token(&html_escape(&op), "operator", topic)
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
  format!("\n{}", inline_indent(content, indent_level))
}

/// Creates an indent with no newline
fn inline_indent(content: &str, indent_level: usize) -> String {
  format!(
    "<span class=\"indent indent-level-{}\">{}</span>",
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

pub fn node_to_signature(topic: topic::Topic, node: &ASTNode) -> String {
  let sig = match node {
    ASTNode::FunctionDefinition {
      node_id,
      kind,
      name,
      visibility,
      state_mutability,
      virtual_,
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
        format!(" {}", format_function_name(name, &new_node_topic(node_id)))
      };

      format!(
        "{}{}{}{}{}",
        virtual_str, visibility, mutability, kind_str, name_str,
      )
    }
    ASTNode::ContractDefinition {
      contract_kind,
      name,
      abstract_,
      ..
    } => {
      let kind = contract_kind_to_string(contract_kind);
      let abstract_str = if *abstract_ {
        format!("{} ", format_keyword("abstract"))
      } else {
        String::new()
      };

      format!(
        "{}{} {}",
        abstract_str,
        format_keyword(&html_escape(&kind)),
        format_type(&html_escape(name)),
      )
    }
    ASTNode::Assignment { .. } => format!("Assignment {}", topic.id),
    ASTNode::BinaryOperation { .. } => format!("BinaryOperation {}", topic.id),
    ASTNode::Conditional { .. } => format!("Conditional {}", topic.id),
    ASTNode::ElementaryTypeNameExpression { .. } => {
      format!("ElementaryTypeNameExpression {}", topic.id)
    }
    ASTNode::FunctionCall { .. } => format!("FunctionCall {}", topic.id),
    ASTNode::FunctionCallOptions { .. } => {
      format!("FunctionCallOptions {}", topic.id)
    }
    ASTNode::Identifier { .. } => format!("Identifier {}", topic.id),
    ASTNode::IdentifierPath { .. } => format!("IdentifierPath {}", topic.id),
    ASTNode::IndexAccess { .. } => format!("IndexAccess {}", topic.id),
    ASTNode::IndexRangeAccess { .. } => {
      format!("IndexRangeAccess {}", topic.id)
    }
    ASTNode::Literal { .. } => format!("Literal {}", topic.id),
    ASTNode::MemberAccess { .. } => format!("MemberAccess {}", topic.id),
    ASTNode::NewExpression { .. } => format!("NewExpression {}", topic.id),
    ASTNode::TupleExpression { .. } => format!("TupleExpression {}", topic.id),
    ASTNode::UnaryOperation { .. } => format!("UnaryOperation {}", topic.id),
    ASTNode::EnumValue { .. } => format!("EnumValue {}", topic.id),
    ASTNode::Block { .. } => format!("Block {}", topic.id),
    ASTNode::SemanticBlock { .. } => format!("SemanticBlock {}", topic.id),
    ASTNode::Break { .. } => format!("Break {}", topic.id),
    ASTNode::Continue { .. } => format!("Continue {}", topic.id),
    ASTNode::DoWhileStatement { .. } => {
      format!("DoWhileStatement {}", topic.id)
    }
    ASTNode::EmitStatement { .. } => format!("EmitStatement {}", topic.id),
    ASTNode::ExpressionStatement { .. } => {
      format!("ExpressionStatement {}", topic.id)
    }
    ASTNode::ForStatement { .. } => format!("ForStatement {}", topic.id),
    ASTNode::IfStatement { .. } => format!("IfStatement {}", topic.id),
    ASTNode::InlineAssembly { .. } => format!("InlineAssembly {}", topic.id),
    ASTNode::PlaceholderStatement { .. } => {
      format!("PlaceholderStatement {}", topic.id)
    }
    ASTNode::Return { .. } => format!("Return {}", topic.id),
    ASTNode::RevertStatement { .. } => format!("RevertStatement {}", topic.id),
    ASTNode::TryStatement { .. } => format!("TryStatement {}", topic.id),
    ASTNode::UncheckedBlock { .. } => format!("UncheckedBlock {}", topic.id),
    ASTNode::VariableDeclarationStatement { .. } => {
      format!("VariableDeclarationStatement {}", topic.id)
    }
    ASTNode::VariableDeclaration { .. } => {
      format!("VariableDeclaration {}", topic.id)
    }
    ASTNode::WhileStatement { .. } => format!("WhileStatement {}", topic.id),
    ASTNode::EventDefinition { .. } => format!("EventDefinition {}", topic.id),
    ASTNode::ErrorDefinition { .. } => format!("ErrorDefinition {}", topic.id),
    ASTNode::ModifierDefinition { .. } => {
      format!("ModifierDefinition {}", topic.id)
    }
    ASTNode::StructDefinition { .. } => {
      format!("StructDefinition {}", topic.id)
    }
    ASTNode::EnumDefinition { .. } => format!("EnumDefinition {}", topic.id),
    ASTNode::UserDefinedValueTypeDefinition { .. } => {
      format!("UserDefinedValueTypeDefinition {}", topic.id)
    }
    ASTNode::PragmaDirective { .. } => format!("PragmaDirective {}", topic.id),
    ASTNode::ImportDirective { .. } => format!("ImportDirective {}", topic.id),
    ASTNode::UsingForDirective { .. } => {
      format!("UsingForDirective {}", topic.id)
    }
    ASTNode::SourceUnit { .. } => format!("SourceUnit {}", topic.id),
    ASTNode::InheritanceSpecifier { .. } => {
      format!("InheritanceSpecifier {}", topic.id)
    }
    ASTNode::ElementaryTypeName { .. } => {
      format!("ElementaryTypeName {}", topic.id)
    }
    ASTNode::FunctionTypeName { .. } => {
      format!("FunctionTypeName {}", topic.id)
    }
    ASTNode::ParameterList { .. } => format!("ParameterList {}", topic.id),
    ASTNode::TryCatchClause { .. } => format!("TryCatchClause {}", topic.id),
    ASTNode::ModifierInvocation { .. } => {
      format!("ModifierInvocation {}", topic.id)
    }
    ASTNode::UserDefinedTypeName { .. } => {
      format!("UserDefinedTypeName {}", topic.id)
    }
    ASTNode::ArrayTypeName { .. } => format!("ArrayTypeName {}", topic.id),
    ASTNode::Mapping { .. } => format!("Mapping {}", topic.id),
    ASTNode::StructuredDocumentation { .. } => {
      format!("StructuredDocumentation {}", topic.id)
    }
    ASTNode::Stub { .. } => format!("Stub {}", topic.id),
    ASTNode::Other { .. } => format!("Other {}", topic.id),
  };

  format!("<pre><code>{}</code></pre>", sig)
}
