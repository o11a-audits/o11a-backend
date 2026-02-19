use crate::core;
use crate::core::topic;
use crate::core::{
  AST, AuditData, DataContext, Node, Scope, TitledTopicKind, TopicMetadata,
  UnnamedTopicKind, ensure_context, insert_into_context,
};
use crate::documentation::parser::{self, DocumentationAST, DocumentationNode};
use std::collections::BTreeMap;
use std::path::Path;

/// Analyzes documentation files and integrates them with the solidity DataContext
/// This MUST be called after solidity analysis completes, as it needs the solidity
/// declarations to resolve inline code references
pub fn analyze(
  project_root: &Path,
  audit_id: &str,
  data_context: &mut DataContext,
  document_files: &[core::ProjectPath],
) -> Result<(), String> {
  // Get the audit data
  let audit_data = data_context
    .get_audit_mut(audit_id)
    .ok_or_else(|| format!("Audit '{}' not found", audit_id))?;

  // Parse document files in the order specified by documents.txt
  let ast_map =
    parser::process_files(project_root, document_files, &audit_data)?;

  // Collect mentions during processing: referenced_topic -> [scope]
  // The scope tells us the container (file), component (section), and member (paragraph)
  let mut mentions_by_topic: BTreeMap<topic::Topic, Vec<Scope>> =
    BTreeMap::new();

  // Process each markdown file and add nodes/declarations to the audit data
  for (project_path, asts) in &ast_map {
    for ast in asts {
      // Add to in_scope_files
      audit_data.in_scope_files.insert(project_path.clone());

      // Add to asts map with stubbed nodes
      let stubbed_ast = DocumentationAST {
        nodes: ast
          .nodes
          .iter()
          .map(|n| parser::children_to_stubs(n.clone()))
          .collect(),
        project_path: project_path.clone(),
        source_content: ast.source_content.clone(),
      };
      audit_data
        .asts
        .insert(project_path.clone(), AST::Documentation(stubbed_ast));

      process_documentation_ast(
        ast,
        project_path,
        audit_data,
        &mut mentions_by_topic,
      )?;
    }
  }

  // Build SourceContexts for each referenced topic and update their mentions field
  populate_mentions(
    &mut audit_data.topic_metadata,
    mentions_by_topic,
    &audit_data.nodes,
  );

  Ok(())
}

fn process_documentation_ast(
  ast: &DocumentationAST,
  project_path: &core::ProjectPath,
  audit_data: &mut AuditData,
  mentions_by_topic: &mut BTreeMap<topic::Topic, Vec<Scope>>,
) -> Result<(), String> {
  let scope = Scope::Container {
    container: project_path.clone(),
  };

  // Process all nodes in the AST
  for node in &ast.nodes {
    process_documentation_node(node, &scope, audit_data, mentions_by_topic)?;
  }

  Ok(())
}

fn process_documentation_node(
  node: &DocumentationNode,
  scope: &Scope,
  audit_data: &mut AuditData,
  mentions_by_topic: &mut BTreeMap<topic::Topic, Vec<Scope>>,
) -> Result<(), String> {
  let topic = topic::new_documentation_topic(node.node_id());

  match node {
    DocumentationNode::Root { children, .. } => {
      // Add the Root node first so build_self_context can look up its source location
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationRoot,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    // Heading: contains text content and optionally a Section child
    // The Heading itself is scoped at the current level, but its Section child
    // creates a nested scope for the section's content.
    DocumentationNode::Heading {
      children, section, ..
    } => {
      // Add the heading node first so build_self_context can look up its source location
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationHeading,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process heading text children (inline formatting nodes)
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }

      // Process the section child if present - it will create its own nested scope
      if let Some(sec) = section {
        process_documentation_node(sec, scope, audit_data, mentions_by_topic)?;
      }
    }

    // Section: groups content under a heading. The section is a child of the Heading,
    // and creates a nested scope for its content.
    DocumentationNode::Section {
      title, children, ..
    } => {
      // Add the section node first so build_self_context can look up its source location
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Build a self-referencing context so the topic panel shows this section's content
      let context = build_self_context(&topic, scope, &audit_data.nodes);

      // Add topic metadata for the section
      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::TitledTopic {
          topic: topic.clone(),
          scope: scope.clone(),
          kind: TitledTopicKind::DocumentationSection,
          title: title.clone(),
          context,
          mentions: vec![],
        },
      );

      // Create nested scope by adding this section to the scope hierarchy.
      // Sections are added regardless of heading level:
      // - First section becomes Component (Container -> Component)
      // - Second nested section becomes Member (Component -> Member)
      // - Third nested section becomes SemanticBlock (Member -> SemanticBlock)
      // - Further nesting stays at SemanticBlock level
      let section_scope = core::add_to_scope(scope, topic.clone());

      // Process section children with the nested scope
      for child in children {
        process_documentation_node(
          child,
          &section_scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::Paragraph { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationParagraph,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Paragraphs don't add to scope - only sections/headers define scope hierarchy.
      // Process children with the same scope.
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::Sentence { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationSentence,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Sentences don't create a new scope level - they stay within the
      // paragraph's semantic block scope. Process children with same scope.
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::CodeBlock { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationCodeBlock,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process children (code tokens) with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::List { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationList,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::BlockQuote { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationBlockQuote,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::InlineCode { children, .. } => {
      audit_data.nodes.insert(
        topic.clone(),
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      let context = build_self_context(&topic, scope, &audit_data.nodes);

      audit_data.topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic: topic.clone(),
          kind: UnnamedTopicKind::DocumentationInlineCode,
          scope: scope.clone(),
          context,
          mentions: vec![],
        },
      );

      // Process children (code tokens) with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    // CodeIdentifier with a referenced_topic creates a mention
    DocumentationNode::CodeIdentifier {
      referenced_topic: Some(ref_topic),
      ..
    } => {
      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));

      // Record the mention using the current scope
      // The scope tells us the containing document element (paragraph, section, or file)
      if !matches!(scope, Scope::Global) {
        mentions_by_topic
          .entry(ref_topic.clone())
          .or_default()
          .push(scope.clone());
      }
    }

    // For all other node types, just add them to the nodes map (no topic_metadata)
    DocumentationNode::Text { .. }
    | DocumentationNode::ThematicBreak { .. }
    | DocumentationNode::CodeKeyword { .. }
    | DocumentationNode::CodeOperator { .. }
    | DocumentationNode::CodeIdentifier { .. }
    | DocumentationNode::CodeText { .. } => {
      audit_data
        .nodes
        .insert(topic, Node::Documentation(node.clone()));
    }

    // For nodes with children that don't create topic_metadata, add the node and recurse
    DocumentationNode::ListItem { children, .. }
    | DocumentationNode::Emphasis { children, .. }
    | DocumentationNode::Strong { children, .. }
    | DocumentationNode::Link { children, .. } => {
      // Add the node with children converted to stubs
      audit_data.nodes.insert(
        topic,
        Node::Documentation(parser::children_to_stubs(node.clone())),
      );

      // Process children with the same scope
      for child in children {
        process_documentation_node(
          child,
          scope,
          audit_data,
          mentions_by_topic,
        )?;
      }
    }

    DocumentationNode::Stub { .. } => {
      // Stubs are already processed, skip
    }
  }

  Ok(())
}

/// Builds a self-referencing SourceContext for a documentation topic.
/// Places the topic as a reference within its scope hierarchy so the
/// topic panel shows the topic's own rendered content.
fn build_self_context(
  topic: &topic::Topic,
  scope: &Scope,
  nodes: &BTreeMap<topic::Topic, Node>,
) -> Vec<core::SourceContext> {
  let mut groups: Vec<core::SourceContext> = Vec::new();
  let sort_key = get_source_location_start(topic, nodes);

  match scope {
    Scope::Global | Scope::Container { .. } => {
      // Topic is at the top level (e.g., Root or H1 section) — use itself as
      // both the scope and the reference so the panel renders its content.
      insert_into_context(
        &mut groups,
        topic.clone(),
        sort_key,
        true,
        None,
        &[],
        core::Reference::project_reference(topic.clone(), sort_key),
      );
    }
    Scope::Component { component, .. } => {
      // Topic is under a component (e.g., H2 section under H1)
      let component_sort_key = get_source_location_start(component, nodes);
      insert_into_context(
        &mut groups,
        component.clone(),
        component_sort_key,
        true,
        None,
        &[],
        core::Reference::project_reference(topic.clone(), sort_key),
      );
    }
    Scope::Member {
      component, member, ..
    } => {
      // Topic is under a member (e.g., H3 section under H2 under H1)
      let component_sort_key = get_source_location_start(component, nodes);
      let member_sort_key = get_source_location_start(member, nodes);
      insert_into_context(
        &mut groups,
        component.clone(),
        component_sort_key,
        true,
        Some((member.clone(), member_sort_key)),
        &[],
        core::Reference::project_reference(topic.clone(), sort_key),
      );
    }
    Scope::ContainingBlock {
      component,
      member,
      containing_blocks,
      ..
    } => {
      if let Some(layer) = containing_blocks.last() {
        let component_sort_key = get_source_location_start(component, nodes);
        let member_sort_key = get_source_location_start(member, nodes);
        let cb_sort_key = get_source_location_start(&layer.block, nodes);
        insert_into_context(
          &mut groups,
          component.clone(),
          component_sort_key,
          true,
          Some((member.clone(), member_sort_key)),
          &[],
          core::Reference::project_reference(layer.block.clone(), cb_sort_key),
        );
      }
    }
  }

  groups
}

/// Builds SourceContexts from collected mentions and updates the referenced topics' mentions field.
/// Groups mentions by component (H1 section), with member-level (sub-H1) and containing_block-level (paragraph) sub-groups.
fn populate_mentions(
  topic_metadata: &mut BTreeMap<topic::Topic, TopicMetadata>,
  mentions_by_topic: BTreeMap<topic::Topic, Vec<core::Scope>>,
  nodes: &BTreeMap<topic::Topic, Node>,
) {
  for (referenced_topic, scopes) in mentions_by_topic {
    let mut mention_groups: Vec<core::SourceContext> = Vec::new();

    for scope in scopes {
      match scope {
        core::Scope::Global | core::Scope::Container { .. } => {
          // Global/Container scope shouldn't have documentation mentions, skip
        }
        core::Scope::Component { component, .. } => {
          // Component-level reference - ensure group exists (no reference to add)
          let scope_sort_key = get_source_location_start(&component, nodes);
          ensure_context(&mut mention_groups, component, scope_sort_key, true);
        }
        core::Scope::Member {
          component, member, ..
        } => {
          // Member-level reference - add member as a scope-level reference
          let component_sort_key = get_source_location_start(&component, nodes);
          let member_sort_key = get_source_location_start(&member, nodes);
          insert_into_context(
            &mut mention_groups,
            component,
            component_sort_key,
            true,
            None,
            &[],
            core::Reference::project_reference(member, member_sort_key),
          );
        }
        core::Scope::ContainingBlock {
          component,
          member,
          containing_blocks,
          ..
        } => {
          // ContainingBlock-level reference - nested under member
          // Use the innermost (last) containing block
          if let Some(layer) = containing_blocks.last() {
            let component_sort_key =
              get_source_location_start(&component, nodes);
            let member_sort_key = get_source_location_start(&member, nodes);
            let cb_sort_key = get_source_location_start(&layer.block, nodes);
            insert_into_context(
              &mut mention_groups,
              component,
              component_sort_key,
              true,
              Some((member, member_sort_key)),
              &[],
              core::Reference::project_reference(
                layer.block.clone(),
                cb_sort_key,
              ),
            );
          }
        }
      }
    }

    // Update the referenced topic's mentions field
    if let Some(TopicMetadata::NamedTopic { mentions, .. }) =
      topic_metadata.get_mut(&referenced_topic)
    {
      *mentions = mention_groups;
    }
  }
}

/// Gets the source location start for a topic from the nodes map.
fn get_source_location_start(
  topic: &topic::Topic,
  nodes: &BTreeMap<topic::Topic, Node>,
) -> Option<usize> {
  nodes
    .get(topic)
    .and_then(|node| node.source_location_start())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::core::{ProjectPath, new_audit_data};
  use crate::documentation::parser::{DocumentationNode, ast_from_markdown};
  use std::collections::HashSet;

  const TEST_MARKDOWN: &str = r#"# Overview

Nudge is a Reallocation Marketplace that helps protocols incentivize asset movement across blockchains and ecosystems.
Nudge empowers protocols and ecosystems to grow assets, boost token demand, and motivate users to reallocate assets within their wallets—driving sustainable, KPI-driven growth.

With Nudge, protocols can create and fund campaigns that reward users for acquiring and holding a specific token for at least a week. Nudge smart contracts acts as an escrow for rewards, while its backend system monitors participants' addresses to ensure they maintain their holdings of said token for the required period. Nudge provides an all-in-one solution, eliminating the need for any technical implementation by protocols looking to run such incentivisation campaigns.

For more details please see: [What is Nudge?](https://docs.nudge.xyz/)

## Main invariants

### Solvency Invariants

Token Balance Integrity

- `rewardToken.balanceOf(campaign) >= pendingRewards + accumulatedFees`
- *Ensures the contract always maintains sufficient reward tokens to cover all pending rewards and accumulated fees.*

Protocol Solvency

- For any active participation p:`p.rewardAmount <= rewardToken.balanceOf(campaign) - (pendingRewards - p.rewardAmount) - accumulatedFees`
- *Guarantees that any individual user's reward can be fully covered by the contract's current token balance, after accounting for fees*"#;

  fn create_test_audit_data() -> AuditData {
    new_audit_data("test-audit".to_string(), HashSet::new())
  }

  fn create_test_project_path() -> ProjectPath {
    ProjectPath {
      file_path: "test.md".to_string(),
    }
  }

  #[test]
  fn test_parse_documentation_ast() {
    let audit_data = create_test_audit_data();
    let project_path = create_test_project_path();

    let result = ast_from_markdown(
      TEST_MARKDOWN,
      &project_path,
      &audit_data,
      &parser::next_node_id,
    );
    assert!(
      result.is_ok(),
      "Failed to parse markdown: {:?}",
      result.err()
    );

    let ast = result.unwrap();
    assert_eq!(ast.nodes.len(), 1, "Expected one root node");

    // The root should contain Heading nodes (which have Section children)
    let root = &ast.nodes[0];
    match root {
      DocumentationNode::Root { children, .. } => {
        // Should have one top-level heading: "Overview"
        assert!(!children.is_empty(), "Root should have children");

        // Find the Overview heading (which contains the section as a child)
        let overview_heading = children.iter().find(|child| {
          matches!(child, DocumentationNode::Heading { section: Some(sec), .. }
            if matches!(sec.as_ref(), DocumentationNode::Section { title, .. } if title == "Overview"))
        });
        assert!(
          overview_heading.is_some(),
          "Should have a Heading with 'Overview' section"
        );

        if let Some(DocumentationNode::Heading {
          section: Some(overview_sec),
          ..
        }) = overview_heading
        {
          if let DocumentationNode::Section { children, .. } =
            overview_sec.as_ref()
          {
            // The Overview section should contain nested headings with sections
            // "Main invariants" is an H2 under Overview
            let main_invariants_heading = children.iter().find(|child| {
              matches!(child, DocumentationNode::Heading { section: Some(sec), .. }
                if matches!(sec.as_ref(), DocumentationNode::Section { title, .. } if title == "Main invariants"))
            });
            assert!(
              main_invariants_heading.is_some(),
              "Should have 'Main invariants' heading under Overview"
            );

            if let Some(DocumentationNode::Heading {
              section: Some(main_sec),
              ..
            }) = main_invariants_heading
            {
              if let DocumentationNode::Section { children, .. } =
                main_sec.as_ref()
              {
                // "Solvency Invariants" is an H3 under "Main invariants"
                let solvency_heading = children.iter().find(|child| {
                  matches!(child, DocumentationNode::Heading { section: Some(sec), .. }
                    if matches!(sec.as_ref(), DocumentationNode::Section { title, .. } if title == "Solvency Invariants"))
                });
                assert!(
                  solvency_heading.is_some(),
                  "Should have 'Solvency Invariants' heading"
                );
              }
            }
          }
        }
      }
      _ => panic!("Expected Root node, got {:?}", root),
    }
  }

  #[test]
  fn test_analyze_documentation_creates_topic_metadata() {
    let mut audit_data = create_test_audit_data();
    let project_path = create_test_project_path();

    // Parse the markdown
    let ast = ast_from_markdown(
      TEST_MARKDOWN,
      &project_path,
      &audit_data,
      &parser::next_node_id,
    )
    .unwrap();

    // Process the AST manually (simulating what analyze() does)
    let mut mentions_by_topic: BTreeMap<topic::Topic, Vec<Scope>> =
      BTreeMap::new();

    audit_data.in_scope_files.insert(project_path.clone());

    let stubbed_ast = parser::DocumentationAST {
      nodes: ast
        .nodes
        .iter()
        .map(|n| parser::children_to_stubs(n.clone()))
        .collect(),
      project_path: project_path.clone(),
      source_content: TEST_MARKDOWN.to_string(),
    };
    audit_data
      .asts
      .insert(project_path.clone(), AST::Documentation(stubbed_ast));

    let result = process_documentation_ast(
      &ast,
      &project_path,
      &mut audit_data,
      &mut mentions_by_topic,
    );
    assert!(result.is_ok(), "Failed to process AST: {:?}", result.err());

    // Check that topic metadata was created
    assert!(
      !audit_data.topic_metadata.is_empty(),
      "Should have created topic metadata"
    );

    // Find titled topics (sections)
    let titled_topics: Vec<_> = audit_data
      .topic_metadata
      .values()
      .filter(|m| matches!(m, TopicMetadata::TitledTopic { .. }))
      .collect();

    assert!(
      !titled_topics.is_empty(),
      "Should have created TitledTopic entries for sections"
    );

    // Check for specific section titles
    let titles: Vec<&str> = titled_topics
      .iter()
      .filter_map(|m| match m {
        TopicMetadata::TitledTopic { title, .. } => Some(title.as_str()),
        _ => None,
      })
      .collect();

    assert!(
      titles.contains(&"Overview"),
      "Should have 'Overview' section, got: {:?}",
      titles
    );
    assert!(
      titles.contains(&"Main invariants"),
      "Should have 'Main invariants' section, got: {:?}",
      titles
    );
    assert!(
      titles.contains(&"Solvency Invariants"),
      "Should have 'Solvency Invariants' section, got: {:?}",
      titles
    );
  }

  #[test]
  fn test_section_scope_hierarchy() {
    let mut audit_data = create_test_audit_data();
    let project_path = create_test_project_path();

    let ast = ast_from_markdown(
      TEST_MARKDOWN,
      &project_path,
      &audit_data,
      &parser::next_node_id,
    )
    .unwrap();

    let mut mentions_by_topic: BTreeMap<topic::Topic, Vec<Scope>> =
      BTreeMap::new();

    audit_data.in_scope_files.insert(project_path.clone());

    let stubbed_ast = parser::DocumentationAST {
      nodes: ast
        .nodes
        .iter()
        .map(|n| parser::children_to_stubs(n.clone()))
        .collect(),
      project_path: project_path.clone(),
      source_content: TEST_MARKDOWN.to_string(),
    };
    audit_data
      .asts
      .insert(project_path.clone(), AST::Documentation(stubbed_ast));

    process_documentation_ast(
      &ast,
      &project_path,
      &mut audit_data,
      &mut mentions_by_topic,
    )
    .unwrap();

    // Find the "Overview" section (H1 - should be Component level)
    let overview = audit_data.topic_metadata.values().find(|m| {
      matches!(m, TopicMetadata::TitledTopic { title, .. } if title == "Overview")
    });
    assert!(overview.is_some(), "Should have 'Overview' section");

    if let Some(TopicMetadata::TitledTopic { scope, .. }) = overview {
      // H1 section should be at Container scope level
      assert!(
        matches!(scope, Scope::Container { .. }),
        "H1 section should have Container scope, got {:?}",
        scope
      );
    }

    // Find the "Main invariants" section (H2 - should be Component level, as member of Overview)
    let main_invariants = audit_data.topic_metadata.values().find(|m| {
      matches!(m, TopicMetadata::TitledTopic { title, .. } if title == "Main invariants")
    });
    assert!(
      main_invariants.is_some(),
      "Should have 'Main invariants' section"
    );

    if let Some(TopicMetadata::TitledTopic { scope, .. }) = main_invariants {
      // H2 section should be at Component scope level (under the H1)
      assert!(
        matches!(scope, Scope::Component { .. }),
        "H2 section should have Component scope, got {:?}",
        scope
      );
    }

    // Find the "Solvency Invariants" section (H3 - should be Member level)
    let solvency = audit_data.topic_metadata.values().find(|m| {
      matches!(m, TopicMetadata::TitledTopic { title, .. } if title == "Solvency Invariants")
    });
    assert!(
      solvency.is_some(),
      "Should have 'Solvency Invariants' section"
    );

    if let Some(TopicMetadata::TitledTopic { scope, .. }) = solvency {
      // H3 section should be at Member scope level
      assert!(
        matches!(scope, Scope::Member { .. }),
        "H3 section should have Member scope, got {:?}",
        scope
      );
    }
  }

  #[test]
  fn test_inline_code_parsing() {
    let mut audit_data = create_test_audit_data();
    let project_path = create_test_project_path();

    let ast = ast_from_markdown(
      TEST_MARKDOWN,
      &project_path,
      &audit_data,
      &parser::next_node_id,
    )
    .unwrap();

    let mut mentions_by_topic: BTreeMap<topic::Topic, Vec<Scope>> =
      BTreeMap::new();

    audit_data.in_scope_files.insert(project_path.clone());

    let stubbed_ast = parser::DocumentationAST {
      nodes: ast
        .nodes
        .iter()
        .map(|n| parser::children_to_stubs(n.clone()))
        .collect(),
      project_path: project_path.clone(),
      source_content: TEST_MARKDOWN.to_string(),
    };
    audit_data
      .asts
      .insert(project_path.clone(), AST::Documentation(stubbed_ast));

    process_documentation_ast(
      &ast,
      &project_path,
      &mut audit_data,
      &mut mentions_by_topic,
    )
    .unwrap();

    // Find InlineCode nodes in the audit data
    let inline_code_nodes: Vec<_> = audit_data
      .nodes
      .values()
      .filter_map(|node| match node {
        Node::Documentation(doc_node) => Some(doc_node),
        _ => None,
      })
      .filter(|node| matches!(node, DocumentationNode::InlineCode { .. }))
      .collect();

    assert!(
      !inline_code_nodes.is_empty(),
      "Should have parsed inline code blocks"
    );

    // Check that inline code contains the expected code expressions
    // The markdown has code like `rewardToken.balanceOf(campaign) >= pendingRewards + accumulatedFees`
    let has_reward_token_code = inline_code_nodes.iter().any(|node| {
      if let DocumentationNode::InlineCode { value, .. } = node {
        value.contains("rewardToken")
      } else {
        false
      }
    });

    assert!(
      has_reward_token_code,
      "Should have inline code containing 'rewardToken'"
    );
  }

  #[test]
  fn test_inline_code_scope_hierarchy() {
    let mut audit_data = create_test_audit_data();
    let project_path = create_test_project_path();

    let ast = ast_from_markdown(
      TEST_MARKDOWN,
      &project_path,
      &audit_data,
      &parser::next_node_id,
    )
    .unwrap();

    let mut mentions_by_topic: BTreeMap<topic::Topic, Vec<Scope>> =
      BTreeMap::new();

    audit_data.in_scope_files.insert(project_path.clone());

    let stubbed_ast = parser::DocumentationAST {
      nodes: ast
        .nodes
        .iter()
        .map(|n| parser::children_to_stubs(n.clone()))
        .collect(),
      project_path: project_path.clone(),
      source_content: TEST_MARKDOWN.to_string(),
    };
    audit_data
      .asts
      .insert(project_path.clone(), AST::Documentation(stubbed_ast));

    process_documentation_ast(
      &ast,
      &project_path,
      &mut audit_data,
      &mut mentions_by_topic,
    )
    .unwrap();

    // Find the inline code node containing the reward token balance check
    // `rewardToken.balanceOf(campaign) >= pendingRewards + accumulatedFees`
    // Search in nodes directly since we need to find by value
    let reward_token_entry = audit_data.nodes.iter().find(|(_, node)| {
      if let Node::Documentation(DocumentationNode::InlineCode {
        value, ..
      }) = node
      {
        value.contains("rewardToken.balanceOf(campaign) >= pendingRewards")
      } else {
        false
      }
    });

    assert!(
      reward_token_entry.is_some(),
      "Should find inline code with rewardToken.balanceOf expression"
    );

    let (topic, _) = reward_token_entry.unwrap();

    // Get the metadata for this topic
    let metadata = audit_data.topic_metadata.get(topic);
    assert!(
      metadata.is_some(),
      "Should have metadata for inline code topic {:?}",
      topic
    );
    let metadata = metadata.unwrap();

    // The inline code is inside (with new section-only scope hierarchy):
    // - H1 "Overview" (component - first nested section)
    // - H2 "Main invariants" (member - second nested section)
    // - H3 "Solvency Invariants" (containing_block - third nested section)
    // Paragraphs no longer add to scope, so inline code inherits section scope.
    match metadata.scope() {
      Scope::ContainingBlock {
        container,
        component,
        member,
        containing_blocks,
      } => {
        // Verify container is our test file
        assert_eq!(
          container.file_path, "test.md",
          "Container should be test.md"
        );

        // Verify component is the "Overview" H1 section (first nested)
        let component_metadata = audit_data.topic_metadata.get(component);
        assert!(
          component_metadata.is_some(),
          "Component topic should exist in metadata"
        );
        assert_eq!(
          component_metadata.unwrap().name(),
          "Overview",
          "Component should be 'Overview' section"
        );

        // Verify member is the "Main invariants" H2 section (second nested)
        let member_metadata = audit_data.topic_metadata.get(member);
        assert!(
          member_metadata.is_some(),
          "Member topic should exist in metadata"
        );
        assert_eq!(
          member_metadata.unwrap().name(),
          "Main invariants",
          "Member should be 'Main invariants' section"
        );

        // Verify containing_block is the "Solvency Invariants" H3 section (third nested)
        let containing_block = &containing_blocks
          .last()
          .expect("Should have at least one containing block layer")
          .block;
        let containing_block_metadata =
          audit_data.topic_metadata.get(containing_block);
        assert!(
          containing_block_metadata.is_some(),
          "Containing block topic should exist in metadata"
        );
        assert_eq!(
          containing_block_metadata.unwrap().name(),
          "Solvency Invariants",
          "Containing block should be 'Solvency Invariants' section"
        );
      }
      other => {
        panic!(
          "Inline code should have ContainingBlock scope, got {:?}",
          other
        );
      }
    }
  }
}
