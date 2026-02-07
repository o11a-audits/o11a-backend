use crate::formatting;
use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};

/// Renders comment markdown content to HTML.
pub fn render_comment_html(content: &str) -> String {
  markdown_to_html(content)
}

/// Converts markdown to HTML using pulldown-cmark
fn markdown_to_html(content: &str) -> String {
  let options = Options::ENABLE_STRIKETHROUGH
    | Options::ENABLE_TABLES
    | Options::ENABLE_FOOTNOTES
    | Options::ENABLE_TASKLISTS;

  let parser = Parser::new_ext(content, options);
  let mut html_output = String::new();

  for event in parser {
    match event {
      Event::Start(tag) => {
        html_output.push_str(&start_tag_to_html(&tag));
      }
      Event::End(tag) => {
        html_output.push_str(&end_tag_to_html(&tag));
      }
      Event::Text(text) => {
        html_output.push_str(&formatting::html_escape(&text));
      }
      Event::Code(code) => {
        html_output.push_str("<code class=\"inline-code\">");
        html_output.push_str(&formatting::html_escape(&code));
        html_output.push_str("</code>");
      }
      Event::SoftBreak => {
        html_output.push('\n');
      }
      Event::HardBreak => {
        html_output.push_str("<br>");
      }
      Event::Rule => {
        html_output.push_str("<hr>");
      }
      Event::Html(html) | Event::InlineHtml(html) => {
        // Pass through raw HTML (escaped for safety)
        html_output.push_str(&formatting::html_escape(&html));
      }
      Event::FootnoteReference(name) => {
        html_output.push_str(&format!(
          "<sup class=\"footnote-ref\"><a href=\"#fn-{}\">[{}]</a></sup>",
          formatting::html_escape(&name),
          formatting::html_escape(&name)
        ));
      }
      Event::TaskListMarker(checked) => {
        let checked_attr = if checked { " checked" } else { "" };
        html_output.push_str(&format!(
          "<input type=\"checkbox\" disabled{}>",
          checked_attr
        ));
      }
      Event::InlineMath(math) | Event::DisplayMath(math) => {
        html_output.push_str(&formatting::html_escape(&math));
      }
    }
  }

  html_output
}

fn start_tag_to_html(tag: &Tag) -> String {
  match tag {
    Tag::Paragraph => "<p>".to_string(),
    Tag::Heading { level, .. } => format!("<h{}>", *level as u8),
    Tag::BlockQuote(_) => "<blockquote>".to_string(),
    Tag::CodeBlock(_) => "<pre><code>".to_string(),
    Tag::List(Some(start)) => format!("<ol start=\"{}\">", start),
    Tag::List(None) => "<ul>".to_string(),
    Tag::Item => "<li>".to_string(),
    Tag::FootnoteDefinition(name) => {
      format!(
        "<div class=\"footnote\" id=\"fn-{}\">",
        formatting::html_escape(name)
      )
    }
    Tag::Table(_) => "<table>".to_string(),
    Tag::TableHead => "<thead><tr>".to_string(),
    Tag::TableRow => "<tr>".to_string(),
    Tag::TableCell => "<td>".to_string(),
    Tag::Emphasis => "<em>".to_string(),
    Tag::Strong => "<strong>".to_string(),
    Tag::Strikethrough => "<del>".to_string(),
    Tag::Link {
      dest_url, title, ..
    } => {
      let title_attr = if title.is_empty() {
        String::new()
      } else {
        format!(" title=\"{}\"", formatting::html_escape(title))
      };
      format!(
        "<a href=\"{}\"{}>",
        formatting::html_escape(dest_url),
        title_attr
      )
    }
    Tag::Image {
      dest_url, title, ..
    } => {
      let title_attr = if title.is_empty() {
        String::new()
      } else {
        format!(" title=\"{}\"", formatting::html_escape(title))
      };
      format!(
        "<img src=\"{}\"{}",
        formatting::html_escape(dest_url),
        title_attr
      )
    }
    Tag::HtmlBlock => String::new(),
    Tag::MetadataBlock(_) => String::new(),
    Tag::DefinitionList => "<dl>".to_string(),
    Tag::DefinitionListTitle => "<dt>".to_string(),
    Tag::DefinitionListDefinition => "<dd>".to_string(),
  }
}

fn end_tag_to_html(tag: &TagEnd) -> String {
  match tag {
    TagEnd::Paragraph => "</p>".to_string(),
    TagEnd::Heading(level) => format!("</h{}>", *level as u8),
    TagEnd::BlockQuote(_) => "</blockquote>".to_string(),
    TagEnd::CodeBlock => "</code></pre>".to_string(),
    TagEnd::List(true) => "</ol>".to_string(),
    TagEnd::List(false) => "</ul>".to_string(),
    TagEnd::Item => "</li>".to_string(),
    TagEnd::FootnoteDefinition => "</div>".to_string(),
    TagEnd::Table => "</table>".to_string(),
    TagEnd::TableHead => "</tr></thead>".to_string(),
    TagEnd::TableRow => "</tr>".to_string(),
    TagEnd::TableCell => "</td>".to_string(),
    TagEnd::Emphasis => "</em>".to_string(),
    TagEnd::Strong => "</strong>".to_string(),
    TagEnd::Strikethrough => "</del>".to_string(),
    TagEnd::Link => "</a>".to_string(),
    TagEnd::Image => ">".to_string(), // Close the img tag
    TagEnd::HtmlBlock => String::new(),
    TagEnd::MetadataBlock(_) => String::new(),
    TagEnd::DefinitionList => "</dl>".to_string(),
    TagEnd::DefinitionListTitle => "</dt>".to_string(),
    TagEnd::DefinitionListDefinition => "</dd>".to_string(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_markdown_to_html_paragraph() {
    let result = markdown_to_html("Hello world");
    assert!(result.contains("<p>"));
    assert!(result.contains("Hello world"));
  }

  #[test]
  fn test_markdown_to_html_code() {
    let result = markdown_to_html("Use `transfer` function");
    assert!(result.contains("<code"));
    assert!(result.contains("transfer"));
  }
}
