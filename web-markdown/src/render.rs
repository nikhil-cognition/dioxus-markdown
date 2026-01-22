use core::ops::Range;

use core::marker::PhantomData;
use std::collections::BTreeMap;

use once_cell::sync::Lazy;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};

use pulldown_cmark::{Alignment, CodeBlockKind, Event, Tag, TagEnd};

#[derive(Eq, PartialEq)]
enum MathMode {
    Inline,
    Display,
}

use super::HtmlElement::*;
use super::{Context, ElementAttributes, HtmlError, LinkDescription, MdComponentProps};

use crate::component::{ComponentCall, CustomHtmlTag, CustomHtmlTagError};
use crate::{get_substr_range, offset_range, MdComponentAttribute};

const HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "comment",
    "constant",
    "constant.builtin",
    "function",
    "function.builtin",
    "function.macro",
    "keyword",
    "number",
    "operator",
    "property",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "string",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.parameter",
];

fn highlight_class(index: usize) -> Option<&'static str> {
    match HIGHLIGHT_NAMES.get(index)? {
        &"keyword" => Some("syntax-keyword"),
        &"string" => Some("syntax-string"),
        &"comment" => Some("syntax-comment"),
        &"function" | &"function.builtin" | &"function.macro" => Some("syntax-function"),
        &"type" | &"type.builtin" => Some("syntax-type"),
        &"number" => Some("syntax-number"),
        &"constant" | &"constant.builtin" => Some("syntax-constant"),
        &"operator" => Some("syntax-operator"),
        &"variable.builtin" => Some("syntax-variable-builtin"),
        &"attribute" => Some("syntax-attribute"),
        &"property" => Some("syntax-property"),
        _ => None,
    }
}

struct LangConfig {
    config: HighlightConfiguration,
}

impl LangConfig {
    fn new(language: tree_sitter::Language, name: &str, highlights_query: &str) -> Self {
        let mut config = HighlightConfiguration::new(language, name, highlights_query, "", "")
            .expect("failed to create highlight config");
        config.configure(HIGHLIGHT_NAMES);
        Self { config }
    }
}

struct HighlightConfigs {
    rust: LangConfig,
    bash: LangConfig,
    python: LangConfig,
    go: LangConfig,
    javascript: LangConfig,
    typescript: LangConfig,
    tsx: LangConfig,
    java: LangConfig,
    c: LangConfig,
    cpp: LangConfig,
    json: LangConfig,
}

static CONFIGS: Lazy<HighlightConfigs> = Lazy::new(|| HighlightConfigs {
    rust: LangConfig::new(
        tree_sitter_rust::LANGUAGE.into(),
        "rust",
        tree_sitter_rust::HIGHLIGHTS_QUERY,
    ),
    bash: LangConfig::new(
        tree_sitter_bash::LANGUAGE.into(),
        "bash",
        tree_sitter_bash::HIGHLIGHT_QUERY,
    ),
    python: LangConfig::new(
        tree_sitter_python::LANGUAGE.into(),
        "python",
        tree_sitter_python::HIGHLIGHTS_QUERY,
    ),
    go: LangConfig::new(
        tree_sitter_go::LANGUAGE.into(),
        "go",
        tree_sitter_go::HIGHLIGHTS_QUERY,
    ),
    javascript: LangConfig::new(
        tree_sitter_javascript::LANGUAGE.into(),
        "javascript",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
    ),
    typescript: LangConfig::new(
        tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
        "typescript",
        tree_sitter_typescript::HIGHLIGHTS_QUERY,
    ),
    tsx: LangConfig::new(
        tree_sitter_typescript::LANGUAGE_TSX.into(),
        "tsx",
        tree_sitter_typescript::HIGHLIGHTS_QUERY,
    ),
    java: LangConfig::new(
        tree_sitter_java::LANGUAGE.into(),
        "java",
        tree_sitter_java::HIGHLIGHTS_QUERY,
    ),
    c: LangConfig::new(
        tree_sitter_c::LANGUAGE.into(),
        "c",
        tree_sitter_c::HIGHLIGHT_QUERY,
    ),
    cpp: LangConfig::new(
        tree_sitter_cpp::LANGUAGE.into(),
        "cpp",
        tree_sitter_cpp::HIGHLIGHT_QUERY,
    ),
    json: LangConfig::new(
        tree_sitter_json::LANGUAGE.into(),
        "json",
        tree_sitter_json::HIGHLIGHTS_QUERY,
    ),
});

fn config_for_lang(lang: &str) -> Option<&'static LangConfig> {
    let configs = &*CONFIGS;
    match lang {
        "rust" | "rs" => Some(&configs.rust),
        "bash" | "sh" | "shell" | "zsh" => Some(&configs.bash),
        "python" | "py" => Some(&configs.python),
        "go" | "golang" => Some(&configs.go),
        "javascript" | "js" => Some(&configs.javascript),
        "typescript" | "ts" => Some(&configs.typescript),
        "tsx" | "jsx" => Some(&configs.tsx),
        "java" => Some(&configs.java),
        "c" | "h" => Some(&configs.c),
        "cpp" | "cc" | "cxx" | "c++" => Some(&configs.cpp),
        "json" => Some(&configs.json),
        _ => None,
    }
}

fn highlight_code(content: &str, kind: &CodeBlockKind) -> Option<String> {
    let lang = match kind {
        CodeBlockKind::Fenced(x) if !x.is_empty() => x.as_ref(),
        _ => return None,
    };

    let lang_config = config_for_lang(lang)?;

    let mut highlighter = Highlighter::new();
    let highlights = highlighter
        .highlight(&lang_config.config, content.as_bytes(), None, |_| None)
        .ok()?;

    let mut html = String::with_capacity(content.len() * 2);
    let mut current_class: Option<&str> = None;

    for event in highlights {
        let event = event.ok()?;
        match event {
            HighlightEvent::Source { start, end } => {
                let text = &content[start..end];
                let escaped = html_escape(text);
                if let Some(class) = current_class {
                    html.push_str("<span class=\"");
                    html.push_str(class);
                    html.push_str("\">");
                    html.push_str(&escaped);
                    html.push_str("</span>");
                } else {
                    html.push_str(&escaped);
                }
            }
            HighlightEvent::HighlightStart(h) => {
                current_class = highlight_class(h.0);
            }
            HighlightEvent::HighlightEnd => {
                current_class = None;
            }
        }
    }

    Some(html)
}

fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}

impl HtmlError {
    fn not_implemented(message: impl ToString) -> Self {
        HtmlError::NotImplemented(message.to_string())
    }
    fn syntax(message: impl ToString) -> Self {
        HtmlError::Syntax(message.to_string())
    }
    fn component(name: impl ToString, msg: impl ToString) -> Self {
        HtmlError::CustomComponent {
            name: name.to_string(),
            msg: msg.to_string(),
        }
    }
}

/// renders a source code in a code block, with syntax highlighting if possible.
/// `cx`: the current markdown context
/// `source`: the source to render
/// `range`: the position of the code in the original source
fn render_code_block<'a, 'callback, F: Context<'a, 'callback>>(
    cx: F,
    source: String,
    k: &CodeBlockKind,
    range: Range<usize>,
) -> F::View {
    let code_attributes = ElementAttributes {
        on_click: Some(cx.make_md_handler(range, true)),
        ..Default::default()
    };

    match highlight_code(&source, k) {
        None => cx.el_with_attributes(
            Code,
            cx.el(Code, cx.el_text(source.into())),
            code_attributes,
        ),
        Some(x) => cx.el_span_with_inner_html(x, code_attributes),
    }
}

#[cfg(feature = "maths")]
/// `render_maths(content)` returns a html node
/// with the latex content `content` compiled inside
fn render_maths<'a, 'callback, F: Context<'a, 'callback>>(
    cx: F,
    content: &str,
    display_mode: MathMode,
    range: Range<usize>,
) -> Result<F::View, HtmlError> {
    use katex::{KatexContext, Settings};

    // The context caches fonts, macros, and environments – reuse it between renders.
    let ctx = KatexContext::default();

    // Start with the default configuration and tweak as needed.
    let settings = Settings::builder()
        .display_mode(display_mode == MathMode::Display)
        .build();

    let class_name = match display_mode {
        MathMode::Inline => "math-inline",
        MathMode::Display => "math-flow",
    };

    let callback = cx.make_md_handler(range, true);

    let attributes = ElementAttributes {
        classes: vec![class_name.to_string()],
        on_click: Some(callback),
        ..Default::default()
    };

    match katex::render_to_string(&ctx, content, &settings) {
        Ok(x) => Ok(cx.el_span_with_inner_html(x, attributes)),
        Err(_) => Err(HtmlError::Math),
    }
}
#[cfg(not(feature = "maths"))]
fn render_maths<'a, 'callback, F: Context<'a, 'callback>>(
    _cx: F,
    _content: &str,
    _display_mode: MathMode,
    _range: Range<usize>,
) -> Result<F::View, HtmlError> {
    Err(HtmlError::Unavailable(
        "Math was not enabled during compilation of the library. Please unable the `maths` feature"
            .into(),
    ))
}

/// `align_string(align)` gives the css string
/// that is used to align text according to `align`
fn align_string(align: Alignment) -> &'static str {
    match align {
        Alignment::Left => "text-align: left",
        Alignment::Right => "text-align: right",
        Alignment::Center => "text-align: center",
        Alignment::None => "",
    }
}

/// Manage the creation of a [`F::View`]
/// from a stream of markdown events
pub struct Renderer<'a, 'callback, 'c, I, F>
where
    I: Iterator<Item = (Event<'a>, Range<usize>)>,
    'callback: 'a,
    F: Context<'a, 'callback>,
{
    __marker: PhantomData<&'callback ()>,
    /// the markdown context
    cx: F,
    /// the stream of markdown [`Event`]s
    stream: &'c mut I,
    /// the alignment settings inside the current table
    column_alignment: Option<Vec<Alignment>>,
    /// the current horizontal index of the cell we are in.
    /// TODO: remove it
    cell_index: usize,
    /// the root tag that this renderer is rendering
    end_tag: Option<TagEnd>,
    /// the current component we are inside of.
    /// custom components doesn't allow nesting.
    current_component: Option<String>,
}

/// Returns true if `raw_html`:
/// - starts with '<'
/// - ends with '>'
/// - does not have any '<' or '>' in between.
///
/// TODO:
/// An string attribute can a ">" character.
fn can_be_custom_component(raw_html: &str) -> bool {
    let chars: Vec<_> = raw_html.trim().chars().collect();
    let len = chars.len();
    if len < 3 {
        return false;
    };
    let (fst, middle, last) = (chars[0], &chars[1..len - 1], chars[len - 1]);
    fst == '<' && last == '>' && middle.iter().all(|c| c != &'<' && c != &'>')
}

impl<'a, 'callback, 'c, I, F> Iterator for Renderer<'a, 'callback, 'c, I, F>
where
    I: Iterator<Item = (Event<'a>, Range<usize>)>,
    'callback: 'a,
    F: Context<'a, 'callback>,
{
    type Item = F::View;

    fn next(&mut self) -> Option<Self::Item> {
        use Event::*;
        let (item, range): (Event<'a>, Range<usize>) = self.stream.next()?;
        let range = range.clone();

        let cx = self.cx;

        let rendered = match item {
            Start(t) => self.render_tag(t, range),
            End(end) => {
                // check if the closing tag is the tag that was open
                // when this renderer was created
                match self.end_tag {
                    Some(t) if t == end => return None,
                    Some(t) => panic!("{end:?} is a wrong closing tag, expected {t:?}"),
                    None => panic!("didn't expect a closing tag"),
                }
            }
            Text(s) => Ok(cx.render_text(s, range)),
            Code(s) => Ok(cx.render_code(s, range)),
            InlineHtml(s) => self.html(&s, range),
            Html(raw_html) => self.html(&raw_html, range),
            FootnoteReference(_) => Err(HtmlError::not_implemented("footnotes refs")),
            SoftBreak => Ok(cx.el_text(" ".into())),
            HardBreak => Ok(self.cx.el_br()),
            Rule => Ok(cx.render_rule(range)),
            TaskListMarker(m) => Ok(cx.render_tasklist_marker(m, range)),
            InlineMath(content) => render_maths(self.cx, &content, MathMode::Inline, range),
            DisplayMath(content) => render_maths(self.cx, &content, MathMode::Display, range),
        };

        Some(rendered.unwrap_or_else(|e| {
            self.cx.el_with_attributes(
                Span,
                self.cx
                    .el_fragment(vec![self.cx.el_text(e.to_string().into()), self.cx.el_br()]),
                ElementAttributes {
                    classes: vec!["markdown-error".to_string()],
                    on_click: None,
                    ..Default::default()
                },
            )
        }))
    }
}

impl<'a, 'callback, 'c, I, F> Renderer<'a, 'callback, 'c, I, F>
where
    I: Iterator<Item = (Event<'a>, Range<usize>)>,
    F: Context<'a, 'callback>,
{
    /// creates a new renderer from a stream of events.
    /// It returns an iterator of [`F::View`]
    pub fn new(cx: F, events: &'c mut I) -> Self {
        Self {
            __marker: PhantomData,
            cx,
            stream: events,
            column_alignment: None,
            cell_index: 0,
            end_tag: None,
            current_component: None,
        }
    }

    /// Try to render `raw_html` as a custom component.
    /// - If it looks like `<Component/>` and Component is registered,
    ///     render the corresponding component.
    /// - If it looks like `<Component>`, and Component is registered,
    ///     extract markdown `<Component/>` is found.
    /// In any other cases, render the string as raw html.
    ///
    /// TODO: document (and fix?) how this behaves if given an open tag and not a closing one.
    fn html(&mut self, raw_html: &str, range: Range<usize>) -> Result<F::View, HtmlError> {
        // TODO: refactor

        match &self.current_component {
            Some(current_name) => {
                if self.end_tag.is_some() {
                    return Err(HtmlError::component(
                        raw_html,
                        "please make sure there is a newline before the end of your component",
                    ));
                }
                match CustomHtmlTag::from_str(raw_html, range.start) {
                    Ok(CustomHtmlTag::End(name)) if name == current_name => {
                        Ok(self.next().unwrap_or(self.cx.el_empty()))
                    }
                    Ok(_) => Err(HtmlError::component(
                        current_name,
                        "expected end of component",
                    )),
                    Err(e) => Err(HtmlError::syntax(e.message)),
                }
            }
            None => {
                // If making a new html tag, check if it has a name that is a valid custom component name.
                // If so, render it accordingly (as the component or error).
                // Otherwise fall through to the catch all inline html case below.
                if can_be_custom_component(raw_html) {
                    match CustomHtmlTag::from_str(raw_html, range.start) {
                        Ok(CustomHtmlTag::Inline(s)) => {
                            if self.cx.has_custom_component(s.name) {
                                return self.custom_component_inline(s);
                            }
                        }
                        Ok(CustomHtmlTag::End(name)) => {
                            if self.cx.has_custom_component(name) {
                                return Err(HtmlError::component(name, "expected start, not end"));
                            }
                        }
                        Ok(CustomHtmlTag::Start(s)) => {
                            if self.cx.has_custom_component(s.name) {
                                return self.custom_component(s);
                            }
                        }
                        Err(CustomHtmlTagError {
                            name: Some(name),
                            message,
                        }) => {
                            if self.cx.has_custom_component(&name) {
                                return Err(HtmlError::component(
                                    name,
                                    format!("not a valid component: {message}"),
                                ));
                            }
                        }
                        // Component did not parse as a custom component far enough to get a name, so fall through to raw html.
                        Err(CustomHtmlTagError {
                            name: None,
                            message: _,
                        }) => {}
                    };
                }
                // Not a custom component, so render html as is without and parsing/validation.
                Ok(self
                    .cx
                    .el_span_with_inner_html(raw_html.to_string(), Default::default()))
            }
        }
    }

    /// Convert attributes from [ComponentCall] format to [MdComponentProps] format.
    fn convert_attributes(input: ComponentCall) -> BTreeMap<String, MdComponentAttribute> {
        // TODO: this should probably unescape the attribute values.
        BTreeMap::from_iter(input.attributes.iter().map(|(k, v)| {
            (
                k.to_string(),
                MdComponentAttribute {
                    value: v.to_string(),
                    range: offset_range(
                        get_substr_range(input.full_string, v).unwrap(),
                        input.range_offset,
                    ),
                },
            )
        }))
    }

    /// Renders a custom component with children.
    fn custom_component(&mut self, description: ComponentCall) -> Result<F::View, HtmlError> {
        let name: &str = description.name;
        if !self.cx.has_custom_component(name) {
            return Err(HtmlError::component(name, "not a valid component"));
        }

        let sub_renderer = Renderer {
            __marker: PhantomData,
            cx: self.cx,
            stream: self.stream,
            column_alignment: self.column_alignment.clone(),
            cell_index: 0,
            end_tag: self.end_tag,
            current_component: Some(description.name.to_string()),
        };
        let children = self.cx.el_fragment(sub_renderer.collect());

        let props = MdComponentProps {
            attributes: Self::convert_attributes(description),
            children,
        };

        match self.cx.render_custom_component(name, props) {
            Ok(x) => Ok(x),
            Err(e) => Err(HtmlError::CustomComponent {
                name: name.to_string(),
                msg: e.0,
            }),
        }
    }

    /// Renders a custom component without children.
    fn custom_component_inline(
        &mut self,
        description: ComponentCall,
    ) -> Result<F::View, HtmlError> {
        let name: &str = description.name;
        if !self.cx.has_custom_component(name) {
            return Err(HtmlError::component(name, "not a valid component"));
        }

        let props = MdComponentProps {
            attributes: Self::convert_attributes(description),
            children: self.cx.el_empty(),
        };

        match self.cx.render_custom_component(name, props) {
            Ok(x) => Ok(x),
            Err(e) => Err(HtmlError::CustomComponent {
                name: name.to_string(),
                msg: e.0,
            }),
        }
    }

    /// renders events in a new renderer,
    /// recursively, until the end of the tag
    fn children(&mut self, tag: Tag<'a>) -> F::View {
        let sub_renderer = Renderer {
            __marker: PhantomData,
            cx: self.cx,
            stream: self.stream,
            column_alignment: self.column_alignment.clone(),
            cell_index: 0,
            end_tag: Some(tag.to_end()),
            current_component: self.current_component.clone(),
        };
        self.cx.el_fragment(sub_renderer.collect())
    }

    /// Collect all text inside `tag` until its closing `End` event.
    ///
    /// Older versions assumed there was exactly one `Event::Text` followed
    /// by the closing `Event::End`, which is not guaranteed with
    /// pulldown-cmark 0.13+ (empty or multi-chunk blocks). This version
    /// accumulates text and stops at the matching closing tag instead
    /// of panicking.
    fn children_text(&mut self, tag: Tag<'a>) -> Option<String> {
        let end = tag.to_end();
        let mut buf = String::new();

        for (event, _range) in self.stream.by_ref() {
            match event {
                pulldown_cmark::Event::Text(s) => buf.push_str(&s),
                pulldown_cmark::Event::SoftBreak | pulldown_cmark::Event::HardBreak => {
                    // Represent line breaks explicitly in the collected text
                    buf.push('\n');
                }
                pulldown_cmark::Event::End(e) if e == end => {
                    // We reached the closing tag for this block
                    return if buf.is_empty() { None } else { Some(buf) };
                }
                // These shouldn't normally appear inside code/metadata blocks,
                // but if they do, panic so this code can be updated to accommodate them.
                _ => panic!("Unexpected content inside of children_text emitted by pulldown-cmark"),
            }
        }

        // If we run out of events without seeing the closing tag,
        // just return whatever we collected (or None if empty).
        if buf.is_empty() {
            None
        } else {
            Some(buf)
        }
    }

    fn render_tag(&mut self, tag: Tag<'a>, range: Range<usize>) -> Result<F::View, HtmlError> {
        let mut cx = self.cx;
        Ok(match tag.clone() {
            Tag::HtmlBlock => self.children(tag),
            Tag::Paragraph => cx.el(Paragraph, self.children(tag)),
            Tag::Heading { level, .. } => cx.el(Heading(level as u8), self.children(tag)),
            Tag::BlockQuote(_) => cx.el(BlockQuote, self.children(tag)),
            Tag::CodeBlock(k) => {
                render_code_block(cx, self.children_text(tag).unwrap_or_default(), &k, range)
            }
            Tag::List(Some(n0)) => cx.el(Ol(n0 as i32), self.children(tag)),
            Tag::List(None) => cx.el(Ul, self.children(tag)),
            Tag::Item => cx.el(Li, self.children(tag)),
            Tag::Table(align) => {
                self.column_alignment = Some(align);
                cx.el(Table, self.children(tag))
            }
            Tag::TableHead => cx.el(Thead, self.children(tag)),
            Tag::TableRow => cx.el(Trow, self.children(tag)),
            Tag::TableCell => {
                let align = self.column_alignment.clone().unwrap()[self.cell_index];
                self.cell_index += 1;
                cx.el_with_attributes(
                    Tcell,
                    self.children(tag),
                    ElementAttributes {
                        style: Some(align_string(align).to_string()),
                        ..Default::default()
                    },
                )
            }
            Tag::Emphasis => cx.el(Italics, self.children(tag)),
            Tag::Strong => cx.el(Bold, self.children(tag)),
            Tag::Strikethrough => cx.el(StrikeThrough, self.children(tag)),
            Tag::Image {
                link_type,
                dest_url,
                title,
                ..
            } => {
                let description = LinkDescription {
                    url: dest_url.to_string(),
                    title: title.to_string(),
                    content: self.children(tag),
                    link_type,
                    image: true,
                };
                cx.render_link(description).map_err(HtmlError::Link)?
            }
            Tag::Link {
                link_type,
                dest_url,
                title,
                ..
            } => {
                let description = LinkDescription {
                    url: dest_url.to_string(),
                    title: title.to_string(),
                    content: self.children(tag),
                    link_type,
                    image: false,
                };
                cx.render_link(description).map_err(HtmlError::Link)?
            }
            Tag::FootnoteDefinition(_) => {
                return Err(HtmlError::not_implemented("footnote not implemented"))
            }
            Tag::MetadataBlock { .. } => {
                if let Some(text) = self.children_text(tag) {
                    cx.set_frontmatter(text)
                }
                cx.el_empty()
            }
            Tag::DefinitionList => {
                return Err(HtmlError::not_implemented(
                    "definition list not implemented",
                ))
            }
            Tag::DefinitionListTitle => {
                return Err(HtmlError::not_implemented(
                    "definition list not implemented",
                ))
            }
            Tag::DefinitionListDefinition => {
                return Err(HtmlError::not_implemented(
                    "definition list not implemented",
                ))
            }
            Tag::Superscript => {
                return Err(HtmlError::not_implemented("superscript not implemented"))
            }
            Tag::Subscript => return Err(HtmlError::not_implemented("subscript not implemented")),
        })
    }
}
