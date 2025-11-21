use oxc_allocator::{self, Allocator};
use oxc_ast::ast::{
  Expression, FormalParameterKind, JSXAttributeItem, JSXChild, JSXExpression, Program,
};
use oxc_ast::{AstBuilder, NONE};
use oxc_span::{Atom, SPAN, SourceType, Span};
use vue_compiler_core::SourceLocation;
use vue_compiler_core::error::ErrorHandler;
use vue_compiler_core::parser::{AstNode, DirectiveArg, ElemProp, Element, Parser};
use vue_compiler_core::scanner::{ScanOption, Scanner};
use vue_compiler_core::util::find_prop;

pub struct NoopErrorHandler;
impl ErrorHandler for NoopErrorHandler {
  fn on_error(&self, _: vue_compiler_core::error::CompilationError) {}
}

pub trait SourceLocatonSpan {
  fn span(&self) -> Span;
}

impl SourceLocatonSpan for SourceLocation {
  fn span(&self) -> Span {
    Span::new(self.start.offset as u32, self.end.offset as u32)
  }
}

pub struct VueOxcParser<'a> {
  allocator: &'a Allocator,
  source_text: &'a str,
  ast: AstBuilder<'a>,
  source_type: SourceType,
}
impl<'a> VueOxcParser<'a> {
  pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
    Self {
      allocator,
      source_text,
      source_type: SourceType::jsx(),
      ast: AstBuilder::new(allocator),
    }
  }

  pub fn parser(&'a mut self) -> Program<'a> {
    let parser = Parser::new(Default::default());
    let scanner = Scanner::new(ScanOption::default());
    let tokens = scanner.scan(self.source_text, NoopErrorHandler);
    let result = parser.parse(tokens, NoopErrorHandler);

    let ast = &self.ast;
    let mut statements = ast.vec();
    for child in result.children {
      match child {
        AstNode::Element(node) => {
          let lang = find_prop(&node, "lang")
            .map(|p| match p.get_ref() {
              ElemProp::Attr(p) => p.value.as_ref().map(|value| value.content.raw),
              _ => None,
            })
            .flatten()
            .unwrap_or("js");
          if lang.starts_with("ts") {
            self.source_type = SourceType::tsx();
          }

          if node.tag_name == "script" {
            let children = if let Some(child) = node.children.get(0) {
              let source = child.get_location().span().source_text(&self.source_text);
              oxc_parser::Parser::new(
                self.allocator,
                source,
                SourceType::from_extension(lang).unwrap(),
              )
              .parse()
              .program
              .body
            } else {
              ast.vec()
            };
            statements.push(ast.statement_expression(
              node.location.span(),
              self.parse_jsx_element(
                node,
                ast.vec1(ast.jsx_child_expression_container(
                  SPAN,
                  JSXExpression::ArrowFunctionExpression(ast.alloc_arrow_function_expression(
                    SPAN,
                    false,
                    false,
                    NONE,
                    ast.formal_parameters(
                      SPAN,
                      FormalParameterKind::ArrowFormalParameters,
                      ast.vec(),
                      NONE,
                    ),
                    NONE,
                    ast.function_body(SPAN, ast.vec(), children),
                  )),
                )),
              ),
            ));
          } else if node.tag_name == "template" {
            for node in node.children {
              if let AstNode::Element(node) = &node {}
            }
          }
        }
        _ => (),
      }
    }
    ast.program(
      SPAN,
      self.source_type,
      self.source_text,
      ast.vec(),
      None,
      ast.vec(),
      statements,
    )
  }

  fn element_span(&self, node: &Element) -> (Span, Span) {
    let start = node.location.start.offset;
    if let Some(child) = node.children.get(0) {
      (
        Span::new(start as u32, child.get_location().start.offset as u32),
        Span::new(
          node.children.last().unwrap().get_location().end.offset as u32,
          node.location.end.offset as u32,
        ),
      )
    } else {
      let index = if let Some(prop) = node.properties.get(0) {
        self.offset(match prop {
          ElemProp::Attr(prop) => prop.location.end.offset,
          ElemProp::Dir(prop) => prop.location.end.offset,
        })
      } else {
        self.offset(start) + node.tag_name.len()
      } as u32
        + 1;
      (Span::new(index, index), Span::new(index, index))
    }
  }

  fn offset(&self, start: usize) -> usize {
    start
      + self.source_text[start..]
        .chars()
        .take_while(|c| c.is_whitespace())
        .count()
  }

  fn roffset(&self, end: usize) -> usize {
    end
      - self.source_text[..end]
        .chars()
        .rev()
        .take_while(|c| c.is_whitespace())
        .count()
  }

  fn parse_jsx_element(
    &self,
    node: Element<'a>,
    children: oxc_allocator::Vec<'a, JSXChild<'a>>,
  ) -> Expression<'a> {
    let ast = self.ast;
    let (open_span, close_span) = self.element_span(&node);
    let tag_name_start = self.offset(node.location.start.offset + 1);
    let end_tag_name_end = self.roffset(node.location.end.offset - 1);
    ast.expression_jsx_element(
      node.location.span(),
      ast.jsx_opening_element(
        open_span,
        ast.jsx_element_name_identifier(
          Span::new(
            tag_name_start as u32,
            (tag_name_start + node.tag_name.len()) as u32,
          ),
          ast.atom(node.tag_name),
        ),
        NONE,
        ast.vec_from_iter(
          node
            .properties
            .into_iter()
            .map(|prop| self.parse_jsx_attribute(prop)),
        ),
      ),
      children,
      Some(ast.jsx_closing_element(
        close_span,
        ast.jsx_element_name_identifier(
          Span::new(
            (end_tag_name_end - node.tag_name.len()) as u32,
            end_tag_name_end as u32,
          ),
          ast.atom(&node.tag_name),
        ),
      )),
    )
  }

  fn parse_jsx_attribute(&self, prop: ElemProp<'a>) -> JSXAttributeItem<'a> {
    let ast = self.ast;
    match prop {
      ElemProp::Attr(attr) => ast.jsx_attribute_item_attribute(
        attr.name_loc.span(),
        ast.jsx_attribute_name_identifier(attr.name_loc.span(), ast.atom(attr.name)),
        if let Some(value) = attr.value {
          Some(ast.jsx_attribute_value_string_literal(
            value.location.span(),
            ast.atom(&value.content.raw),
            None,
          ))
        } else {
          None
        },
      ),
      ElemProp::Dir(dir) => ast.jsx_attribute_item_attribute(
        dir.head_loc.span(),
        match dir.name {
          "bind" => ast.jsx_attribute_name_identifier(
            dir.head_loc.span(),
            self.parse_argument(dir.argument.unwrap(), dir.modifiers),
          ),
          _ => {
            if let Some(argument) = &dir.argument {
              let namespace_start = dir.location.start.offset as u32;
              let namespace_end = namespace_start + 2 + dir.name.len() as u32;
              match argument {
                DirectiveArg::Static(arg) => ast.jsx_attribute_name_namespaced_name(
                  dir.head_loc.span(),
                  ast.jsx_identifier(
                    Span::new(namespace_start, namespace_end),
                    ast.atom(&format!("v-{}", dir.name)),
                  ),
                  ast.jsx_identifier(
                    Span::new(namespace_end + 1, namespace_end + 1 + arg.len() as u32),
                    self.parse_argument(dir.argument.unwrap(), dir.modifiers),
                  ),
                ),
                DirectiveArg::Dynamic(arg) => {
                  unimplemented!()
                }
              }
            } else {
              ast.jsx_attribute_name_identifier(
                dir.head_loc.span(),
                self.ast.atom(&format!(
                  "v-{}{}",
                  dir.name,
                  self.parse_modifiers(dir.modifiers)
                )),
              )
            }
          }
        },
        if let Some(expr) = dir.expression {
          Some(
            ast.jsx_attribute_value_expression_container(
              expr.location.span(),
              oxc_parser::Parser::new(self.allocator, &expr.content.raw, self.source_type)
                .parse_expression()
                .unwrap()
                .into(),
            ),
          )
        } else {
          None
        },
      ),
    }
  }

  fn parse_argument(&self, argument: DirectiveArg, modifiers: Vec<&'a str>) -> Atom<'a> {
    self.ast.atom(&format!(
      "{}{}",
      match argument {
        DirectiveArg::Static(arg) => arg,
        DirectiveArg::Dynamic(arg) => {
          "unimplemented"
        }
      },
      self.parse_modifiers(modifiers)
    ))
  }
  fn parse_modifiers(&self, modifiers: Vec<&'a str>) -> String {
    if !modifiers.is_empty() {
      format!("_{}", modifiers.join("_"))
    } else {
      String::new()
    }
  }
}
