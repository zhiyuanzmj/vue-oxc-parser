use std::cell::RefCell;

use oxc_allocator::{self, Allocator, TakeIn};
use oxc_ast::ast::{
  AssignmentTarget, Expression, FormalParameterKind, JSXAttributeItem, JSXChild, JSXExpression,
  Program, Statement,
};
use oxc_ast::{AstBuilder, Comment, CommentKind, NONE};
use oxc_ast_visit::VisitMut;
use oxc_span::{Atom, GetSpan, SPAN, SourceType, Span};
use vue_compiler_core::SourceLocation;
use vue_compiler_core::error::ErrorHandler;
use vue_compiler_core::parser::{
  AstNode, DirectiveArg, ElemProp, Element, ParseOption, Parser, SourceNode, TextNode,
  WhitespaceStrategy,
};
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
  comments: RefCell<oxc_allocator::Vec<'a, Comment>>,
}
impl<'a> VueOxcParser<'a> {
  pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
    let ast = AstBuilder::new(allocator);
    Self {
      allocator,
      source_text,
      source_type: SourceType::jsx(),
      ast,
      comments: RefCell::from(ast.vec()),
    }
  }

  pub fn parse(&'a mut self) -> Program<'a> {
    let parser = Parser::new(ParseOption {
      whitespace: WhitespaceStrategy::Preserve,
      ..Default::default()
    });
    let scanner = Scanner::new(ScanOption::default());
    let tokens = scanner.scan(self.source_text, NoopErrorHandler);
    let result = parser.parse(tokens, NoopErrorHandler);

    let ast = &self.ast;
    let mut children = ast.vec();
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
            let script_block = if let Some(child) = node.children.get(0) {
              let span = child.get_location().span();
              let source = span.source_text(&self.source_text);
              oxc_parser::Parser::new(
                self.allocator,
                ast
                  .atom(&format!("{}{source}", " ".repeat(span.start as usize)))
                  .as_str(),
                SourceType::from_extension(lang).unwrap(),
              )
              .parse()
              .program
              .body
            } else {
              ast.vec()
            };
            children.push(self.parse_element(
              node,
              Some(ast.vec1(ast.jsx_child_expression_container(
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
                  ast.function_body(SPAN, ast.vec(), script_block),
                )),
              ))),
            ));
          } else if node.tag_name == "template" {
            children.push(self.parse_element(node, None))
          }
        }
        AstNode::Text(text) => children.push(self.parse_text(text)),
        AstNode::Comment(comment) => children.push(self.parse_comment(comment)),
        AstNode::Interpolation(interp) => children.push(self.parse_interpolation(interp)),
      }
    }
    ast.program(
      SPAN,
      self.source_type,
      self.source_text,
      self.comments.borrow_mut().take_in(ast.allocator),
      None,
      ast.vec(),
      ast.vec1(ast.statement_expression(
        SPAN,
        ast.expression_jsx_fragment(
          SPAN,
          ast.jsx_opening_fragment(SPAN),
          children,
          ast.jsx_closing_fragment(SPAN),
        ),
      )),
    )
  }

  fn parse_children(
    &self,
    start: u32,
    end: u32,
    children: Vec<AstNode<'a>>,
  ) -> oxc_allocator::Vec<'a, JSXChild<'a>> {
    let ast = self.ast;
    if children.is_empty() {
      return ast.vec();
    }
    let mut result = self.ast.vec_with_capacity(children.len() + 2);

    if let Some(first) = children.get(0)
      && matches!(first, AstNode::Element(_) | AstNode::Interpolation(_))
      && start != first.get_location().start.offset as u32
    {
      let span = Span::new(start, first.get_location().start.offset as u32);
      let value = span.source_text(self.source_text);
      result.push(ast.jsx_child_text(span, value, Some(ast.atom(value))))
    }

    let last = if let Some(last) = children.last()
      && matches!(last, AstNode::Element(_) | AstNode::Interpolation(_))
      && end != last.get_location().end.offset as u32
    {
      let span = Span::new(last.get_location().end.offset as u32, end);
      let value = span.source_text(self.source_text);
      Some(ast.jsx_child_text(span, value, Some(ast.atom(value))))
    } else {
      None
    };

    result.extend(children.into_iter().filter_map(|child| match child {
      AstNode::Element(node) => Some(self.parse_element(node, None)),
      AstNode::Text(text) => Some(self.parse_text(text)),
      AstNode::Comment(comment) => Some(self.parse_comment(comment)),
      AstNode::Interpolation(interp) => Some(self.parse_interpolation(interp)),
    }));

    if let Some(last) = last {
      result.push(last)
    }

    result
  }

  fn parse_element(
    &self,
    node: Element<'a>,
    children: Option<oxc_allocator::Vec<'a, JSXChild<'a>>>,
  ) -> JSXChild<'a> {
    let ast = self.ast;

    let open_element_span = {
      let start = node.location.start.offset;
      let end = if let Some(prop) = node.properties.last() {
        self.offset(match prop {
          ElemProp::Attr(prop) => prop.location.end.offset,
          ElemProp::Dir(prop) => prop.location.end.offset,
        })
      } else {
        start + 1 + node.tag_name.len()
      } + 1;
      Span::new(start as u32, end as u32)
    };

    let end_element_span = {
      let end = node.location.end.offset;
      let start = (self.roffset(end) - node.tag_name.len() - 3) as u32;
      Span::new(start as u32, end as u32)
    };

    ast.jsx_child_element(
      node.location.span(),
      ast.jsx_opening_element(
        open_element_span,
        ast.jsx_element_name_identifier(
          Span::new(
            open_element_span.start + 1,
            open_element_span.start + 1 + node.tag_name.len() as u32,
          ),
          ast.atom(node.tag_name),
        ),
        NONE,
        ast.vec_from_iter(
          node
            .properties
            .into_iter()
            .map(|prop| self.parse_attribute(prop)),
        ),
      ),
      if let Some(children) = children {
        children
      } else {
        self.parse_children(open_element_span.end, end_element_span.start, node.children)
      },
      Some(ast.jsx_closing_element(
        end_element_span,
        ast.jsx_element_name_identifier(
          Span::new(
            end_element_span.start + 2,
            end_element_span.start + 2 + node.tag_name.len() as u32,
          ),
          ast.atom(&node.tag_name),
        ),
      )),
    )
  }

  fn parse_attribute(&self, prop: ElemProp<'a>) -> JSXAttributeItem<'a> {
    let ast = self.ast;
    match prop {
      ElemProp::Attr(attr) => {
        let attr_end = self.roffset(attr.location.end.offset) as u32;
        let attr_span = Span::new(attr.location.start.offset as u32, attr_end);
        ast.jsx_attribute_item_attribute(
          attr_span,
          ast.jsx_attribute_name_identifier(attr.name_loc.span(), ast.atom(attr.name)),
          if let Some(value) = attr.value {
            Some(ast.jsx_attribute_value_string_literal(
              Span::new(value.location.span().start as u32 + 1, attr_end - 1),
              ast.atom(&value.content.raw),
              None,
            ))
          } else {
            None
          },
        )
      }
      ElemProp::Dir(dir) => {
        let dir_start = dir.location.start.offset as u32 + if dir.name == "bind" { 1 } else { 0 };
        let dir_end = self.roffset(dir.location.end.offset) as u32;
        ast.jsx_attribute_item_attribute(
          Span::new(dir_start, dir_end),
          match dir.name {
            "bind" => ast.jsx_attribute_name_identifier(
              Span::new(dir_start, dir.head_loc.end.offset as u32),
              self.parse_argument(dir.argument.unwrap(), dir.modifiers),
            ),
            _ => {
              if let Some(argument) = &dir.argument {
                let namespace_end = dir_start + 2 + dir.name.len() as u32;
                match argument {
                  DirectiveArg::Static(arg) => ast.jsx_attribute_name_namespaced_name(
                    dir.head_loc.span(),
                    ast.jsx_identifier(
                      Span::new(dir_start, namespace_end),
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
            let span = Span::new(expr.location.start.offset as u32 + 1, dir_end - 1);
            let mut expression =
              self.parse_expression(expr.content.raw, expr.location.start.offset);
            if dir.name == "slot" || dir.name == "for" {
              DefaultValueToAssignment::new(self).visit_expression(&mut expression);
            }
            Some(ast.jsx_attribute_value_expression_container(span, expression.into()))
          } else {
            None
          },
        )
      }
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

  fn parse_text(&self, text: TextNode<'a>) -> JSXChild<'a> {
    let ast = self.ast;
    let raw = ast.atom(text.text[0].raw);
    ast.jsx_child_text(text.location.span(), raw, Some(raw))
  }

  fn parse_comment(&self, comment: SourceNode<'a>) -> JSXChild<'a> {
    let ast = self.ast;
    let span = comment.location.span();
    self.comments.borrow_mut().push(Comment::new(
      span.start + 1,
      span.end - 1,
      CommentKind::Block,
    ));
    ast.jsx_child_expression_container(
      span,
      ast.jsx_expression_empty_expression(Span::new(span.start + 1, span.end - 1)),
    )
  }

  fn parse_interpolation(&self, introp: SourceNode<'a>) -> JSXChild<'a> {
    let ast = self.ast;
    let span = Span::new(
      introp.location.start.offset as u32 + 1,
      introp.location.end.offset as u32 - 1,
    );
    ast.jsx_child_expression_container(
      span,
      self
        .parse_expression(introp.source, span.start as usize)
        .into(),
    )
  }

  pub fn parse_expression(&self, source: &'a str, start: usize) -> Expression<'a> {
    let ast = &self.ast;
    let source_text = ast
      .atom(&format!("{}({})", " ".repeat(start), &source))
      .as_str();
    let mut program = oxc_parser::Parser::new(self.allocator, source_text, self.source_type)
      .parse()
      .program;
    let Some(Statement::ExpressionStatement(stmt)) = program.body.get_mut(0) else {
      panic!("parse expression error")
    };
    let Expression::ParenthesizedExpression(expression) = &mut stmt.expression else {
      unreachable!()
    };
    expression.expression.take_in(self.allocator)
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
}

struct DefaultValueToAssignment<'a, 'ctx> {
  context: &'ctx VueOxcParser<'a>,
}
impl<'a, 'ctx> DefaultValueToAssignment<'a, 'ctx> {
  pub fn new(context: &'ctx VueOxcParser<'a>) -> Self {
    Self { context }
  }
}
impl<'a, 'ctx> VisitMut<'a> for DefaultValueToAssignment<'a, 'ctx> {
  fn visit_object_property(&mut self, it: &mut oxc_ast::ast::ObjectProperty<'a>) {
    if it.value.span().end != it.span.end {
      let ast = &self.context.ast;
      let value_start = it.value.span().start as usize;
      let source = &self.context.source_text[value_start..it.span.end as usize];
      let mut expression = oxc_parser::Parser::new(
        ast.allocator,
        ast
          .atom(&format!("{}{}", " ".repeat(it.span.start as usize), source))
          .as_str(),
        self.context.source_type,
      )
      .parse_expression()
      .unwrap();
      if let Expression::AssignmentExpression(expr) = &mut expression {
        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &mut expr.left {
          id.span = SPAN;
        }
        it.value = expression.take_in(ast.allocator);
      }
    }
  }
}
