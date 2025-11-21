use std::fs;
use std::rc::Rc;

use oxc_allocator::{self, Allocator, TakeIn};
use oxc_ast::ast::{
  Expression, FormalParameterKind, JSXAttribute, JSXAttributeItem, JSXAttributeName,
  JSXAttributeValue, JSXElement, JSXElementName, JSXExpression, JSXExpressionContainer,
  JSXOpeningElement, Program,
};
use oxc_ast::{AstBuilder, NONE};
use oxc_codegen::{Codegen, CodegenOptions};
use oxc_span::{Atom, SPAN, SourceType, Span};
use serde::Serialize;
use serde_json::to_string_pretty;
use vue_compiler_core::SourceLocation;
use vue_compiler_core::compiler::{BaseCompiler, TemplateCompiler};
use vue_compiler_core::error::{ErrorHandler, VecErrorHandler};
use vue_compiler_core::parser::{AstNode, Directive, DirectiveArg, ElemProp, ParseOption, Parser};
use vue_compiler_core::scanner::{ScanOption, Scanner};
use vue_compiler_core::util::{PropPattern, find_prop, prop_finder};

pub struct NoopErrorHandler;
impl ErrorHandler for NoopErrorHandler {
  fn on_error(&self, _: vue_compiler_core::error::CompilationError) {}
}

pub trait SourceLocatonSpan {
  fn span(&self) -> Span;
}

impl SourceLocatonSpan for SourceLocation {
  fn span(self: &Self) -> Span {
    Span::new(self.start.offset as u32, self.end.offset as u32)
  }
}

pub struct VueToJsx<'a> {
  allocator: &'a Allocator,
  source: &'a str,
  ast: AstBuilder<'a>,
  source_type: SourceType,
}
impl<'a> VueToJsx<'a> {
  pub fn new(allocator: &'a Allocator, source: &'a str) -> Self {
    let ast = AstBuilder::new(allocator);
    Self {
      allocator,
      source,
      ast,
      source_type: SourceType::jsx(),
    }
  }

  pub fn parser(&'a mut self) -> Program<'a> {
    let parser = Parser::new(Default::default());
    let scanner = Scanner::new(ScanOption::default());
    let tokens = scanner.scan(self.source, NoopErrorHandler);
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
            let loc = node.children[0].get_location();
            let source = &self.source[loc.start.offset..loc.end.offset];
            let mut program = oxc_parser::Parser::new(
              self.allocator,
              source,
              SourceType::from_extension(lang).unwrap(),
            )
            .parse()
            .program;
            statements.push(
              ast.statement_expression(
                node.location.span(),
                ast.expression_jsx_element(
                  node.location.span(),
                  ast.jsx_opening_element(
                    Span::new(
                      node.location.start.offset as u32,
                      node.children[0].get_location().start.offset as u32,
                    ),
                    ast.jsx_element_name_identifier(
                      Span::new(
                        node.location.start.offset as u32 + 1,
                        (node.location.start.offset + 1 + node.tag_name.len()) as u32,
                      ),
                      ast.atom(node.tag_name),
                    ),
                    NONE,
                    ast.vec_from_iter(
                      node
                        .properties
                        .into_iter()
                        .map(|prop| self.gen_jsx_attribute(prop)),
                    ),
                  ),
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
                      ast.function_body(SPAN, ast.vec(), program.body.take_in(self.allocator)),
                    )),
                  )),
                  Some(ast.jsx_closing_element(
                    Span::new(
                      node.children.last().unwrap().get_location().end.offset as u32,
                      node.location.end.offset as u32,
                    ),
                    ast.jsx_element_name_identifier(
                      Span::new(
                        node.children.last().unwrap().get_location().end.offset as u32 + 1,
                        (node.children.last().unwrap().get_location().end.offset
                          + 1
                          + node.tag_name.len()) as u32,
                      ),
                      ast.atom(node.tag_name),
                    ),
                  )),
                ),
              ),
            );
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
      self.source,
      ast.vec(),
      None,
      ast.vec(),
      statements.into(),
    )
  }

  fn gen_jsx_attribute(&self, prop: ElemProp<'a>) -> JSXAttributeItem<'a> {
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
            self.gen_argument(dir.argument.unwrap(), dir.modifiers),
          ),
          _ => {
            if let Some(argument) = &dir.argument {
              match argument {
                DirectiveArg::Static(arg) => ast.jsx_attribute_name_namespaced_name(
                  dir.head_loc.span(),
                  ast.jsx_identifier(
                    Span::new(
                      dir.location.start.offset as u32,
                      (dir.location.start.offset + 2 + dir.name.len()) as u32,
                    ),
                    ast.atom(&format!("v-{}", dir.name)),
                  ),
                  ast.jsx_identifier(
                    Span::new(
                      dir.location.start.offset as u32 + 2,
                      (dir.location.start.offset + 2 + arg.len()) as u32,
                    ),
                    self.gen_argument(dir.argument.unwrap(), dir.modifiers),
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
                  self.gen_modifiers(dir.modifiers)
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

  fn gen_argument(&self, argument: DirectiveArg, modifiers: Vec<&'a str>) -> Atom<'a> {
    self.ast.atom(&format!(
      "{}{}",
      match argument {
        DirectiveArg::Static(arg) => arg,
        DirectiveArg::Dynamic(arg) => {
          "unimplemented"
        }
      },
      self.gen_modifiers(modifiers)
    ))
  }
  fn gen_modifiers(&self, modifiers: Vec<&'a str>) -> String {
    if !modifiers.is_empty() {
      format!("_{}", modifiers.join("_"))
    } else {
      String::new()
    }
  }
}
