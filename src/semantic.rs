use oxc_allocator::{Allocator, TakeIn};
use oxc_ast::{
  AstBuilder, NONE,
  ast::{
    BinaryOperator, Expression, FormalParameter, FormalParameterKind, JSXAttribute,
    JSXAttributeItem, JSXAttributeName, JSXAttributeValue, JSXChild, JSXElement, JSXExpression,
    ObjectPropertyKind, PropertyKind, Statement,
  },
};
use oxc_ast_visit::VisitMut;
use oxc_parser::Parser;
use oxc_span::{GetSpan, SPAN, SourceType};

use crate::parser::{ParserReturn, VueOxcParser};

impl<'a> VueOxcParser<'a> {
  pub fn parse_for_semantic(self) -> ParserReturn<'a> {
    let allocator = self.ast.allocator;
    let source_type = self.source_type.clone();
    let source_text = self.source_text;
    let mut ret = self.parse();
    let body = &mut ret.program.body as *mut oxc_allocator::Vec<Statement>;
    let Statement::ExpressionStatement(stmt) = &mut unsafe { &mut *body }[0] else {
      return ret;
    };
    let Expression::JSXFragment(jsx_fragment) = &mut stmt.expression else {
      return ret;
    };
    let ast = AstBuilder::new(allocator);
    let children = &mut jsx_fragment.children as *mut oxc_allocator::Vec<JSXChild>;
    for child in unsafe { &mut *children } {
      match child {
        JSXChild::Element(element) => {
          if element
            .opening_element
            .name
            .get_identifier_name()
            .and_then(|name| Some(name.eq("script")))
            .unwrap_or_default()
          {
            let is_setup = element.opening_element.attributes.iter().any(|attr| {
              attr
                .as_attribute()
                .and_then(|attr| Some(attr.name.get_identifier().name.eq("setup")))
                .unwrap_or_default()
            });

            if let JSXChild::ExpressionContainer(child) = &mut element.children[0]
              && let JSXExpression::ArrowFunctionExpression(expr) = &mut child.expression
            {
              let mut statements = expr.body.statements.take_in(allocator);
              if is_setup {
                let last = unsafe { &mut *body }.last_mut().unwrap();
                statements.push(last.take_in(allocator));
                *last = ast.statement_expression(
                  SPAN,
                  ast.expression_arrow_function(
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
                    ast.function_body(SPAN, ast.vec(), statements),
                  ),
                );
              } else {
                let index = ret.program.body.len() - 1;
                unsafe { &mut *body }.splice(index..index, statements);
              }
            };
          }
        }
        _ => (),
      }
    }

    TransformDreictives {
      ast: &ast,
      allocator: &allocator,
      source_text,
      source_type,
    }
    .visit_program(&mut ret.program);
    ret
  }
}

struct TransformDreictives<'a, 'b> {
  ast: &'b AstBuilder<'a>,
  allocator: &'a Allocator,
  source_type: SourceType,
  source_text: &'b str,
}

impl<'a, 'b> VisitMut<'a> for TransformDreictives<'a, 'b> {
  fn visit_jsx_element(&mut self, it: &mut JSXElement<'a>) {
    let mut v_slot = None;
    let mut v_for = None;
    let _it = it as *mut JSXElement;
    for dir in &mut unsafe { &mut *_it }.opening_element.attributes {
      let JSXAttributeItem::Attribute(dir) = dir else {
        continue;
      };

      let dir_name = match &mut dir.name {
        JSXAttributeName::Identifier(name) => name.name,
        JSXAttributeName::NamespacedName(name) => name.namespace.name,
      };

      match dir_name.as_str() {
        "v-slot" => v_slot = Some(dir),
        "v-for" => v_for = Some(dir),
        _ => (),
      }
    }

    if let Some(v_slot) = v_slot {
      self.transform_v_slot(v_slot, it)
    }
    if let Some(v_for) = v_for {
      self.transform_v_for(v_for, it)
    }
    self.visit_jsx_children(&mut it.children);
  }
}

impl<'a, 'b> TransformDreictives<'a, 'b> {
  fn transform_v_slot(&self, dir: &mut JSXAttribute<'a>, it: &mut JSXElement<'a>) {
    let ast = self.ast;
    let mut name = ast.property_key_static_identifier(
      SPAN,
      match &mut dir.name {
        JSXAttributeName::Identifier(_) => "default",
        JSXAttributeName::NamespacedName(name) => name.name.name.as_str(),
      },
    );
    let mut computed = false;

    let params = if let Some(value) = &mut dir.value
      && let JSXAttributeValue::ExpressionContainer(value) = value
    {
      let mut value = value.expression.take_in(self.allocator);
      let expr = if let JSXExpression::ObjectExpression(value) = &mut value
        && let Some(ObjectPropertyKind::ObjectProperty(prop)) = value.properties.get_mut(0)
        && prop.computed
        && prop.span.eq(&SPAN)
      {
        computed = true;
        name = prop.key.take_in(self.allocator);
        &mut prop.value
      } else {
        value.to_expression_mut()
      };
      self.parse_params(expr)
    } else {
      ast.vec()
    };

    it.children = ast.vec1(
      ast.jsx_child_expression_container(
        SPAN,
        ast
          .expression_object(
            SPAN,
            ast.vec_from_array([ast.object_property_kind_object_property(
              SPAN,
              PropertyKind::Init,
              name,
              ast.expression_arrow_function(
                SPAN,
                true,
                false,
                NONE,
                ast.formal_parameters(
                  SPAN,
                  FormalParameterKind::ArrowFormalParameters,
                  params,
                  NONE,
                ),
                NONE,
                ast.function_body(
                  SPAN,
                  ast.vec(),
                  ast.vec1(ast.statement_expression(
                    SPAN,
                    ast.expression_jsx_fragment(
                      SPAN,
                      ast.jsx_opening_fragment(SPAN),
                      it.children.take_in(self.allocator),
                      ast.jsx_closing_fragment(SPAN),
                    ),
                  )),
                ),
              ),
              false,
              false,
              computed,
            )]),
          )
          .into(),
      ),
    )
  }

  fn transform_v_for(&self, dir: &mut JSXAttribute<'a>, it: &mut JSXElement<'a>) {
    let ast = self.ast;

    let params = if let Some(value) = &mut dir.value
      && let JSXAttributeValue::ExpressionContainer(value) = value
      && let JSXExpression::BinaryExpression(expr) = &mut value.expression
      && expr.operator == BinaryOperator::In
    {
      let mut left = expr.left.take_in(self.allocator);
      self.parse_params(&mut left)
    } else {
      ast.vec()
    };

    it.children = ast.vec1(
      ast.jsx_child_expression_container(
        SPAN,
        ast
          .expression_arrow_function(
            SPAN,
            true,
            false,
            NONE,
            ast.formal_parameters(
              SPAN,
              FormalParameterKind::ArrowFormalParameters,
              params,
              NONE,
            ),
            NONE,
            ast.function_body(
              SPAN,
              ast.vec(),
              ast.vec1(ast.statement_expression(
                SPAN,
                ast.expression_jsx_fragment(
                  SPAN,
                  ast.jsx_opening_fragment(SPAN),
                  it.children.take_in(self.allocator),
                  ast.jsx_closing_fragment(SPAN),
                ),
              )),
            ),
          )
          .into(),
      ),
    )
  }

  fn parse_params(&self, expr: &mut Expression) -> oxc_allocator::Vec<'a, FormalParameter<'a>> {
    let span = expr.without_parentheses().span();
    if !span.eq(&SPAN)
      && let Expression::ArrowFunctionExpression(mut expr) = Parser::new(
        self.allocator,
        self
          .ast
          .atom(&format!(
            "/*{}*/({})=>{{}}",
            "*".repeat(span.start as usize - 5),
            span.source_text(self.source_text)
          ))
          .as_str(),
        self.source_type,
      )
      .parse_expression()
      .unwrap()
    {
      expr.params.items.take_in(self.allocator)
    } else {
      self.ast.vec()
    }
  }
}
