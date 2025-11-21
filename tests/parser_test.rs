use std::{fs, path::PathBuf};

use insta::assert_snapshot;
use oxc_allocator::Allocator;
use vue_oxc_parser::parser::VueOxcParser;

#[test]
fn parser_test() {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("tests/fixtures/ElTable.vue");
  let source = fs::read_to_string(path).unwrap();
  let allocator = Allocator::new();
  let mut vue_to_jsx = VueOxcParser::new(&allocator, &source);
  let program = vue_to_jsx.parser();
  let result = oxc_codegen::Codegen::new().build(&program);
  assert_snapshot!(result.code);
}
