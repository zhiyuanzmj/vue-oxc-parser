use std::{fs, path::PathBuf};

use insta::assert_snapshot;
use oxc_allocator::Allocator;
use oxc_codegen::{CodegenOptions, CommentOptions};
use vue_oxc_parser::parser::VueOxcParser;

#[test]
fn parser_test() {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("tests/fixtures/ElTable.vue");
  let source = fs::read_to_string(path).unwrap();
  let allocator = Allocator::new();
  let mut vue_oxc_parser = VueOxcParser::new(&allocator, &source);
  let program = vue_oxc_parser.parse();
  let result = oxc_codegen::Codegen::new()
    .with_options(CodegenOptions {
      comments: CommentOptions::default(),
      ..Default::default()
    })
    .build(&program);
  dbg!(result.legal_comments);
  assert_snapshot!(result.code);
}
