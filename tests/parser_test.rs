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
  let program = VueOxcParser::new(&allocator, &source).parse().program;
  let result = oxc_codegen::Codegen::new()
    .with_options(CodegenOptions {
      comments: CommentOptions::default(),
      ..Default::default()
    })
    .build(&program);
  assert_snapshot!(result.code);
}

#[test]
fn parser_semantic_test() {
  let allocator = Allocator::new();
  let program = VueOxcParser::new(
    &allocator,
    r#"<template>
      <Comp v-slot:name="{ foo }">{{ foo }}</Comp>
      <Comp v-slot:[name(foo)]="{ foo }">{{ foo }}</Comp>

      <Comp>
        <template #default="{foo}">{{ foo }}</template>
        <template #name="{bar}">{{ bar }}</template>
      </Comp>

      <Comp v-for="name in list">
        {{ name }}
      </Comp>

      <Comp>
        <template #[name]="scope" v-for="(name, Slot) in slots">
          <Slot v-bind={scope} />
        </template>
      </Comp>
    </template>"#,
  )
  .parse_for_semantic()
  .program;
  let result = oxc_codegen::Codegen::new()
    .with_options(CodegenOptions {
      comments: CommentOptions::default(),
      ..Default::default()
    })
    .build(&program);
  assert_snapshot!(result.code);
}
