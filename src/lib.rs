use std::path::PathBuf;

use oxc_allocator::Allocator;
use oxc_codegen::CodegenOptions;
use wasm_bindgen::prelude::wasm_bindgen;
pub mod utils;

use crate::parser::VueOxcParser;

pub mod parser;
pub mod semantic;

#[wasm_bindgen]
pub struct ParseResult {
  #[wasm_bindgen(getter_with_clone)]
  pub code: String,
  #[wasm_bindgen(getter_with_clone)]
  pub map: String,
  #[wasm_bindgen(getter_with_clone)]
  pub ast: String,
}

#[wasm_bindgen]
pub fn parse(source_text: String, filename: String) -> ParseResult {
  let allocator = Allocator::new();
  let program = VueOxcParser::new(&allocator, &source_text).parse().program;
  let result = oxc_codegen::Codegen::new()
    .with_options(CodegenOptions {
      source_map_path: Some(PathBuf::from(&filename)),
      ..Default::default()
    })
    .build(&program);

  ParseResult {
    code: result.code,
    map: result.map.unwrap().to_json_string(),
    ast: program.to_pretty_estree_ts_json(false),
  }
}

#[wasm_bindgen]
pub fn parse_for_semantic(source_text: String, filename: String) -> ParseResult {
  let allocator = Allocator::new();
  let program = VueOxcParser::new(&allocator, &source_text)
    .parse_for_semantic()
    .program;
  let result = oxc_codegen::Codegen::new()
    .with_options(CodegenOptions {
      source_map_path: Some(PathBuf::from(&filename)),
      ..Default::default()
    })
    .build(&program);

  ParseResult {
    code: result.code,
    map: result.map.unwrap().to_json_string(),
    ast: program.to_pretty_estree_ts_json(false),
  }
}
