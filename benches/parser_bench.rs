use std::{fs, path::PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use oxc_allocator::Allocator;
use oxc_span::SourceType;
use vue_compiler_core::{
  parser::{ParseOption, Parser, WhitespaceStrategy},
  scanner::{ScanOption, Scanner},
};
use vue_oxc_parser::{
  parse,
  parser::{NoopErrorHandler, VueOxcParser},
};

fn bench_compile(b: &mut Criterion) {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let filename = "tests/fixtures/ElTable.vue";
  path.push(filename);
  let source_text = &fs::read_to_string(path).unwrap();

  let mut group = b.benchmark_group("Parser");

  group.bench_function("vue-oxc-parser", |b| {
    b.iter(|| {
      let allocator = Allocator::new();
      let mut vue_oxc_parser = VueOxcParser::new(&allocator, &source_text);
      vue_oxc_parser.parse();
    })
  });

  // group.bench_function("vue-compiler-core", |b| {
  //   b.iter(|| {
  //     let parser = Parser::new(ParseOption {
  //       whitespace: WhitespaceStrategy::Preserve,
  //       ..Default::default()
  //     });
  //     let scanner = Scanner::new(ScanOption::default());
  //     let tokens = scanner.scan(source_text, NoopErrorHandler);
  //     parser.parse(tokens, NoopErrorHandler);
  //   })
  // });

  // let source_text = &parse(source_text.clone(), filename.to_string()).code;
  // group.bench_function("oxc-parser", |b| {
  //   b.iter(|| {
  //     let allocator = Allocator::new();
  //     oxc_parser::Parser::new(&allocator, source_text, SourceType::jsx()).parse();
  //   })
  // });

  group.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
