use std::{fs, path::PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};

fn bench_compile(b: &mut Criterion) {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("tests/fixtures/ElTable.vue");
  let source = &fs::read_to_string(path).unwrap();

  // b.bench_function("vue-compiler-core", |b| {
  //     b.iter(|| {
  //         parser(source);
  //     })
  // });
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
