use std::{fs, path::PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use html_parser::Dom;
use html5ever::{parse_document, tendril::TendrilSink};

fn bench_compile(c: &mut Criterion) {
    let mut b = c.benchmark_group("HTML Parsers");
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/fixtures/ElTable.vue");
    let source = &fs::read_to_string(path).unwrap();

    // b.bench_function("vue-compiler-core", |b| {
    //     b.iter(|| {
    //         parser(source);
    //     })
    // });

    b.bench_function("html_parser", |b| b.iter(|| Dom::parse(source)));

    b.bench_function("html5ever", |b| {
        b.iter(|| {
            parse_document(markup5ever_rcdom::RcDom::default(), Default::default())
                .from_utf8()
                .read_from(&mut source.as_bytes())
                .unwrap()
        })
    });

    b.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
