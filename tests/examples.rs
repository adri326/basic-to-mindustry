#![feature(fs_try_exists)]

use std::path::PathBuf;

#[path = "./common.rs"]
mod common;
use common::*;

use basic_to_mindustry::optimize::{optimize_constant, optimize_jump_always, optimize_jump_op};
use basic_to_mindustry::parse::{build_ast, tokenize};
use basic_to_mindustry::prelude::*;
use basic_to_mindustry::translate::translate_ast;

#[test]
fn test_examples() {
    let config = Config::default();

    for (file_name, file) in read_basic_files("./examples/") {
        let tokenized = tokenize(&file).unwrap_or_else(|e| {
            panic!("Error tokenizing {:?}: {:?}", file_name, e);
        });
        let parsed = build_ast(&tokenized, &config).unwrap_or_else(|e| {
            panic!("Error parsing {:?}: {:?}", file_name, e);
        });
        let translated = translate_ast(&parsed, &mut Default::default(), &config);
        let optimized = optimize_constant(translated);

        let _ = optimized;
    }
}

// TODO: implement proper equality of `MindustryProgram`s and parse the expected results instead
#[test]
fn test_examples_opt() {
    let config = Config::default();

    for (file_name, file) in read_basic_files("./examples/") {
        let Some(program_name) = PathBuf::from(file_name.clone())
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(|s| s.to_string())
        else {
            panic!(
                "Basic program in examples/ has an invalid filename: {}",
                file_name
            );
        };

        let opt_0 = format!("tests/examples/{}.0.mlog", program_name);
        if !std::fs::try_exists(&opt_0).unwrap() {
            continue;
        }

        let opt_0 = std::fs::read_to_string(opt_0).unwrap_or_else(|e| {
            panic!(
                "Couldn't open tests/examples/{}.0.mlog: {:?}",
                program_name, e
            );
        });

        let tokenized = tokenize(&file).unwrap_or_else(|e| {
            panic!("Error tokenizing {:?}: {:?}", file_name, e);
        });
        let parsed = build_ast(&tokenized, &config).unwrap_or_else(|e| {
            panic!("Error parsing {:?}: {:?}", file_name, e);
        });
        let translated = translate_ast(&parsed, &mut Default::default(), &config);

        pretty_assertions::assert_eq!(opt_0.trim(), format!("{}", translated).trim());

        let optimized = optimize_jump_always(optimize_jump_op(optimize_constant(translated)));

        let opt_1 = std::fs::read_to_string(format!("tests/examples/{}.1.mlog", program_name))
            .unwrap_or_else(|e| {
                panic!(
                    "Couldn't open tests/examples/{}.1.mlog: {:?}",
                    program_name, e
                );
            });

        pretty_assertions::assert_eq!(opt_1.trim(), format!("{}", optimized).trim());
    }
}
