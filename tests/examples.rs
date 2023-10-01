#![feature(fs_try_exists)]

use std::path::{Path, PathBuf};

use basic_to_mindustry::common::Config;
use basic_to_mindustry::compile::{optimize_constant, translate_ast, optimize_jump_op, optimize_jump_always};
use basic_to_mindustry::parse::{build_ast, tokenize};

fn read_basic_examples() -> impl Iterator<Item=(String, String)> {
    Path::new("./examples/").read_dir().unwrap().filter_map(|entry| {
        let Ok(entry) = entry else {
            return None;
        };

        if entry
            .file_name()
            .into_string()
            .map(|name| name.ends_with(".basic"))
            .unwrap_or(false)
        {
            let file_name = entry.file_name().into_string().unwrap();
            let file = std::fs::read_to_string(entry.path()).unwrap_or_else(|e| {
                panic!("Error opening {:?}: {:?}", file_name, e);
            });
            Some((file_name, file))
        } else {
            None
        }
    })
}

#[test]
fn test_examples() {
    let config = Config::default();

    for (file_name, file) in read_basic_examples() {
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

    for (file_name, file) in read_basic_examples() {
        let Some(program_name) = PathBuf::from(file_name.clone()).file_stem().and_then(|stem| stem.to_str()).map(|s| s.to_string()) else {
            panic!("Basic program in examples/ has an invalid filename: {}", file_name);
        };

        let opt_0 = format!("tests/examples/{}.0.mlog", program_name);
        if !std::fs::try_exists(&opt_0).unwrap() {
            continue
        }

        let opt_0 = std::fs::read_to_string(opt_0).unwrap_or_else(|e| {
            panic!("Couldn't open tests/examples/{}.0.mlog: {:?}", program_name, e);
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

        let opt_1 = std::fs::read_to_string(format!("tests/examples/{}.1.mlog", program_name)).unwrap_or_else(|e| {
            panic!("Couldn't open tests/examples/{}.1.mlog: {:?}", program_name, e);
        });

        pretty_assertions::assert_eq!(opt_1.trim(), format!("{}", optimized).trim());
    }
}
