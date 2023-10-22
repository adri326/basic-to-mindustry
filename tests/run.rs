#![feature(fs_try_exists)]

#[path = "./common.rs"]
mod common;
use common::*;

use basic_to_mindustry::{
    interpreter::{run, StopCondition, Value},
    optimize::{optimize_constant, optimize_jump_always, optimize_jump_op},
    parse_and_translate,
    prelude::MindustryProgram,
};

/// Verifies that each test file in the `tests/print-success` test suite return `success` in the `@print` buffer.
#[test]
fn test_interprete_success() {
    fn test(program: &MindustryProgram, file_name: &str, opt_name: &str) {
        let result = run(&program, StopCondition::End);

        assert_eq!(
            result.get("@print"),
            Some(&Value::String(String::from("success"))),
            "File {file_name} did not print 'success' in the {opt_name} configuration: {:?}",
            result.get("@print").cloned().unwrap_or_default()
        );
    }

    macro_rules! chain {
        ( $( $optimizer:expr ),* ) => {
            Box::new(|prog| {
                let res = prog;
                $(
                    let res = $optimizer(res);
                )*
                res
            })
        }
    }

    let optimizations: Vec<(
        &'static str,
        Box<dyn Fn(MindustryProgram) -> MindustryProgram>,
    )> = vec![
        ("unoptimized", chain!()),
        ("constant", chain!(optimize_constant)),
        (
            "constant+jump_op",
            chain!(optimize_constant, optimize_jump_op),
        ),
        (
            "level-1",
            chain!(optimize_constant, optimize_jump_op, optimize_jump_always),
        ),
    ];

    for (file_name, file) in read_basic_files("./tests/print-success/") {
        let unoptimized = parse_and_translate(&file, &Default::default()).unwrap();

        for (opt_name, optimizer) in optimizations.iter() {
            let optimized = optimizer(unoptimized.clone());
            test(&optimized, &file_name, opt_name);
        }
    }
}
