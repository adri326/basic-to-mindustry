use basic_to_mindustry::{
    common::Config,
    compile::{optimize_constant, optimize_jump_always, optimize_jump_op, translate_ast, Namer},
    parse::{build_ast, tokenize},
};

fn main() {
    let path = std::env::args().nth(1).expect("Expected 1 argument");
    let source = std::fs::read_to_string(path).expect("Couldn't read input file");
    let config = Config::default();

    let tokens = tokenize(&source).unwrap();
    let parsed = build_ast(&tokens, &config).unwrap();
    let transformed = translate_ast(&parsed, &mut Namer::default(), &config);

    println!("{}", transformed);

    let optimized = optimize_constant(transformed);
    let optimized = optimize_jump_op(optimized);
    let optimized = optimize_jump_always(optimized);

    // println!("== OPT == ({} instructions)", optimized.0.len());
    println!("{}", optimized);
}
