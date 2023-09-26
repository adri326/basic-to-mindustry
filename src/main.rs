use basic_to_mindustry::{
    compile::{optimize_set_use, translate_ast, Namer},
    parse::{build_ast, tokenize},
};

fn main() {
    let path = std::env::args().nth(1).expect("Expected 1 argument");
    let source = std::fs::read_to_string(path).expect("Couldn't read input file");

    let tokens = tokenize(&source).unwrap();
    let parsed = build_ast(&tokens).unwrap();
    let transformed = translate_ast(&parsed, &mut Namer::default());

    println!("{}", transformed);

    let optimized = optimize_set_use(transformed);

    println!("== OPT ==");
    println!("{}", optimized);
}
