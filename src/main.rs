use arboard::SetExtLinux;
use basic_to_mindustry::{
    optimize::{optimize_constant, optimize_jump_always, optimize_jump_op},
    parse::{build_ast, tokenize},
    prelude::*,
    translate::{translate_ast, Namer},
};
use clap::Parser;

const DAEMONIZE_ARG: &'static str = "__mbas_daemonize";

#[derive(Clone, Copy, PartialEq)]
enum OptLevel {
    None,
    Conservative,
}

fn main() {
    handle_clipboard_daemon();

    let args = Args::parse();
    let opt_level = match args.opt {
        Some(0) => OptLevel::None,
        Some(1) | None => OptLevel::Conservative,
        _ => OptLevel::Conservative,
    };

    let source = std::fs::read_to_string(&args.input).expect("Couldn't read input file");
    let config = Config::default();

    let tokens = tokenize(&source).unwrap_or_else(|err| {
        err.display_panic(&source);
    });
    let parsed = build_ast(&tokens, &config).unwrap_or_else(|err| {
        err.display_panic(&source);
    });
    let transformed = translate_ast(&parsed, &mut Namer::default(), &config);

    let optimized = if opt_level == OptLevel::Conservative {
        let optimized = optimize_constant(transformed);
        let optimized = optimize_jump_op(optimized);
        optimize_jump_always(optimized)
    } else {
        transformed
    };

    // println!("== OPT == ({} instructions)", optimized.0.len());

    if args.copy {
        copy_result(&optimized);
    } else {
        println!("{}", optimized);
    }
}

#[derive(Parser, Debug)]
#[command(author = "Shad Amethyst", version, about = "A BASIC to Mindustry Logic compiler and optimizer.", long_about = None)]
struct Args {
    #[arg(short, long, help = "If set, copies the results to the clipboard.")]
    copy: bool,

    #[arg(
        short,
        long,
        help = "Optimization level, one of (0, 1)",
        long_help = "Defines the amount of optimizations done on the generated program:\n- A value of 0 means that no optimizations will be done (useful for debugging the compiler itself)\n- A value of 1 means that conservative optimizations will be done (which still allows you to embed the code generated with other pieces of code)"
    )]
    opt: Option<u8>,

    #[arg(index = 1, help = "The path to the input program")]
    input: String,
}

fn handle_clipboard_daemon() {
    #[cfg(target_os = "linux")]
    if std::env::args().nth(1).as_deref() == Some(DAEMONIZE_ARG) {
        let path = std::env::args().nth(2).unwrap();
        let compiled_file = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("Error reading {}: {}", path, e);
        });
        arboard::Clipboard::new()
            .expect("Couldn't open clipboard")
            .set()
            .wait()
            .text(&compiled_file)
            .expect("Couldn't copy file to clipboard");

        // Re-advertise the clipboard contents for a second once a paste has been detected,
        // in cases where the clipboard is read twice.
        arboard::Clipboard::new()
            .expect("Couldn't open clipboard")
            .set_text(compiled_file)
            .expect("Couldn't copy file to clipboard");
        std::thread::sleep(std::time::Duration::from_secs(1));

        std::fs::remove_file(&path).unwrap_or_else(|e| {
            eprintln!("Couldn't remove temporary file {path}: {}", e);
        });

        std::process::exit(0);
    }
}

fn copy_result(program: &MindustryProgram) {
    use rand::Rng;
    use std::env::{current_exe, temp_dir};
    use std::fs::File;
    use std::io::Write;

    if cfg!(target_os = "linux") {
        let file_name = format!(
            "mbas-tmp-output-{}.mlog",
            rand::thread_rng().gen_range(0..1000000)
        );
        let file_name = temp_dir().join(&file_name);
        let mut tmp_file = File::create(&file_name).expect("Couldn't create temporary file");
        write!(tmp_file, "{}", program).expect("Error writing to file");

        std::process::Command::new(
            current_exe().expect("Path to the current executable is not available"),
        )
        .arg(DAEMONIZE_ARG)
        .arg(file_name)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .spawn()
        .expect("Error spawning child clipboard process");
    } else {
        arboard::Clipboard::new()
            .expect("Couldn't open clipboard")
            .set_text(format!("{}", program))
            .expect("Couldn't copy to clipboard");
    }
}
