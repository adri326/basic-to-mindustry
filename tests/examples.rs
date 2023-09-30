use std::path::Path;

use basic_to_mindustry::common::Config;
use basic_to_mindustry::compile::{optimize_set_use, translate_ast};
use basic_to_mindustry::parse::{build_ast, tokenize};

#[test]
fn test_examples() {
    let config = Config::default();
    for entry in Path::new("./examples/").read_dir().unwrap() {
        let Ok(entry) = entry else { continue };
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

            let tokenized = tokenize(&file).unwrap_or_else(|e| {
                panic!("Error tokenizing {:?}: {:?}", file_name, e);
            });
            let parsed = build_ast(&tokenized, &config).unwrap_or_else(|e| {
                panic!("Error parsing {:?}: {:?}", file_name, e);
            });
            let translated = translate_ast(&parsed, &mut Default::default(), &config);
            let optimized = optimize_set_use(translated);

            let _ = optimized;
        }
    }
}
