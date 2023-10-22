pub mod config;
pub mod cursor;
pub mod interpreter;
pub mod optimize;
pub mod parse;
pub mod repr;
pub mod translate;

pub fn parse_and_translate(
    raw: &str,
    config: &config::Config,
) -> Result<repr::mlog::MindustryProgram, parse::ParseError> {
    let tokens = parse::tokenize(raw)?;
    let ast = parse::build_ast(&tokens, config)?;
    let translated = translate::translate_ast(&ast, &mut Default::default(), config);

    Ok(translated)
}

pub mod prelude {
    pub use crate::config::Config;
    pub use crate::cursor::Cursor;
    pub use crate::repr::basic::*;
    pub use crate::repr::mlog::*;
    pub use crate::repr::operator::*;
    pub use crate::repr::position::*;
}
