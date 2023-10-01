mod tokenize;
pub use tokenize::*;

mod ast;
pub use ast::*;

#[cfg(test)]
mod test;

#[derive(PartialEq, Clone, Debug)]
pub enum ParseError {
    InvalidToken(String),
    UnexpectedToken(BasicToken),
    MissingToken(BasicToken),
    InvalidArgumentCount(String, usize, usize),
    ExpectedVariable,
    ExpectedOperand,
}
