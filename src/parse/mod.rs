mod tokenize;
pub use tokenize::*;

mod ast;
pub use ast::*;

use crate::repr::basic::BasicAstExpression;

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
    WrongForVariable(String, String),
    InvalidArgument(BasicAstExpression),
}
