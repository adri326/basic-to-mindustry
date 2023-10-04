mod tokenize;
pub use tokenize::*;

mod ast;
pub use ast::*;

use crate::repr::{basic::BasicAstExpression, position::Position};

#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub position: Position,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub(crate) fn missing_token(token: BasicToken, position: Position) -> Self {
        Self {
            kind: ParseErrorKind::MissingToken(token),
            position,
        }
    }

    pub(crate) fn unexpected_token(token: BasicToken, position: Position) -> ParseError {
        Self {
            kind: ParseErrorKind::UnexpectedToken(token),
            position,
        }
    }

    pub fn display_panic(&self, source: &str) -> ! {
        self.position
            .underline(source, &mut std::io::stderr())
            .unwrap();

        eprint!("ERROR ");

        match &self.kind {
            ParseErrorKind::InvalidToken(line_rest) => {
                eprintln!(
                    "Syntax error: '{}' is not a valid token",
                    line_rest
                        .split_ascii_whitespace()
                        .nth(0)
                        .unwrap_or(&line_rest)
                );
            }
            ParseErrorKind::UnexpectedToken(token) => {
                eprintln!("Parse error: this token ({:?}) isn't allowed here", token);
            }
            ParseErrorKind::MissingToken(token) => {
                eprintln!(
                    "Parse error: expected a token ({:?}), but it is missing",
                    token
                );
            }
            ParseErrorKind::InvalidArgumentCount(fn_name, expected, actual) => {
                eprintln!("Invalid argument count: expected {} arguments to the function '{}', got {} arguments", expected, fn_name, actual);
            }
            ParseErrorKind::ExpectedVariable => {
                eprintln!("This parameter expects a variable name");
            }
            ParseErrorKind::ExpectedOperand => {
                eprintln!("Expected an expression operand here (eg. a number, a variable or a subexpression)");
            }
            ParseErrorKind::WrongForVariable(expected, actual) => {
                eprintln!(
                    "Variable of NEXT ('{}') does not match the variable of FOR ('{}')",
                    actual, expected
                );
            }
            ParseErrorKind::InvalidArgument(expr) => {
                eprintln!("{:?} is not a valid argument to this function", expr);
            }
        }

        std::process::exit(1);
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParseErrorKind {
    InvalidToken(String),
    UnexpectedToken(BasicToken),
    MissingToken(BasicToken),
    InvalidArgumentCount(String, usize, usize),
    ExpectedVariable,
    ExpectedOperand,
    WrongForVariable(String, String),
    InvalidArgument(BasicAstExpression),
}
