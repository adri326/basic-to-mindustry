use super::*;
use crate::cursor::Cursor;

#[derive(Clone, Debug, PartialEq)]
pub enum BasicAstExpression {
    Integer(i64),
    Float(f64),
    Variable(String),
    Binary(Operator, Box<BasicAstExpression>, Box<BasicAstExpression>),
}

macro_rules! impl_op_basic_ast_expression {
    ( $std_op:ty, $fn_name:ident, $self_op:expr ) => {
        impl $std_op for BasicAstExpression {
            type Output = BasicAstExpression;

            fn $fn_name(self, other: Self) -> Self {
                Self::Binary($self_op, Box::new(self), Box::new(other))
            }
        }
    };
}

// These are primarily here for ease of use in testing
impl_op_basic_ast_expression!(std::ops::Add, add, Operator::Add);
impl_op_basic_ast_expression!(std::ops::Sub, sub, Operator::Sub);
impl_op_basic_ast_expression!(std::ops::Mul, mul, Operator::Mul);
impl_op_basic_ast_expression!(std::ops::Div, div, Operator::Div);

#[derive(Clone, Debug, PartialEq)]
pub enum BasicAstOperation {
    Assign(String, BasicAstExpression),
    Jump(String),
    IfThenElse(BasicAstExpression, BasicAstBlock, BasicAstBlock),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicAstInstruction {
    pub label: Option<String>,
    pub operation: BasicAstOperation,
}

impl From<BasicAstOperation> for BasicAstInstruction {
    fn from(operation: BasicAstOperation) -> Self {
        Self {
            label: None,
            operation,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct BasicAstBlock {
    pub instructions: Vec<BasicAstInstruction>,
}

impl BasicAstBlock {
    pub fn new(instructions: impl IntoIterator<Item = BasicAstInstruction>) -> Self {
        Self {
            instructions: instructions.into_iter().collect(),
        }
    }
}

/// Returns the index of the first token matching `needle`
fn find_token_index(tokens: &[BasicToken], needle: BasicToken) -> Result<usize, ParseError> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, t)| **t == needle)
        .map(|(i, _)| i)
        .ok_or(ParseError::MissingToken(needle))
}

pub(crate) fn parse_expression(
    tokens: &mut Cursor<'_, BasicToken>,
) -> Result<BasicAstExpression, ParseError> {
    /// Returns the first non-newline token in `tokens`
    fn peek<'a>(tokens: &'a [BasicToken]) -> Option<&'a BasicToken> {
        tokens.iter().find(|t| !matches!(t, BasicToken::NewLine))
    }

    /// Parses a single expression item
    fn parse_expression_item(
        tokens: &mut Cursor<'_, BasicToken>,
    ) -> Result<BasicAstExpression, ParseError> {
        match tokens.peek(2) {
            [BasicToken::Integer(int), ..] => {
                tokens.take(1);
                Ok(BasicAstExpression::Integer(*int))
            }
            [BasicToken::Float(float), ..] => {
                tokens.take(1);
                Ok(BasicAstExpression::Float(*float))
            }
            [BasicToken::Name(_fn_name), BasicToken::OpenParen, ..] => {
                unimplemented!("Function calls are not yet supported");
            }
            [BasicToken::Name(name), ..] => {
                tokens.take(1);
                Ok(BasicAstExpression::Variable(name.clone()))
            }
            [BasicToken::OpenParen, ..] => {
                tokens.take(1);
                let res = parse_expression(tokens)?;
                if let Some(BasicToken::CloseParen) = tokens.take(1).get(0) {
                    Ok(res)
                } else {
                    Err(ParseError::MissingToken(BasicToken::CloseParen))
                }
            }
            [first, ..] => Err(ParseError::UnexpectedToken(first.clone())),
            [] => Err(ParseError::ExpectedOperand),
        }
    }

    /// Given an lhs and a minimum precedence, eats as many binary operations as possible,
    /// recursively calling itself when an operator with a higher precedence is encountered.
    ///
    /// See https://en.wikipedia.org/wiki/Operator-precedence_parser for more information
    fn parse_expression_main(
        tokens: &mut Cursor<'_, BasicToken>,
        lhs: BasicAstExpression,
        min_precedence: u8,
    ) -> Result<BasicAstExpression, ParseError> {
        let mut ast = lhs;
        while let Some(&BasicToken::Operator(operator)) = peek(tokens) {
            if operator.precedence() < min_precedence {
                break;
            }
            tokens.take(1);
            let mut rhs = parse_expression_item(tokens)?;
            while let Some(&BasicToken::Operator(sub_operator)) = peek(tokens) {
                if sub_operator.precedence() > operator.precedence() {
                    rhs = parse_expression_main(tokens, rhs, operator.precedence() + 1)?;
                } else {
                    break;
                }
            }

            ast = BasicAstExpression::Binary(operator, Box::new(ast), Box::new(rhs));
        }

        Ok(ast)
    }

    // Remove starting newlines
    let lhs = parse_expression_item(tokens)?;
    let res = parse_expression_main(tokens, lhs, 0)?;

    Ok(res)
}

pub fn build_ast(tokens: &[BasicToken]) -> Result<BasicAstBlock, ParseError> {
    let mut tokens = Cursor::from(tokens);
    let mut instructions = Vec::new();
    let mut current_label: Option<String> = None;

    while tokens.len() > 0 {
        match tokens.peek(3) {
            [BasicToken::NewLine, BasicToken::Integer(label), ..] => {
                tokens.take(2);
                current_label = Some(label.to_string());
            }
            [BasicToken::NewLine, BasicToken::Name(label), BasicToken::LabelEnd, ..] => {
                tokens.take(3);
                current_label = Some(label.clone());
            }
            [BasicToken::NewLine, ..] => {
                tokens.take(1);
                current_label = None;
            }
            [BasicToken::Name(variable_name), BasicToken::Assign, ..] => {
                tokens.take(2);
                let expression = parse_expression(&mut tokens)?;
                instructions.push(BasicAstInstruction {
                    label: current_label.take(),
                    operation: BasicAstOperation::Assign(variable_name.clone(), expression),
                });
            }
            [BasicToken::If, ..] => {
                tokens.take(1);
                let then_index = find_token_index(&tokens, BasicToken::Then)?;
                let end_index = find_token_index(&tokens, BasicToken::EndIf)?;

                let condition = parse_expression(&mut tokens.range(0..then_index))?;
                if let Ok(else_index) = find_token_index(&tokens, BasicToken::Else) {
                    let true_branch = build_ast(&tokens[(then_index + 1)..else_index])?;
                    let false_branch = build_ast(&tokens[(else_index + 1)..end_index])?;

                    instructions.push(BasicAstInstruction {
                        label: current_label.take(),
                        operation: BasicAstOperation::IfThenElse(
                            condition,
                            true_branch,
                            false_branch,
                        ),
                    });
                } else {
                    let true_branch = build_ast(&tokens[(then_index + 1)..end_index])?;
                    instructions.push(BasicAstInstruction {
                        label: current_label.take(),
                        operation: BasicAstOperation::IfThenElse(
                            condition,
                            true_branch,
                            BasicAstBlock::default(),
                        ),
                    });
                }

                tokens.take(end_index + 1);
            }
            [BasicToken::Goto, BasicToken::Integer(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction {
                    label: current_label.take(),
                    operation: BasicAstOperation::Jump(label.to_string()),
                });
            }
            [BasicToken::Goto, BasicToken::Name(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction {
                    label: current_label.take(),
                    operation: BasicAstOperation::Jump(label.clone()),
                });
            }
            _ => {
                if cfg!(test) {
                    eprintln!("No match found in main ast loop!");
                    eprintln!("Lookahead: {:?}", tokens.peek(5));
                    eprintln!("Instructions: {:?}", instructions);
                }
                return Err(ParseError::UnexpectedToken(tokens[0].clone()));
            }
        }
    }

    Ok(BasicAstBlock { instructions })
}
