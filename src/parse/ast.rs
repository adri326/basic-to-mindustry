use super::*;
use crate::cursor::Cursor;
use crate::prelude::*;

/// Pops the last context, matches on it, and pushes the result of the match branch to the parent instruction list.
/// Errors can be returned early, and branches need to be exhaustive.
macro_rules! pop_context {
    (
        $context_stack:expr, $instructions:ident,
        { $( $match:pat => $instr:block $(,)? )* }
    ) => {
        match $context_stack.pop() {
            Some(($instructions, context)) => {
                match context {
                    $(
                        $match => {
                            let Some((ref mut parent_instructions, _)) = $context_stack.last_mut()
                            else {
                                unreachable!("{} not wrapped in another context", stringify!($match));
                            };

                            #[allow(unreachable_code)]
                            parent_instructions.push($instr);
                        },
                    )*
                }
            }

            None => {
                unreachable!("Empty context stack");
            },
        }
    }
}

pub fn build_ast(tokens: &[BasicToken], config: &Config) -> Result<BasicAstBlock, ParseError> {
    enum Context {
        Main,
        If(BasicAstExpression),
        IfElse(BasicAstExpression, BasicAstBlock),
        For(
            String,
            BasicAstExpression,
            BasicAstExpression,
            BasicAstExpression,
        ),
        While(BasicAstExpression),
        Do,
        DoWhile(BasicAstExpression),
    }

    let mut tokens = Cursor::from(tokens);
    let mut context_stack: Vec<(Vec<BasicAstInstruction>, Context)> =
        vec![(Vec::new(), Context::Main)];

    while !tokens.is_empty() {
        let Some((ref mut instructions, _context)) = context_stack.last_mut() else {
            unreachable!("Context stack got emptied!");
        };

        match tokens.peek(3) {
            [BasicToken::NewLine, BasicToken::Integer(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction::JumpLabel(label.to_string()));
            }
            [BasicToken::NewLine, BasicToken::Name(label), BasicToken::LabelEnd, ..] => {
                tokens.take(3);
                instructions.push(BasicAstInstruction::JumpLabel(label.clone()));
            }
            [BasicToken::NewLine, ..] => {
                tokens.take(1);
            }
            // == If-then-else ==
            [BasicToken::If, ..] => {
                tokens.take(1);
                let then_index = find_token_index(&tokens, BasicToken::Then)?;

                let condition = parse_expression(&mut tokens.range(0..then_index))?;

                tokens.take(then_index + 1);

                context_stack.push((Vec::new(), Context::If(condition)));
            }
            [BasicToken::Else, ..] => {
                tokens.take(1);

                match context_stack.pop().unwrap() {
                    (instructions, Context::If(condition)) => {
                        context_stack.push((
                            Vec::new(),
                            Context::IfElse(condition, BasicAstBlock { instructions }),
                        ));
                    }
                    (_instructions, _) => {
                        return Err(ParseError::UnexpectedToken(BasicToken::Else));
                    }
                }
            }
            [BasicToken::EndIf, ..] => {
                tokens.take(1);

                pop_context!(context_stack, instructions, {
                    Context::If(condition) => {
                        BasicAstInstruction::IfThenElse(
                            condition,
                            BasicAstBlock { instructions },
                            BasicAstBlock::default(),
                        )
                    },
                    Context::IfElse(condition, true_branch) => {
                        BasicAstInstruction::IfThenElse(
                            condition,
                            true_branch,
                            BasicAstBlock { instructions },
                        )
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(BasicToken::EndIf));
                    }
                });
            }
            // == For loops ==
            [BasicToken::For, BasicToken::Name(variable), BasicToken::Assign, ..] => {
                tokens.take(3);

                let start = parse_expression(&mut tokens)?;

                expect_next_token(&mut tokens, &BasicToken::To)?;
                tokens.take(1);

                let end = parse_expression(&mut tokens)?;

                let step = if let Some(BasicToken::Step) = tokens.get(0) {
                    tokens.take(1);

                    parse_expression(&mut tokens)?
                } else {
                    BasicAstExpression::Integer(1)
                };

                expect_next_token(&mut tokens, &BasicToken::NewLine)?;

                context_stack.push((Vec::new(), Context::For(variable.clone(), start, end, step)));
            }
            [BasicToken::Next, BasicToken::Name(variable), ..] => {
                tokens.take(2);

                pop_context!(context_stack, instructions, {
                    Context::For(expected_variable, start, end, step) => {
                        if *variable != expected_variable {
                            return Err(ParseError::WrongForVariable(
                                expected_variable,
                                variable.clone(),
                            ));
                        }

                        BasicAstInstruction::For {
                            variable: expected_variable,
                            start,
                            end,
                            step,
                            instructions: BasicAstBlock::new(instructions),
                        }
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(BasicToken::Next));
                    }
                });
            }
            // == While loops ==
            [BasicToken::While, ..] => {
                tokens.take(1);
                let condition = parse_expression(&mut tokens)?;
                expect_next_token(&tokens, &BasicToken::NewLine)?;

                context_stack.push((Vec::new(), Context::While(condition)));
            }
            [BasicToken::Do, BasicToken::While, ..] => {
                tokens.take(2);
                let condition = parse_expression(&mut tokens)?;
                expect_next_token(&tokens, &BasicToken::NewLine)?;

                context_stack.push((Vec::new(), Context::DoWhile(condition)));
            }
            [BasicToken::Do, ..] => {
                tokens.take(1);
                context_stack.push((Vec::new(), Context::Do));
                expect_next_token(&tokens, &BasicToken::NewLine)?;
            }

            [BasicToken::Wend, ..] => {
                tokens.take(1);

                pop_context!(context_stack, instructions, {
                    Context::While(condition) => {
                        BasicAstInstruction::While(condition, BasicAstBlock::new(instructions))
                    },
                    _ => {
                        return Err(ParseError::UnexpectedToken(BasicToken::Wend));
                    }
                });
            }
            [BasicToken::Loop, BasicToken::While, ..] => {
                tokens.take(2);

                let condition = parse_expression(&mut tokens)?;

                pop_context!(context_stack, instructions, {
                    Context::Do => {
                        BasicAstInstruction::DoWhile(condition, BasicAstBlock::new(instructions))
                    },
                    Context::DoWhile(_) => {
                        return Err(ParseError::UnexpectedToken(BasicToken::While));
                    },
                    _ => {
                        return Err(ParseError::UnexpectedToken(BasicToken::Loop));
                    }
                });
            }
            [BasicToken::Loop, ..] => {
                tokens.take(1);

                pop_context!(context_stack, instructions, {
                    Context::DoWhile(condition) => {
                        BasicAstInstruction::DoWhile(condition, BasicAstBlock::new(instructions))
                    },
                    Context::Do => {
                        return Err(ParseError::MissingToken(BasicToken::While));
                    },
                    _ => {
                        return Err(ParseError::UnexpectedToken(BasicToken::Wend));
                    }
                });
            }

            // == Goto and End ==
            [BasicToken::Goto, BasicToken::Integer(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction::Jump(label.to_string()));
            }
            [BasicToken::Goto, BasicToken::Name(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction::Jump(label.clone()));
            }
            [BasicToken::End, ..] => {
                tokens.take(1);
                instructions.push(BasicAstInstruction::End);
            }
            // == Misc ==
            [BasicToken::Name(variable_name), BasicToken::Assign, ..] => {
                tokens.take(2);
                let expression = parse_expression(&mut tokens)?;
                instructions.push(BasicAstInstruction::Assign(
                    variable_name.clone(),
                    expression,
                ));
            }
            [BasicToken::Print, ..] => {
                tokens.take(1);

                let mut expressions = Vec::new();

                if let Some(BasicToken::NewLine) = tokens.get(0) {
                    instructions.push(BasicAstInstruction::Print(expressions));
                } else {
                    expressions.push((parse_expression(&mut tokens)?, false));

                    while let Some(BasicToken::Comma) = tokens.get(0) {
                        tokens.take(1);
                        expressions.push((parse_expression(&mut tokens)?, false));
                    }

                    instructions.push(BasicAstInstruction::Print(expressions));
                }

                match tokens.get(0) {
                    Some(BasicToken::NewLine) | None => {}
                    Some(other) => {
                        return Err(ParseError::UnexpectedToken(other.clone()));
                    }
                }
            }
            [BasicToken::Name(fn_name), BasicToken::OpenParen, ..] => {
                tokens.take(2);
                let mut arguments = Vec::new();

                while tokens.get(0) != Some(&BasicToken::CloseParen) {
                    arguments.push(parse_expression(&mut tokens)?);

                    match tokens.get(0) {
                        Some(BasicToken::Comma) => {
                            tokens.take(1);
                        }
                        Some(BasicToken::CloseParen) => break,
                        _ => return Err(ParseError::MissingToken(BasicToken::Comma)),
                    }
                }

                match tokens.take(1) {
                    [BasicToken::CloseParen] => {}
                    [other] => {
                        return Err(ParseError::UnexpectedToken(other.clone()));
                    }
                    _ => {
                        return Err(ParseError::MissingToken(BasicToken::CloseParen));
                    }
                }

                let lowercase_fn_name = fn_name.to_lowercase();

                if let Some(translation_fn) = config.special_functions.get(&lowercase_fn_name) {
                    instructions.push(translation_fn(arguments)?);
                } else if let Some((_, mutating, n_args)) =
                    config.builtin_functions.get(&lowercase_fn_name)
                {
                    if *mutating {
                        let BasicAstExpression::Variable(_) = &arguments[0] else {
                            return Err(ParseError::ExpectedVariable);
                        };
                    }

                    if arguments.len() != *n_args {
                        return Err(ParseError::InvalidArgumentCount(
                            lowercase_fn_name,
                            *n_args,
                            arguments.len(),
                        ));
                    }

                    instructions.push(BasicAstInstruction::CallBuiltin(
                        lowercase_fn_name,
                        arguments,
                    ));
                } else {
                    unimplemented!("No function named {} found!", fn_name);
                }
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

    if context_stack.is_empty() {
        unreachable!("Empty context stack");
    } else if context_stack.len() > 1 {
        match &context_stack.last().unwrap().1 {
            Context::If(_) | Context::IfElse(_, _) => {
                return Err(ParseError::MissingToken(BasicToken::EndIf));
            }
            Context::For(_, _, _, _) => {
                return Err(ParseError::MissingToken(BasicToken::Next));
            }
            Context::While(_) => {
                return Err(ParseError::MissingToken(BasicToken::Wend));
            }
            Context::Do | Context::DoWhile(_) => {
                return Err(ParseError::MissingToken(BasicToken::Loop));
            }
            Context::Main => {
                unreachable!("There cannot be another context below the main context");
            }
        }
    }

    let (instructions, Context::Main) = context_stack.into_iter().next().unwrap() else {
        unreachable!("The lowermost context must be the main context");
    };

    Ok(BasicAstBlock { instructions })
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

pub(crate) fn parse_expression(
    tokens: &mut Cursor<'_, BasicToken>,
) -> Result<BasicAstExpression, ParseError> {
    /// Returns the first non-newline token in `tokens`
    fn peek(tokens: &[BasicToken]) -> Option<&BasicToken> {
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
            [BasicToken::Name(fn_name), BasicToken::OpenParen, ..] => {
                tokens.take(2);
                let fn_name_lowercase = fn_name.to_ascii_lowercase();
                let mut arguments = Vec::new();
                while tokens.get(0) != Some(&BasicToken::CloseParen) {
                    arguments.push(parse_expression(tokens)?);

                    match tokens.get(0) {
                        Some(BasicToken::Comma) => {
                            tokens.take(1);
                        }
                        Some(BasicToken::CloseParen) => break,
                        _ => return Err(ParseError::MissingToken(BasicToken::Comma)),
                    }
                }

                expect_next_token(tokens, &BasicToken::CloseParen)?;
                tokens.take(1);

                if let Ok(unary_operator) = UnaryOperator::try_from(fn_name_lowercase.as_str()) {
                    if arguments.len() != 1 {
                        Err(ParseError::InvalidArgumentCount(
                            fn_name_lowercase,
                            1,
                            arguments.len(),
                        ))
                    } else {
                        Ok(BasicAstExpression::Unary(
                            unary_operator,
                            Box::new(arguments.into_iter().next().unwrap()),
                        ))
                    }
                } else if let Some(binary_operator) =
                    Operator::from_fn_name(fn_name_lowercase.as_str())
                {
                    if arguments.len() != 2 {
                        Err(ParseError::InvalidArgumentCount(
                            fn_name_lowercase,
                            2,
                            arguments.len(),
                        ))
                    } else {
                        let mut iter = arguments.into_iter();
                        Ok(BasicAstExpression::Binary(
                            binary_operator,
                            Box::new(iter.next().unwrap()),
                            Box::new(iter.next().unwrap()),
                        ))
                    }
                } else {
                    unimplemented!(
                        "User function calls are not yet supported! Function: {:?}",
                        fn_name
                    );
                }
            }
            [BasicToken::Name(name), ..] => {
                tokens.take(1);
                Ok(BasicAstExpression::Variable(name.clone()))
            }
            [BasicToken::String(string), ..] => {
                tokens.take(1);
                Ok(BasicAstExpression::String(string.clone()))
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

fn expect_next_token(
    tokens: &Cursor<'_, BasicToken>,
    expected: &BasicToken,
) -> Result<(), ParseError> {
    match tokens.get(0) {
        Some(token) if token == expected => Ok(()),
        Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
        None => Err(ParseError::MissingToken(expected.clone())),
    }
}
