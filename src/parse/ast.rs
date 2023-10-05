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

pub fn build_ast(
    tokens: &[(BasicToken, Position)],
    config: &Config,
) -> Result<BasicAstBlock, ParseError> {
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

        let Some(position) = tokens.get(0).map(|pair| pair.1) else {
            break;
        };

        match &drop_position(tokens.peek(3))[..] {
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

                let condition = parse_expression(&mut tokens.range(0..then_index), config)?;

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
                        return Err(ParseError::unexpected_token(BasicToken::Else, position));
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
                        return Err(ParseError::unexpected_token(BasicToken::EndIf, position));
                    }
                });
            }
            // == For loops ==
            [BasicToken::For, BasicToken::Name(variable), BasicToken::Assign, ..] => {
                tokens.take(3);

                let start = parse_expression(&mut tokens, config)?;

                expect_next_token(&mut tokens, &BasicToken::To)?;
                tokens.take(1);

                let end = parse_expression(&mut tokens, config)?;

                let step = if let Some((BasicToken::Step, _pos)) = tokens.get(0) {
                    tokens.take(1);

                    parse_expression(&mut tokens, config)?
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
                            return Err(ParseError::new(
                                ParseErrorKind::WrongForVariable(
                                    expected_variable,
                                    variable.clone(),
                                ),
                                last_position(&tokens),
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
                        return Err(ParseError::unexpected_token(BasicToken::Next, position));
                    }
                });
            }
            // == While loops ==
            [BasicToken::While, ..] => {
                tokens.take(1);
                let condition = parse_expression(&mut tokens, config)?;
                expect_next_token(&tokens, &BasicToken::NewLine)?;

                context_stack.push((Vec::new(), Context::While(condition)));
            }
            [BasicToken::Do, BasicToken::While, ..] => {
                tokens.take(2);
                let condition = parse_expression(&mut tokens, config)?;
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
                        return Err(ParseError::unexpected_token(BasicToken::Wend, position));
                    }
                });
            }
            [BasicToken::Loop, BasicToken::While, ..] => {
                tokens.take(2);

                let condition = parse_expression(&mut tokens, config)?;

                pop_context!(context_stack, instructions, {
                    Context::Do => {
                        BasicAstInstruction::DoWhile(condition, BasicAstBlock::new(instructions))
                    },
                    Context::DoWhile(_) => {
                        return Err(ParseError::unexpected_token(BasicToken::While, last_position(&tokens)));
                    },
                    _ => {
                        return Err(ParseError::unexpected_token(BasicToken::Loop, position));
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
                        return Err(ParseError::missing_token(BasicToken::While, position.at_end()));
                    },
                    _ => {
                        return Err(ParseError::unexpected_token(BasicToken::Wend, position));
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
            [BasicToken::GoSub, BasicToken::Integer(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction::GoSub(label.to_string()));
            }
            [BasicToken::GoSub, BasicToken::Name(label), ..] => {
                tokens.take(2);
                instructions.push(BasicAstInstruction::GoSub(label.clone()));
            }
            [BasicToken::Return, ..] => {
                tokens.take(1);
                instructions.push(BasicAstInstruction::Return);
            }
            // == Misc ==
            [BasicToken::Name(variable_name), BasicToken::Assign, ..] => {
                tokens.take(2);
                let expression = parse_expression(&mut tokens, config)?;
                instructions.push(BasicAstInstruction::Assign(
                    variable_name.clone(),
                    expression,
                ));
            }
            [BasicToken::Print, ..] => {
                tokens.take(1);

                let mut expressions = Vec::new();

                if let Some((BasicToken::NewLine, _)) = tokens.get(0) {
                    instructions.push(BasicAstInstruction::Print(expressions));
                } else {
                    expressions.push((parse_expression(&mut tokens, config)?, false));

                    while let Some((BasicToken::Comma, _)) = tokens.get(0) {
                        tokens.take(1);
                        expressions.push((parse_expression(&mut tokens, config)?, false));
                    }

                    instructions.push(BasicAstInstruction::Print(expressions));
                }

                match tokens.get(0) {
                    Some((BasicToken::NewLine, _)) | None => {}
                    Some((other, position)) => {
                        return Err(ParseError::unexpected_token(other.clone(), *position));
                    }
                }
            }
            [BasicToken::Name(fn_name), BasicToken::OpenParen, ..] => {
                tokens.take(2);
                let mut arguments = Vec::new();

                while tokens.get(0).map(|pair| &pair.0) != Some(&BasicToken::CloseParen) {
                    arguments.push(parse_expression(&mut tokens, config)?);

                    match tokens.get(0) {
                        Some((BasicToken::Comma, _)) => {
                            tokens.take(1);
                        }
                        Some((BasicToken::CloseParen, _)) => break,
                        _ => {
                            return Err(ParseError::missing_token(
                                BasicToken::Comma,
                                last_position(&tokens),
                            ))
                        }
                    }
                }

                expect_next_token(&tokens, &BasicToken::CloseParen)?;
                tokens.take(1);
                let span = position.until(last_position(&tokens));

                let lowercase_fn_name = fn_name.to_lowercase();

                if let Some(translation_fn) = config.special_routines.get(&lowercase_fn_name) {
                    instructions.push(translation_fn(arguments, span)?);
                } else if let Some((_, mutating, n_args)) =
                    config.builtin_routines.get(&lowercase_fn_name)
                {
                    if *mutating {
                        let BasicAstExpression::Variable(_) = &arguments[0] else {
                            return Err(ParseError::new(
                                ParseErrorKind::ExpectedVariable,
                                last_position(&tokens),
                            ));
                        };
                    }

                    if arguments.len() != *n_args {
                        return Err(ParseError::new(
                            ParseErrorKind::InvalidArgumentCount(
                                lowercase_fn_name,
                                *n_args,
                                arguments.len(),
                            ),
                            span,
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
                return Err(ParseError::unexpected_token(tokens[0].clone().0, position));
            }
        }
    }

    if context_stack.is_empty() {
        unreachable!("Empty context stack");
    } else if context_stack.len() > 1 {
        let position = last_position(&tokens).at_end();

        match &context_stack.last().unwrap().1 {
            Context::If(_) | Context::IfElse(_, _) => {
                return Err(ParseError::missing_token(BasicToken::EndIf, position));
            }
            Context::For(_, _, _, _) => {
                return Err(ParseError::missing_token(BasicToken::Next, position));
            }
            Context::While(_) => {
                return Err(ParseError::missing_token(BasicToken::Wend, position));
            }
            Context::Do | Context::DoWhile(_) => {
                return Err(ParseError::missing_token(BasicToken::Loop, position));
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
fn find_token_index(
    tokens: &[(BasicToken, Position)],
    needle: BasicToken,
) -> Result<usize, ParseError> {
    tokens
        .iter()
        .enumerate()
        .find(|(_index, (t, _pos))| *t == needle)
        .map(|(i, _)| i)
        .ok_or(ParseError::missing_token(
            needle,
            tokens.last().map(|pair| pair.1).unwrap_or_default(),
        ))
}

macro_rules! impl_op_basic_ast_expression {
    ( $std_op:ty, $fn_name:ident, $self_op:expr ) => {
        impl $std_op for BasicAstExpression {
            type Output = BasicAstExpression;

            fn $fn_name(self, other: Self) -> Self {
                Self::Binary($self_op.into(), Box::new(self), Box::new(other))
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
    tokens: &mut Cursor<'_, (BasicToken, Position)>,
    config: &Config,
) -> Result<BasicAstExpression, ParseError> {
    /// Returns the first non-newline token in `tokens`
    fn peek(tokens: &[(BasicToken, Position)]) -> Option<&BasicToken> {
        tokens
            .iter()
            .find(|t| !matches!(t.0, BasicToken::NewLine))
            .map(|pair| &pair.0)
    }

    /// Parses a single expression item
    fn parse_expression_item(
        tokens: &mut Cursor<'_, (BasicToken, Position)>,
        config: &Config,
    ) -> Result<BasicAstExpression, ParseError> {
        let position = tokens.get(0).map(|pair| pair.1).unwrap_or_default();

        match &drop_position(tokens.peek(2))[..] {
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
                while tokens.get(0).map(|pair| &pair.0) != Some(&BasicToken::CloseParen) {
                    arguments.push(parse_expression(tokens, config)?);

                    match tokens.get(0).map(|pair| &pair.0) {
                        Some(BasicToken::Comma) => {
                            tokens.take(1);
                        }
                        Some(BasicToken::CloseParen) => break,
                        _ => return Err(ParseError::missing_token(BasicToken::Comma, position)),
                    }
                }

                expect_next_token(tokens, &BasicToken::CloseParen)?;
                tokens.take(1);
                let span = position.until(last_position(tokens));

                if let Ok(unary_operator) = UnaryOperator::try_from(fn_name_lowercase.as_str()) {
                    if arguments.len() != 1 {
                        Err(ParseError::new(
                            ParseErrorKind::InvalidArgumentCount(
                                fn_name_lowercase,
                                1,
                                arguments.len(),
                            ),
                            span,
                        ))
                    } else {
                        Ok(BasicAstExpression::Unary(
                            unary_operator,
                            Box::new(arguments.into_iter().next().unwrap()),
                        ))
                    }
                } else if let Some(binary_operator) =
                    BasicOperator::from_fn_name(fn_name_lowercase.as_str())
                {
                    if arguments.len() != 2 {
                        Err(ParseError::new(
                            ParseErrorKind::InvalidArgumentCount(
                                fn_name_lowercase,
                                2,
                                arguments.len(),
                            ),
                            span,
                        ))
                    } else {
                        let mut iter = arguments.into_iter();
                        Ok(BasicAstExpression::Binary(
                            binary_operator,
                            Box::new(iter.next().unwrap()),
                            Box::new(iter.next().unwrap()),
                        ))
                    }
                } else if let Some(function_config) =
                    config.builtin_functions.get(&fn_name_lowercase)
                {
                    function_config.validate_args(&arguments, span)?;
                    Ok(BasicAstExpression::BuiltinFunction(
                        fn_name_lowercase,
                        arguments,
                    ))
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
                let res = parse_expression(tokens, config)?;
                if let Some((BasicToken::CloseParen, _)) = tokens.take(1).get(0) {
                    Ok(res)
                } else {
                    Err(ParseError::missing_token(
                        BasicToken::CloseParen,
                        position.at_end(),
                    ))
                }
            }
            [first, ..] => Err(ParseError::unexpected_token((**first).clone(), position)),
            [] => Err(ParseError::new(
                ParseErrorKind::ExpectedOperand,
                last_position(&tokens).at_end(),
            )),
        }
    }

    /// Given an lhs and a minimum precedence, eats as many binary operations as possible,
    /// recursively calling itself when an operator with a higher precedence is encountered.
    ///
    /// See https://en.wikipedia.org/wiki/Operator-precedence_parser for more information
    fn parse_expression_main(
        tokens: &mut Cursor<'_, (BasicToken, Position)>,
        lhs: BasicAstExpression,
        min_precedence: u8,
        config: &Config,
    ) -> Result<BasicAstExpression, ParseError> {
        let mut ast = lhs;
        while let Some(&BasicToken::Operator(operator)) = peek(tokens) {
            if operator.precedence() < min_precedence {
                break;
            }
            tokens.take(1);
            let mut rhs = parse_expression_item(tokens, config)?;
            while let Some(&BasicToken::Operator(sub_operator)) = peek(tokens) {
                if sub_operator.precedence() > operator.precedence() {
                    rhs = parse_expression_main(tokens, rhs, operator.precedence() + 1, config)?;
                } else {
                    break;
                }
            }

            ast = BasicAstExpression::Binary(operator, Box::new(ast), Box::new(rhs));
        }

        Ok(ast)
    }

    // Remove starting newlines
    let lhs = parse_expression_item(tokens, config)?;
    let res = parse_expression_main(tokens, lhs, 0, config)?;

    Ok(res)
}

fn expect_next_token(
    tokens: &Cursor<'_, (BasicToken, Position)>,
    expected: &BasicToken,
) -> Result<(), ParseError> {
    match tokens.get(0) {
        Some((token, _position)) if token == expected => Ok(()),
        Some((token, position)) => Err(ParseError::new(
            ParseErrorKind::UnexpectedToken(token.clone()),
            *position,
        )),
        None => Err(ParseError::missing_token(
            expected.clone(),
            tokens.last().unwrap().1,
        )),
    }
}
