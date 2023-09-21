use regex::Regex;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    RShift,
    LShift,
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
    // etc.
}

impl Operator {
    fn precedence(self) -> u8 {
        use Operator as O;
        match self {
            O::Add | O::Sub => 5,
            O::RShift | O::LShift => 4,
            O::Mod => 3,
            O::Mul | O::Div => 0,
            O::Eq | O::Neq | O::Gt | O::Lt | O::Gte | O::Lte => 10,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum BasicToken {
    NewLine,
    Assign,
    If,
    Then,
    Else,
    EndIf,
    Goto,
    OpenParen,
    CloseParen,
    Integer(i64),
    Float(f64),
    Name(String),
    String(String),
    Operator(Operator),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParseError {
    InvalidToken(String),
    UnexpectedToken(BasicToken),
    MissingToken(BasicToken),
    ExpectedOperand,
}

pub fn tokenize(raw: &str) -> Result<Vec<BasicToken>, ParseError> {
    macro_rules! match_token {
        ( $line:expr, $res:expr $(;)? ) => {};
        (
            $line:expr, $res:expr;
            $matcher:ident => (),
            $(
                $rest_matcher:ident $(($rest_match_name:ident))? => $rest_value:tt,
            )*
        ) => {
            if let Some(matched) = $matcher.find($line) {
                $line = &$line[matched.end()..];
                continue
            }
            match_token!(
                $line, $res;
                $(
                    $rest_matcher $(($rest_match_name))? => $rest_value,
                )*
            );
        };
        (
            $line:expr, $res:expr;
            $matcher:ident $(($match_name:ident))? => $value:expr,
            $(
                $rest_matcher:ident $(($rest_match_name:ident))? => $rest_value:tt,
            )*
        ) => {
            if let Some(matched) = $matcher.find($line) {
                $line = &$line[matched.end()..];
                $(let $match_name = matched.as_str();)?
                $res.push($value);
                continue
            }
            match_token!(
                $line, $res;
                $(
                    $rest_matcher $(($rest_match_name))? => $rest_value,
                )*
            );
        }
    }

    let mut res = Vec::new();
    let match_let = Regex::new(r"(?i)^let").unwrap();
    let match_jump = Regex::new(r"(?i)^go\s*to").unwrap();
    let match_word = Regex::new(r"(?i)^(?:if|then|else|end\s?if)").unwrap();
    let match_space = Regex::new(r"^\s+").unwrap();
    let match_variable = Regex::new(r"^@?[a-zA-Z_][a-zA-Z_0-9]*").unwrap();
    let match_float = Regex::new(r"^[0-9]*\.[0-9]+").unwrap();
    let match_integer = Regex::new(r"^[0-9]+").unwrap();
    let match_assign = Regex::new(r"^=").unwrap();
    let match_operator = Regex::new(r"^(?:[+\-*/%]|[<>]=?|[!=]=|<<|>>)").unwrap();
    let match_paren = Regex::new(r"^(?:\(|\))").unwrap();
    // TODO: handle escapes
    let match_string = Regex::new(r#""[^"]*""#).unwrap();
    let match_comment = Regex::new(r"(?i)^rem\s.*$").unwrap();
    // TODO: handle labels

    for mut line in raw.lines() {
        if line.len() > 0 {
            res.push(BasicToken::NewLine);
        }
        while line.len() > 0 {
            match_token!(line, res;
                match_space => (),
                match_let => (),
                match_comment => (),
                match_jump => (BasicToken::Goto),
                match_word(word) => (match word.to_lowercase().as_str() {
                    "if" => BasicToken::If,
                    "then" => BasicToken::Then,
                    "else" => BasicToken::Else,
                    "end if" | "endif" => BasicToken::EndIf,
                    _ => unreachable!(),
                }),
                match_variable(name) => (BasicToken::Name(name.to_string())),
                match_float(float) => (BasicToken::Float(float.parse().unwrap())),
                match_integer(int) => (BasicToken::Integer(int.parse().unwrap())),
                match_assign => (BasicToken::Assign),
                match_operator(op) => (BasicToken::Operator(match op {
                    "+" => Operator::Add,
                    "-" => Operator::Sub,
                    "*" => Operator::Mul,
                    "/" => Operator::Div,
                    "%" => Operator::Mod,
                    "<" => Operator::Lt,
                    "<=" => Operator::Lte,
                    ">" => Operator::Gt,
                    ">=" => Operator::Gte,
                    "<<" => Operator::LShift,
                    ">>" => Operator::RShift,
                    _ => unreachable!(),
                })),
                match_paren(paren) => (if paren == "(" {
                    BasicToken::OpenParen
                } else {
                    BasicToken::CloseParen
                }),
                match_string(with_quotes) => (BasicToken::String(with_quotes[1..with_quotes.len() - 1].to_string())),
            );
            // If this line is reached, then none of the matches above matched
            return Err(ParseError::InvalidToken(line.to_string()));
        }
    }

    Ok(res)
}

pub enum BasicAstExpression {
    Integer(i64),
    Float(f64),
    Variable(String),
    Binary(Operator, Box<BasicAstExpression>, Box<BasicAstExpression>),
}

pub enum BasicAstOperation {
    Assign(String, BasicAstExpression),
    Jump(String),
    IfThenElse(BasicAstExpression, BasicAstBlock, BasicAstBlock),
}

pub struct BasicAstInstruction {
    pub label: Option<String>,
    pub operation: BasicAstOperation,
}

#[derive(Default)]
pub struct BasicAstBlock {
    pub instructions: Vec<BasicAstInstruction>,
}

fn find_token_index(tokens: &[BasicToken], needle: BasicToken) -> Result<usize, ParseError> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, t)| **t == needle)
        .map(|(i, _)| i)
        .ok_or(ParseError::MissingToken(needle))
}

fn parse_expression(mut tokens: &[BasicToken]) -> Result<BasicAstExpression, ParseError> {
    /// Advances `tokens` by `by` tokens, skipping the first newline tokens if present
    fn advance(tokens: &mut &[BasicToken], by: usize) {
        while let Some(BasicToken::NewLine) = tokens.get(0) {
            *tokens = &(*tokens)[1..];
        }
        *tokens = &(*tokens)[by..];
    }

    /// Returns the first non-newline token in `tokens`
    fn peek<'a>(tokens: &'a &[BasicToken]) -> Option<&'a BasicToken> {
        tokens.iter().find(|t| !matches!(t, BasicToken::NewLine))
    }

    /// Parses a single expression item
    fn parse_expression_item(tokens: &mut &[BasicToken]) -> Result<BasicAstExpression, ParseError> {
        match *tokens {
            [BasicToken::Integer(int), ..] => {
                advance(tokens, 1);
                Ok(BasicAstExpression::Integer(*int))
            },
            [BasicToken::Float(float), ..] => {
                advance(tokens, 1);
                Ok(BasicAstExpression::Float(*float))
            },
            [BasicToken::Name(_fn_name), BasicToken::OpenParen, ..] => {
                unimplemented!("Function calls are not yet supported");
            },
            [BasicToken::Name(name), ..] => {
                advance(tokens, 1);
                Ok(BasicAstExpression::Variable(name.clone()))
            },
            [] => Err(ParseError::ExpectedOperand),
            _ => Err(ParseError::UnexpectedToken(tokens[0].clone())),
        }
    }

    /// Given an lhs and a minimum precedence, eats as many binary operations as possible,
    /// recursively calling itself when an operator with a higher precedence is encountered.
    ///
    /// See https://en.wikipedia.org/wiki/Operator-precedence_parser for more information
    fn parse_expression_main(tokens: &mut &[BasicToken], lhs: BasicAstExpression, min_precedence: u8) -> Result<BasicAstExpression, ParseError> {
        let mut ast = lhs;
        while let Some(&BasicToken::Operator(operator)) = peek(tokens) {
            if operator.precedence() < min_precedence {
                break
            }
            advance(tokens, 1);
            let mut rhs = parse_expression_item(tokens)?;
            while let Some(&BasicToken::Operator(sub_operator)) = peek(tokens) {
                if sub_operator.precedence() > operator.precedence() {
                    rhs = parse_expression_main(tokens, rhs, operator.precedence() + 1)?;
                }
            }

            ast = BasicAstExpression::Binary(operator, Box::new(ast), Box::new(rhs));
        }

        Ok(ast)
    }

    // Remove starting newlines
    let lhs = parse_expression_item(&mut tokens)?;
    advance(&mut tokens, 1);
    let res = parse_expression_main(&mut tokens, lhs, 0)?;

    assert_eq!(tokens, []);

    Ok(res)
}

pub fn build_ast(mut tokens: &[BasicToken]) -> Result<BasicAstBlock, ParseError> {
    let mut instructions = Vec::new();
    let mut current_label: Option<String> = None;

    while tokens.len() > 0 {
        match &tokens[..] {
            [BasicToken::NewLine, BasicToken::Integer(label), ..] => {
                tokens = &tokens[2..];
                current_label = Some(label.to_string());
            }
            [BasicToken::NewLine, BasicToken::Name(label), ..] => {
                tokens = &tokens[2..];
                current_label = Some(label.clone());
            }
            [BasicToken::NewLine, ..] => {
                tokens = &tokens[1..];
                current_label = None;
            }
            [BasicToken::Name(variable_name), BasicToken::Assign, ..] => {
                tokens = &tokens[2..];
                let expression = parse_expression(tokens)?;
                // TODO: advance `tokens`
                instructions.push(BasicAstInstruction {
                    label: current_label.take(),
                    operation: BasicAstOperation::Assign(variable_name.clone(), expression)
                });
            }
            [BasicToken::If, ..] => {
                tokens = &tokens[1..];
                let then_index = find_token_index(tokens, BasicToken::Then)?;
                let end_index = find_token_index(tokens, BasicToken::EndIf)?;

                let condition = parse_expression(&tokens[0..then_index])?;
                if let Ok(else_index) = find_token_index(tokens, BasicToken::Else) {
                    let true_branch = build_ast(&tokens[(then_index + 1)..else_index])?;
                    let false_branch = build_ast(&tokens[(else_index + 1)..end_index])?;

                    instructions.push(BasicAstInstruction {
                        label: current_label.take(),
                        operation: BasicAstOperation::IfThenElse(condition, true_branch, false_branch)
                    });
                } else {
                    let true_branch = build_ast(&tokens[(then_index + 1)..end_index])?;
                    instructions.push(BasicAstInstruction {
                        label: current_label.take(),
                        operation: BasicAstOperation::IfThenElse(condition, true_branch, BasicAstBlock::default())
                    });
                }

                tokens = &tokens[end_index..];
            }
            _ => {
                return Err(ParseError::UnexpectedToken(tokens[0].clone()));
            }
        }
    }

    Ok(BasicAstBlock { instructions })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_basic() {
        assert_eq!(
            tokenize("hello + world").unwrap(),
            vec![
                BasicToken::NewLine,
                BasicToken::Name(String::from("hello")),
                BasicToken::Operator(Operator::Add),
                BasicToken::Name(String::from("world")),
            ],
        );

        assert_eq!(
            tokenize("let thing = thing / 2").unwrap(),
            vec![
                BasicToken::NewLine,
                BasicToken::Name(String::from("thing")),
                BasicToken::Assign,
                BasicToken::Name(String::from("thing")),
                BasicToken::Operator(Operator::Div),
                BasicToken::Integer(2)
            ],
        );

        assert_eq!(
            tokenize("10 thing = thing + 0.5\ngoto 10").unwrap(),
            vec![
                BasicToken::NewLine,
                BasicToken::Integer(10),
                BasicToken::Name(String::from("thing")),
                BasicToken::Assign,
                BasicToken::Name(String::from("thing")),
                BasicToken::Operator(Operator::Add),
                BasicToken::Float(0.5),
                BasicToken::NewLine,
                BasicToken::Goto,
                BasicToken::Integer(10),
            ],
        );

        assert_eq!(
            tokenize("x = 0\n\nif x > 0 then\nprint(\"Positive\")\nend if").unwrap(),
            vec![
                BasicToken::NewLine,
                BasicToken::Name(String::from("x")),
                BasicToken::Assign,
                BasicToken::Integer(0),
                BasicToken::NewLine,
                BasicToken::If,
                BasicToken::Name(String::from("x")),
                BasicToken::Operator(Operator::Gt),
                BasicToken::Integer(0),
                BasicToken::Then,
                BasicToken::NewLine,
                BasicToken::Name(String::from("print")),
                BasicToken::OpenParen,
                BasicToken::String(String::from("Positive")),
                BasicToken::CloseParen,
                BasicToken::NewLine,
                BasicToken::EndIf,
            ],
        );
    }
}
