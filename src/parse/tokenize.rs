use super::ParseError;
use crate::prelude::*;
use regex::Regex;

#[derive(PartialEq, Clone, Debug)]
pub enum BasicToken {
    NewLine,
    Assign,
    If,
    Then,
    Else,
    EndIf,
    Goto,
    For,
    To,
    Step,
    Next,
    LabelEnd,
    OpenParen,
    CloseParen,
    Comma,
    Print,
    Integer(i64),
    Float(f64),
    Name(String),
    String(String),
    Operator(Operator),
}

/// Transforms a raw string into a sequence of `BasicToken`s
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
    let match_word =
        Regex::new(r"(?i)^(?:if|then|else|end\s?if|print|for|to|step|next)(?:\s|$)").unwrap();
    let match_space = Regex::new(r"^\s+").unwrap();
    let match_variable = Regex::new(r"^@?[a-zA-Z_][a-zA-Z_0-9]*").unwrap();
    let match_float = Regex::new(r"^[0-9]*\.[0-9]+").unwrap();
    let match_integer = Regex::new(r"^[0-9]+").unwrap();
    let match_assign = Regex::new(r"^=").unwrap();
    let match_comma = Regex::new(r"^,").unwrap();
    let match_operator = Regex::new(r"^(?:[+\-*/%]|[<>]=?|[!=]=|<<|>>)").unwrap();
    let match_label_end = Regex::new(r"^:").unwrap();
    let match_paren = Regex::new(r"^(?:\(|\))").unwrap();
    // TODO: handle escapes
    let match_string = Regex::new(r#""[^"]*""#).unwrap();
    let match_comment = Regex::new(r"(?i)^rem\s.*$").unwrap();
    // TODO: handle labels

    for mut line in raw.lines() {
        if !line.is_empty() {
            res.push(BasicToken::NewLine);
        }
        while !line.is_empty() {
            // Main match clause for tokens
            match_token!(line, res;
                match_space => (),
                match_let => (),
                match_comment => (),
                match_jump => (BasicToken::Goto),
                match_word(word) => (match word.to_lowercase().as_str().trim() {
                    "if" => BasicToken::If,
                    "then" => BasicToken::Then,
                    "else" => BasicToken::Else,
                    "end if" | "endif" => BasicToken::EndIf,
                    "print" => BasicToken::Print,
                    "for" => BasicToken::For,
                    "to" => BasicToken::To,
                    "step" => BasicToken::Step,
                    "next" => BasicToken::Next,
                    _ => unreachable!("{}", word),
                }),
                match_variable(name) => (BasicToken::Name(name.to_string())),
                match_float(float) => (BasicToken::Float(float.parse().unwrap())),
                match_integer(int) => (BasicToken::Integer(int.parse().unwrap())),
                match_comma => (BasicToken::Comma),
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
                    "==" => Operator::Eq,
                    "!=" => Operator::Neq,
                    _ => unreachable!(),
                })),
                match_assign => (BasicToken::Assign),
                match_label_end => (BasicToken::LabelEnd),
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
