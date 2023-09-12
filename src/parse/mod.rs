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
    Integer(u64),
    Float(f64),
    Name(String),
    String(String),
    Operator(Operator),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParseError {
    InvalidToken(String),
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
