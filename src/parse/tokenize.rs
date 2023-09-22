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
    pub(crate) fn precedence(self) -> u8 {
        use Operator as O;
        match self {
            O::Add | O::Sub => 3,
            O::RShift | O::LShift => 4,
            O::Mod => 5,
            O::Mul | O::Div => 10,
            O::Eq | O::Neq | O::Gt | O::Lt | O::Gte | O::Lte => 0,
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
            // Main match clause for tokens
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
