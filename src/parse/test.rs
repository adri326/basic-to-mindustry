use crate::cursor::Cursor;

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

#[test]
fn test_operator_precedence() {
    fn test_parse<const N: usize>(list: [BasicToken; N]) -> BasicAstExpression {
        parse_expression(&mut Cursor::from(&list)).unwrap()
    }

    fn test_err<const N: usize>(list: [BasicToken; N]) -> ParseError {
        parse_expression(&mut Cursor::from(&list)).err().unwrap()
    }

    assert_eq!(
        test_parse([BasicToken::Name(String::from("hello"))]),
        BasicAstExpression::Variable(String::from("hello"))
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Name(String::from("world")),
        ]),
        BasicAstExpression::Variable(String::from("hello"))
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::Integer(1),
        ]),
        BasicAstExpression::Binary(
            Operator::Add,
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Integer(1)),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Mul),
            BasicToken::Name(String::from("world")),
        ]),
        BasicAstExpression::Binary(
            Operator::Add,
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Mul,
                Box::new(BasicAstExpression::Integer(2)),
                Box::new(BasicAstExpression::Variable(String::from("world"))),
            )),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Mul),
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Add),
            BasicToken::Name(String::from("world")),
        ]),
        BasicAstExpression::Binary(
            Operator::Add,
            Box::new(BasicAstExpression::Binary(
                Operator::Mul,
                Box::new(BasicAstExpression::Variable(String::from("hello"))),
                Box::new(BasicAstExpression::Integer(2)),
            )),
            Box::new(BasicAstExpression::Variable(String::from("world"))),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Mul),
            BasicToken::OpenParen,
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Add),
            BasicToken::Name(String::from("world")),
            BasicToken::CloseParen,
        ]),
        BasicAstExpression::Binary(
            Operator::Mul,
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Add,
                Box::new(BasicAstExpression::Integer(2)),
                Box::new(BasicAstExpression::Variable(String::from("world"))),
            )),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::OpenParen,
            BasicToken::Name(String::from("world")),
            BasicToken::Operator(Operator::Mul),
            BasicToken::Integer(2),
            BasicToken::CloseParen,
        ]),
        BasicAstExpression::Binary(
            Operator::Add,
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Mul,
                Box::new(BasicAstExpression::Variable(String::from("world"))),
                Box::new(BasicAstExpression::Integer(2)),
            )),
        )
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
        ]),
        ParseError::ExpectedOperand
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::OpenParen,
            BasicToken::Name(String::from("world")),
            BasicToken::Operator(Operator::Mul),
            BasicToken::Integer(2),
        ]),
        ParseError::MissingToken(BasicToken::CloseParen)
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::Operator(Operator::Mul),
        ]),
        ParseError::UnexpectedToken(BasicToken::Operator(Operator::Mul))
    );

    assert!(matches!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::OpenParen,
        ]),
        ParseError::ExpectedOperand | ParseError::MissingToken(BasicToken::CloseParen)
    ));

    assert!(matches!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add),
            BasicToken::OpenParen,
            BasicToken::CloseParen
        ]),
        ParseError::ExpectedOperand | ParseError::UnexpectedToken(BasicToken::CloseParen)
    ));

    assert_eq!(
        test_err([BasicToken::Operator(Operator::Add), BasicToken::Integer(2)]),
        ParseError::UnexpectedToken(BasicToken::Operator(Operator::Add))
    );
}
