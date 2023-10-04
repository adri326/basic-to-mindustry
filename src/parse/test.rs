use super::*;
use crate::prelude::*;

fn test_drop_position(tokens: Result<Vec<(BasicToken, Position)>, ParseError>) -> Vec<BasicToken> {
    tokens.unwrap().into_iter().map(|pair| pair.0).collect()
}

#[test]
fn test_tokenize_basic() {
    assert_eq!(
        test_drop_position(tokenize("hello + world")),
        vec![
            BasicToken::NewLine,
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Name(String::from("world")),
        ],
    );

    assert_eq!(
        test_drop_position(tokenize("let thing = thing / 2")),
        vec![
            BasicToken::NewLine,
            BasicToken::Name(String::from("thing")),
            BasicToken::Assign,
            BasicToken::Name(String::from("thing")),
            BasicToken::Operator(Operator::Div.into()),
            BasicToken::Integer(2)
        ],
    );

    assert_eq!(
        test_drop_position(tokenize("10 thing = thing + 0.5\ngoto 10")),
        vec![
            BasicToken::NewLine,
            BasicToken::Integer(10),
            BasicToken::Name(String::from("thing")),
            BasicToken::Assign,
            BasicToken::Name(String::from("thing")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Float(0.5),
            BasicToken::NewLine,
            BasicToken::Goto,
            BasicToken::Integer(10),
        ],
    );

    assert_eq!(
        test_drop_position(tokenize(
            "x = 0\n\nif x > 0 then\nprint(\"Positive\")\nend if"
        )),
        vec![
            BasicToken::NewLine,
            BasicToken::Name(String::from("x")),
            BasicToken::Assign,
            BasicToken::Integer(0),
            BasicToken::NewLine,
            BasicToken::If,
            BasicToken::Name(String::from("x")),
            BasicToken::Operator(Operator::Gt.into()),
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

    assert_eq!(
        test_drop_position(tokenize("if x > 0 then\nend\nend if")),
        vec![
            BasicToken::NewLine,
            BasicToken::If,
            BasicToken::Name(String::from("x")),
            BasicToken::Operator(Operator::Gt.into()),
            BasicToken::Integer(0),
            BasicToken::Then,
            BasicToken::NewLine,
            BasicToken::End,
            BasicToken::NewLine,
            BasicToken::EndIf,
        ]
    );
}

#[test]
fn test_parse_for() {
    assert_eq!(
        test_drop_position(tokenize("FOR x = 0 TO y\nPRINT x\nNEXT x")),
        vec![
            BasicToken::NewLine,
            BasicToken::For,
            BasicToken::Name(String::from("x")),
            BasicToken::Assign,
            BasicToken::Integer(0),
            BasicToken::To,
            BasicToken::Name(String::from("y")),
            BasicToken::NewLine,
            BasicToken::Print,
            BasicToken::Name(String::from("x")),
            BasicToken::NewLine,
            BasicToken::Next,
            BasicToken::Name(String::from("x")),
        ]
    );

    assert_eq!(
        build_ast(
            &tokenize("FOR x = 0 TO y\nPRINT x\nNEXT x").unwrap(),
            &Default::default()
        )
        .unwrap(),
        BasicAstBlock::new([BasicAstInstruction::For {
            variable: String::from("x"),
            start: BasicAstExpression::Integer(0),
            end: BasicAstExpression::Variable(String::from("y")),
            step: BasicAstExpression::Integer(1),
            instructions: BasicAstBlock::new([BasicAstInstruction::Print(vec![(
                BasicAstExpression::Variable(String::from("x")),
                false
            )]),])
        }])
    );
}

#[test]
fn test_operator_precedence() {
    fn test_parse<const N: usize>(list: [BasicToken; N]) -> BasicAstExpression {
        parse_expression(&mut Cursor::from(
            &list
                .into_iter()
                .map(|token| (token, Position::default()))
                .collect::<Vec<_>>()[..],
        ))
        .unwrap()
    }

    fn test_err<const N: usize>(list: [BasicToken; N]) -> ParseErrorKind {
        parse_expression(&mut Cursor::from(
            &list
                .into_iter()
                .map(|token| (token, Position::default()))
                .collect::<Vec<_>>()[..],
        ))
        .err()
        .unwrap()
        .kind
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
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Integer(1),
        ]),
        BasicAstExpression::Binary(
            Operator::Add.into(),
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Integer(1)),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Mul.into()),
            BasicToken::Name(String::from("world")),
        ]),
        BasicAstExpression::Binary(
            Operator::Add.into(),
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Mul.into(),
                Box::new(BasicAstExpression::Integer(2)),
                Box::new(BasicAstExpression::Variable(String::from("world"))),
            )),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Mul.into()),
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Name(String::from("world")),
        ]),
        BasicAstExpression::Binary(
            Operator::Add.into(),
            Box::new(BasicAstExpression::Binary(
                Operator::Mul.into(),
                Box::new(BasicAstExpression::Variable(String::from("hello"))),
                Box::new(BasicAstExpression::Integer(2)),
            )),
            Box::new(BasicAstExpression::Variable(String::from("world"))),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Mul.into()),
            BasicToken::OpenParen,
            BasicToken::Integer(2),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Name(String::from("world")),
            BasicToken::CloseParen,
        ]),
        BasicAstExpression::Binary(
            Operator::Mul.into(),
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Add.into(),
                Box::new(BasicAstExpression::Integer(2)),
                Box::new(BasicAstExpression::Variable(String::from("world"))),
            )),
        )
    );

    assert_eq!(
        test_parse([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::OpenParen,
            BasicToken::Name(String::from("world")),
            BasicToken::Operator(Operator::Mul.into()),
            BasicToken::Integer(2),
            BasicToken::CloseParen,
        ]),
        BasicAstExpression::Binary(
            Operator::Add.into(),
            Box::new(BasicAstExpression::Variable(String::from("hello"))),
            Box::new(BasicAstExpression::Binary(
                Operator::Mul.into(),
                Box::new(BasicAstExpression::Variable(String::from("world"))),
                Box::new(BasicAstExpression::Integer(2)),
            )),
        )
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
        ]),
        ParseErrorKind::ExpectedOperand
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::OpenParen,
            BasicToken::Name(String::from("world")),
            BasicToken::Operator(Operator::Mul.into()),
            BasicToken::Integer(2),
        ]),
        ParseErrorKind::MissingToken(BasicToken::CloseParen)
    );

    assert_eq!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Operator(Operator::Mul.into()),
        ]),
        ParseErrorKind::UnexpectedToken(BasicToken::Operator(Operator::Mul.into()))
    );

    assert!(matches!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::OpenParen,
        ]),
        ParseErrorKind::ExpectedOperand | ParseErrorKind::MissingToken(BasicToken::CloseParen)
    ));

    assert!(matches!(
        test_err([
            BasicToken::Name(String::from("hello")),
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::OpenParen,
            BasicToken::CloseParen
        ]),
        ParseErrorKind::ExpectedOperand | ParseErrorKind::UnexpectedToken(BasicToken::CloseParen)
    ));

    assert_eq!(
        test_err([
            BasicToken::Operator(Operator::Add.into()),
            BasicToken::Integer(2)
        ]),
        ParseErrorKind::UnexpectedToken(BasicToken::Operator(Operator::Add.into()))
    );
}

fn test_build_ast(raw: &str) -> BasicAstBlock {
    let tokens = tokenize(raw).unwrap_or_else(|e| {
        panic!(
            "Error while tokenizing: {:?}\nProgram:\n```\n{}\n```",
            e, raw
        );
    });
    let parsed = build_ast(&tokens, &Default::default()).unwrap_or_else(|e| {
        panic!(
            "Error while parsing: {:?}\nProgram:\n```\n{}\n```\nTokens:\n{:#?}",
            e, raw, tokens
        );
    });
    parsed
}

#[test]
fn test_ast_basics() {
    assert_eq!(
        test_build_ast("LET X = 2 * 5\n"),
        BasicAstBlock::new([BasicAstInstruction::Assign(
            String::from("X"),
            BasicAstExpression::Integer(2) * BasicAstExpression::Integer(5)
        )])
    );

    assert_eq!(
        test_build_ast("IF X < 0 THEN\nX = 0-X\nEND IF"),
        BasicAstBlock::new([BasicAstInstruction::IfThenElse(
            BasicAstExpression::Binary(
                Operator::Lt.into(),
                Box::new(BasicAstExpression::Variable(String::from("X"))),
                Box::new(BasicAstExpression::Integer(0))
            ),
            BasicAstBlock::new([BasicAstInstruction::Assign(
                String::from("X"),
                BasicAstExpression::Integer(0) - BasicAstExpression::Variable(String::from("X"))
            )]),
            BasicAstBlock::default()
        ),])
    );

    assert_eq!(
        test_build_ast("GOTO 10\nGOTO hello"),
        BasicAstBlock::new([
            BasicAstInstruction::Jump(String::from("10")),
            BasicAstInstruction::Jump(String::from("hello")),
        ])
    );
}

#[test]
fn test_ast_labels() {
    assert_eq!(
        test_build_ast("10 LET X = 0\n20 LET Y = 2 * X\n"),
        BasicAstBlock::new([
            BasicAstInstruction::JumpLabel(String::from("10")),
            BasicAstInstruction::Assign(String::from("X"), BasicAstExpression::Integer(0)),
            BasicAstInstruction::JumpLabel(String::from("20")),
            BasicAstInstruction::Assign(
                String::from("Y"),
                BasicAstExpression::Integer(2) * BasicAstExpression::Variable(String::from("X"))
            ),
        ])
    );

    assert_eq!(
        test_build_ast("start: LET X = 0\nmultiply: LET Y = 2 * X\n"),
        BasicAstBlock::new([
            BasicAstInstruction::JumpLabel(String::from("start")),
            BasicAstInstruction::Assign(String::from("X"), BasicAstExpression::Integer(0)),
            BasicAstInstruction::JumpLabel(String::from("multiply")),
            BasicAstInstruction::Assign(
                String::from("Y"),
                BasicAstExpression::Integer(2) * BasicAstExpression::Variable(String::from("X"))
            ),
        ])
    );
}
