use super::*;

#[test]
fn test_interprete_operation() {
    fn run_test(
        instructions: impl IntoIterator<Item = MindustryOperation>,
    ) -> HashMap<String, Value> {
        let program = MindustryProgram::from(instructions.into_iter().collect::<Vec<_>>());

        let end_state = run(&program, StopCondition::End);

        end_state
    }

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Add,
            Operand::Integer(1),
            Operand::Float(2.0)
        )])
        .get("x"),
        Some(&Value::Number(3.0))
    );

    assert_eq!(
        run_test([
            MindustryOperation::Set(String::from("a"), Operand::Integer(2)),
            MindustryOperation::Operator(
                String::from("x"),
                Operator::Add,
                Operand::Variable(String::from("a")),
                Operand::Integer(1)
            )
        ])
        .get("x"),
        Some(&Value::Number(3.0))
    );

    assert_eq!(
        run_test([
            MindustryOperation::Set(String::from("a"), Operand::Float(-1.5)),
            MindustryOperation::Set(String::from("b"), Operand::Float(0.25)),
            MindustryOperation::Operator(
                String::from("x"),
                Operator::Add,
                Operand::Variable(String::from("a")),
                Operand::Variable(String::from("b"))
            )
        ])
        .get("x"),
        Some(&Value::Number(-1.25))
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Add,
            Operand::Integer(7),
            Operand::String(String::from("2"))
        )])
        .get("x"),
        Some(&Value::Number(8.0)),
        "7 + \"2\" should equal 8"
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Add,
            Operand::Integer(7),
            Operand::String(String::from(""))
        )])
        .get("x"),
        Some(&Value::Number(8.0)),
        "7 + \"\" should equal 8"
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Sub,
            Operand::Integer(7),
            Operand::Integer(3)
        )])
        .get("x"),
        Some(&Value::Number(4.0)),
        "7 - 3 should equal 4"
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Eq,
            Operand::Integer(7),
            Operand::Integer(3)
        )])
        .get("x"),
        Some(&Value::Number(0.0)),
        "7 == 3 should equal 0"
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Eq,
            Operand::Integer(7),
            Operand::Integer(7)
        )])
        .get("x"),
        Some(&Value::Number(1.0)),
        "7 == 7 should equal 1"
    );

    assert_eq!(
        run_test([MindustryOperation::Operator(
            String::from("x"),
            Operator::Neq,
            Operand::Integer(7),
            Operand::Integer(3)
        )])
        .get("x"),
        Some(&Value::Number(1.0)),
        "7 != 3 should equal 1"
    );
}
