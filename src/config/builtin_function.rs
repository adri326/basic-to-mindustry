use crate::{
    parse::ParseError,
    parse::ParseErrorKind,
    prelude::*,
    translate::{translate_expression, Namer},
};

pub trait BuiltinFunction {
    fn validate_args(
        &self,
        args: &[BasicAstExpression],
        call_span: Position,
    ) -> Result<(), ParseError>;

    fn translate(
        &self,
        arguments: &[BasicAstExpression],
        namer: &mut Namer,
        config: &Config,
        target_name: String,
    ) -> MindustryProgram;
}

macro_rules! expect_n_args {
    ( $args:expr, $call_span:expr, $expected:expr, $name:expr ) => {
        if $args.len() != $expected {
            return Err(ParseError::new(
                ParseErrorKind::InvalidArgumentCount(String::from($name), $expected, $args.len()),
                $call_span,
            ));
        }
    };
}

fn translate_arguments(
    arguments: &[BasicAstExpression],
    namer: &mut Namer,
    config: &Config,
) -> (Vec<String>, MindustryProgram) {
    let names = (0..arguments.len())
        .map(|_| namer.temporary())
        .collect::<Vec<_>>();

    let mut res = MindustryProgram::new();

    for (index, arg) in arguments.iter().enumerate() {
        res.append(&mut translate_expression(
            arg,
            namer,
            names[index].clone(),
            config,
        ));
    }

    (names, res)
}

#[derive(Clone, Copy)]
pub struct Sensor;

impl BuiltinFunction for Sensor {
    fn validate_args(
        &self,
        args: &[BasicAstExpression],
        call_span: Position,
    ) -> Result<(), ParseError> {
        expect_n_args!(args, call_span, 2, "SENSOR");
        Ok(())
    }

    fn translate(
        &self,
        arguments: &[BasicAstExpression],
        namer: &mut Namer,
        config: &Config,
        target_name: String,
    ) -> MindustryProgram {
        let (arg_names, mut res) = translate_arguments(arguments, namer, config);

        res.push(MindustryOperation::Sensor {
            out_name: target_name,
            object: Operand::Variable(arg_names[0].clone()),
            key: Operand::Variable(arg_names[1].clone()),
        });

        res
    }
}

/// An alternative to `Sensor` that uses a key for the second argument, instead of a variable that is dereferenced at runtime.
/// Used for the dot (`.`) operator, which allows users to write `@unit.health` instead of `@unit.@health`
#[derive(Clone, Copy)]
pub struct SensorOperator;

impl BuiltinFunction for SensorOperator {
    fn validate_args(
        &self,
        args: &[BasicAstExpression],
        call_span: Position,
    ) -> Result<(), ParseError> {
        // Should only raise an error if a user calls the function manually
        expect_n_args!(args, call_span, 2, "__SENSOR_OPERATOR");

        match &args[1] {
            BasicAstExpression::Variable(_name) => Ok(()),
            other => Err(ParseError::new(
                ParseErrorKind::InvalidArgument(other.clone()),
                call_span,
            )),
        }
    }

    fn translate(
        &self,
        arguments: &[BasicAstExpression],
        namer: &mut Namer,
        config: &Config,
        target_name: String,
    ) -> MindustryProgram {
        let object_name = namer.temporary();
        let mut res = translate_expression(&arguments[0], namer, object_name.clone(), config);
        let BasicAstExpression::Variable(key) = &arguments[1] else {
            unreachable!("Expected second argument to __sensor_operator to be a variable");
        };

        res.push(MindustryOperation::Sensor {
            out_name: target_name,
            object: Operand::Variable(object_name),
            key: Operand::Variable(format!("@{}", key)),
        });

        res
    }
}
