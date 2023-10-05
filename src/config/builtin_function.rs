use crate::{parse::ParseError, parse::ParseErrorKind, prelude::*, translate::Namer};

pub trait BuiltinFunction {
    fn validate_args(
        &self,
        args: &[BasicAstExpression],
        call_span: Position,
    ) -> Result<(), ParseError>;

    fn translate(
        &self,
        arg_names: &[String],
        namer: &mut Namer,
        config: &Config,
        target_name: String,
    ) -> MindustryProgram;
}

#[derive(Clone, Copy)]
pub struct Sensor;

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
        arg_names: &[String],
        _namer: &mut Namer,
        _config: &Config,
        target_name: String,
    ) -> MindustryProgram {
        vec![MindustryOperation::Sensor {
            out_name: target_name,
            object: Operand::Variable(arg_names[0].clone()),
            key: Operand::Variable(arg_names[1].clone()),
        }]
        .into()
    }
}
