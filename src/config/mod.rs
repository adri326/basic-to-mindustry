use std::collections::HashMap;

use crate::{
    parse::{ParseError, ParseErrorKind},
    repr::basic::{BasicAstExpression, BasicAstInstruction},
    repr::position::Position,
};

mod builtin_function;
use builtin_function::*;

pub struct Config {
    pub builtin_routines: HashMap<String, (Option<String>, bool, usize)>,

    /// Used for functions like `print_flush_world`
    pub special_routines: HashMap<
        String,
        Box<dyn Fn(Vec<BasicAstExpression>, Position) -> Result<BasicAstInstruction, ParseError>>,
    >,

    pub builtin_functions: HashMap<String, Box<dyn BuiltinFunction>>,
}

impl Default for Config {
    fn default() -> Self {
        macro_rules! builtin_function {
            ( $name:expr, $target_name:expr, $mutating:expr, $n_args:expr ) => {
                (
                    String::from($name),
                    (
                        ($target_name as Option<&'static str>).map(String::from),
                        $mutating,
                        $n_args,
                    ),
                )
            };
        }

        let mut special_routines: HashMap<
            String,
            Box<dyn Fn(Vec<BasicAstExpression>, Position) -> Result<BasicAstInstruction, _>>,
        > = HashMap::new();

        special_routines.insert(
            String::from("print_flush_global"),
            Box::new(|arguments: Vec<BasicAstExpression>, position| {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    return Err(ParseError::new(
                        ParseErrorKind::InvalidArgument(arguments[0].clone()),
                        position,
                    ));
                };

                let expected_length = match buffer.as_str() {
                    "notify" => 1,
                    "mission" => 1,
                    "announce" => 2,
                    "toast" => 2,
                    _ => {
                        return Err(ParseError::new(
                            ParseErrorKind::InvalidArgument(arguments[0].clone()),
                            position,
                        ))
                    }
                };

                if arguments.len() != expected_length {
                    return Err(ParseError::new(
                        ParseErrorKind::InvalidArgumentCount(
                            String::from("print_flush_global"),
                            expected_length,
                            arguments.len(),
                        ),
                        position,
                    ));
                }

                Ok(BasicAstInstruction::CallBuiltin(
                    String::from("print_flush_global"),
                    arguments,
                ))
            }),
        );

        special_routines.insert(
            String::from("control"),
            Box::new(|arguments, position| {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    return Err(ParseError::new(
                        ParseErrorKind::InvalidArgument(arguments[0].clone()),
                        position,
                    ));
                };

                let expected_length = match buffer.as_str() {
                    "enabled" => 3,
                    "shoot" => 5,
                    "shootp" => 4,
                    "config" => 3,
                    "color" => 3,
                    _ => {
                        return Err(ParseError::new(
                            ParseErrorKind::InvalidArgument(arguments[0].clone()),
                            position,
                        ))
                    }
                };

                if arguments.len() != expected_length {
                    return Err(ParseError::new(
                        ParseErrorKind::InvalidArgumentCount(
                            String::from("control"),
                            expected_length,
                            arguments.len(),
                        ),
                        position,
                    ));
                }

                Ok(BasicAstInstruction::CallBuiltin(
                    String::from("control"),
                    arguments,
                ))
            }),
        );

        let mut builtin_functions: HashMap<String, Box<dyn BuiltinFunction>> = HashMap::new();

        builtin_functions.insert(String::from("sensor"), Box::new(Sensor));

        Self {
            builtin_routines: HashMap::from([
                builtin_function!("print_flush", None, false, 1),
                builtin_function!("read", None, true, 3),
                builtin_function!("write", None, false, 3),
                builtin_function!("wait", Some("wait"), false, 1),
                // TODO: don't use a generic operation here either
                builtin_function!("set_flag", Some("setflag"), false, 2),
                builtin_function!("get_flag", Some("getflag"), true, 2),
                // TODO: same thing
                builtin_function!("spawn", Some("spawn"), false, 6),
            ]),
            special_routines,
            builtin_functions,
        }
    }
}
