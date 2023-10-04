use std::collections::HashMap;

use crate::{
    parse::ParseError,
    repr::basic::{BasicAstExpression, BasicAstInstruction},
};

pub struct Config {
    pub builtin_functions: HashMap<String, (Option<String>, bool, usize)>,

    /// Used for functions like `print_flush_world`
    pub special_functions: HashMap<
        String,
        Box<dyn Fn(Vec<BasicAstExpression>) -> Result<BasicAstInstruction, ParseError>>,
    >,
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

        let mut special_functions: HashMap<
            String,
            Box<dyn Fn(Vec<BasicAstExpression>) -> Result<BasicAstInstruction, _>>,
        > = HashMap::new();

        special_functions.insert(
            String::from("print_flush_global"),
            Box::new(|arguments: Vec<BasicAstExpression>| {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    return Err(ParseError::InvalidArgument(arguments[0].clone()));
                };

                let expected_length = match buffer.as_str() {
                    "notify" => 1,
                    "mission" => 1,
                    "announce" => 2,
                    "toast" => 2,
                    _ => return Err(ParseError::InvalidArgument(arguments[0].clone())),
                };

                if arguments.len() != expected_length {
                    return Err(ParseError::InvalidArgumentCount(
                        String::from("print_flush_global"),
                        expected_length,
                        arguments.len(),
                    ));
                }

                Ok(BasicAstInstruction::CallBuiltin(
                    String::from("print_flush_global"),
                    arguments,
                ))
            }),
        );

        special_functions.insert(
            String::from("control"),
            Box::new(|arguments| {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    return Err(ParseError::InvalidArgument(arguments[0].clone()));
                };

                let expected_length = match buffer.as_str() {
                    "enabled" => 3,
                    "shoot" => 5,
                    "shootp" => 4,
                    "config" => 3,
                    "color" => 3,
                    _ => return Err(ParseError::InvalidArgument(arguments[0].clone())),
                };

                if arguments.len() != expected_length {
                    return Err(ParseError::InvalidArgumentCount(
                        String::from("control"),
                        expected_length,
                        arguments.len(),
                    ));
                }

                Ok(BasicAstInstruction::CallBuiltin(
                    String::from("control"),
                    arguments,
                ))
            }),
        );

        Self {
            builtin_functions: HashMap::from([
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
            special_functions,
        }
    }
}
