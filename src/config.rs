use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Config {
    pub builtin_functions: HashMap<String, (String, bool, usize)>,
}

impl Default for Config {
    fn default() -> Self {
        macro_rules! builtin_function {
            ( $name:expr, $target_name:expr, $mutating:expr, $n_args:expr ) => {
                (
                    String::from($name),
                    (String::from($target_name), $mutating, $n_args),
                )
            };
        }
        Self {
            builtin_functions: HashMap::from([
                builtin_function!("print_flush", "printflush", false, 1),
                // TODO: write a special case for message
                builtin_function!("print_message_mission", "message mission", false, 0),
                builtin_function!("read", "read", true, 3),
                // TODO: don't use a generic operation here
                builtin_function!("write", "write", false, 3),
                builtin_function!("wait", "wait", false, 1),
                // TODO: don't use a generic operation here either
                builtin_function!("set_flag", "setflag", false, 2),
                builtin_function!("get_flag", "getflag", true, 2),
                // TODO: same thing
                builtin_function!("spawn", "spawn", false, 6),
            ]),
        }
    }
}
