use std::collections::HashMap;

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
    Max,
    Min,
    Pow,
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
            _ => 128,
        }
    }

    pub(crate) fn from_fn_name(raw: &str) -> Option<Self> {
        match raw {
            "max" => Some(Self::Max),
            "min" => Some(Self::Min),
            "pow" => Some(Self::Pow),
            _ => None,
        }
    }
}

pub(crate) fn format_operator(operator: Operator) -> &'static str {
    match operator {
        Operator::Eq => "equal",
        Operator::Neq => "notEqual",
        Operator::Lt => "lessThan",
        Operator::Lte => "lessThanEq",
        Operator::Gt => "greaterThan",
        Operator::Gte => "greaterThanEq",
        Operator::Add => "add",
        Operator::Sub => "sub",
        Operator::Mul => "mul",
        Operator::Div => "div",
        Operator::Mod => "mod",
        Operator::RShift => "shr",
        Operator::LShift => "shl",
        Operator::Max => "max",
        Operator::Min => "min",
        Operator::Pow => "pow",
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum UnaryOperator {
    Floor,
    Round,
    Ceil,
    Rand,
    Sqrt,
}

impl TryFrom<&str> for UnaryOperator {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "floor" => Ok(Self::Floor),
            "round" => Ok(Self::Round),
            "ceil" => Ok(Self::Ceil),
            "rand" => Ok(Self::Rand),
            "sqrt" => Ok(Self::Sqrt),
            _ => Err(()),
        }
    }
}

pub(crate) fn format_unary_operator(operator: UnaryOperator) -> &'static str {
    match operator {
        UnaryOperator::Floor => "floor",
        UnaryOperator::Round => "round",
        UnaryOperator::Ceil => "ceil",
        UnaryOperator::Rand => "rand",
        UnaryOperator::Sqrt => "sqrt",
    }
}

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
