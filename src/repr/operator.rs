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
