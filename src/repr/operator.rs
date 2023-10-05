#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    IDiv,
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
    And,
    Or,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BasicOperator {
    Operator(Operator),
    // TODO: rename to something else and have it not use variables as rhs keys
    Sensor,
}

impl BasicOperator {
    pub(crate) fn precedence(self) -> u8 {
        match self {
            Self::Sensor => 20,
            Self::Operator(op) => op.precedence(),
        }
    }

    pub(crate) fn from_fn_name(raw: &str) -> Option<Self> {
        match raw {
            "max" => Some(Self::Operator(Operator::Max)),
            "min" => Some(Self::Operator(Operator::Min)),
            "pow" => Some(Self::Operator(Operator::Pow)),
            _ => None,
        }
    }
}

impl Operator {
    pub(crate) fn precedence(self) -> u8 {
        match self {
            Self::Add | Self::Sub => 3,
            Self::RShift | Self::LShift => 4,
            Self::Mod => 5,
            Self::Mul | Self::Div | Self::IDiv => 10,
            Self::Eq | Self::Neq | Self::Gt | Self::Lt | Self::Gte | Self::Lte => 1,
            Self::And | Self::Or => 0,
            _ => 128,
        }
    }
}

impl TryFrom<BasicOperator> for Operator {
    type Error = ();

    fn try_from(value: BasicOperator) -> Result<Self, Self::Error> {
        match value {
            BasicOperator::Operator(op) => Ok(op),
            BasicOperator::Sensor => Err(()),
        }
    }
}

impl From<Operator> for BasicOperator {
    fn from(value: Operator) -> Self {
        Self::Operator(value)
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
        Operator::IDiv => "idiv",
        Operator::Mod => "mod",
        Operator::RShift => "shr",
        Operator::LShift => "shl",
        Operator::Max => "max",
        Operator::Min => "min",
        Operator::Pow => "pow",
        Operator::And => "and",
        Operator::Or => "or",
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum UnaryOperator {
    Floor,
    Round,
    Ceil,
    Rand,
    Sqrt,
    // Not,
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
