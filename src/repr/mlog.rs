use super::operator::*;

#[derive(Debug, Clone)]
pub enum Operand {
    Variable(String),
    String(String),
    Integer(i64),
    Float(f64),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Variable(name) => write!(f, "{}", name),
            Operand::String(string) => {
                let escaped = string.replace('\\', r"\\").replace('"', r#"\""#);
                write!(f, "\"{}\"", escaped)
            }
            Operand::Integer(int) => write!(f, "{}", int),
            Operand::Float(float) => write!(f, "{}", float),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MindustryOperation {
    JumpLabel(String),
    Jump(String),
    JumpIf(String, Operator, Operand, Operand),
    Operator(String, Operator, Operand, Operand),
    UnaryOperator(String, UnaryOperator, Operand),
    // TODO: add RandOperator
    Set(String, Operand),
    /// A generic operation, with the following invariants:
    /// - all of the operands are read-only
    /// - there is no external dependency to other variables
    /// - no external variable is modified
    Generic(String, Vec<Operand>),
    /// A generic, mutating operation `(name, out_name, operands)`, with the following invariants:
    /// - all of the operands are read-only
    /// - there is no external dependency to other variables, except `out_name`
    /// - only `out_name` is modified
    GenericMut(String, String, Vec<Operand>),
}

impl MindustryOperation {
    pub(crate) fn operands(&self) -> Box<[&Operand]> {
        match self {
            Self::JumpIf(_label, _operator, lhs, rhs) => Box::new([lhs, rhs]),
            Self::Operator(_target, _operator, lhs, rhs) => Box::new([lhs, rhs]),
            Self::UnaryOperator(_target, _operator, value) => Box::new([value]),
            Self::Set(_target, value) => Box::new([value]),
            Self::Generic(_name, operands) => {
                operands.iter().collect::<Vec<_>>().into_boxed_slice()
            }
            Self::GenericMut(_name, _out_name, operands) => {
                operands.iter().collect::<Vec<_>>().into_boxed_slice()
            }
            _ => Box::new([]),
        }
    }

    pub(crate) fn operands_mut(&mut self) -> Vec<&mut Operand> {
        match self {
            Self::JumpIf(_label, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::Operator(_target, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::UnaryOperator(_target, _operator, value) => vec![value],
            Self::Set(_target, value) => vec![value],
            Self::Generic(_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            Self::GenericMut(_name, _out_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            _ => vec![],
        }
    }

    pub(crate) fn mutates(&self, var_name: &str) -> bool {
        match self {
            MindustryOperation::JumpLabel(_)
            | MindustryOperation::Jump(_)
            | MindustryOperation::JumpIf(_, _, _, _)
            | MindustryOperation::Generic(_, _) => false,

            MindustryOperation::Operator(out_name, _, _, _)
            | MindustryOperation::UnaryOperator(out_name, _, _)
            | MindustryOperation::Set(out_name, _)
            | MindustryOperation::GenericMut(_, out_name, _) => out_name == var_name,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct MindustryProgram(pub Vec<MindustryOperation>);

impl MindustryProgram {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, operation: MindustryOperation) {
        self.0.push(operation);
    }

    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0);
    }
}

impl From<Vec<MindustryOperation>> for MindustryProgram {
    fn from(value: Vec<MindustryOperation>) -> Self {
        Self(value)
    }
}
