use super::operator::*;

const STRING_BUFFER: &'static str = "@__mlog__string";

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
pub enum WorldPrintFlush {
    Notify,
    Mission,
    Announce(Operand),
    Toast(Operand),
}

#[derive(Debug, Clone)]
pub enum MindustryOperation {
    JumpLabel(String),
    Jump(String),
    JumpIf(String, Operator, Operand, Operand),
    End,
    Operator(String, Operator, Operand, Operand),
    UnaryOperator(String, UnaryOperator, Operand),

    /// Reads the value at the `index`-th place in `cell`
    Read {
        out_name: String,
        cell: Operand,
        index: Operand,
    },
    /// Writes `value` to the `index`-th place in `cell`
    Write {
        value: Operand,
        cell: Operand,
        index: Operand,
    },
    /// Appends the given value to the print buffer
    Print(Operand),
    /// Flushes the print buffer to a message cell
    PrintFlush(Operand),
    /// Available to world processors only - flushes the print buffer to a global buffer
    WorldPrintFlush(WorldPrintFlush),

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
            Self::Jump(_) | Self::JumpLabel(_) | Self::End => Box::new([]),
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
            Self::PrintFlush(cell) => Box::new([cell]),
            Self::WorldPrintFlush(wpf) => match wpf {
                WorldPrintFlush::Notify => Box::new([]),
                WorldPrintFlush::Mission => Box::new([]),
                WorldPrintFlush::Announce(time) => Box::new([time]),
                WorldPrintFlush::Toast(time) => Box::new([time]),
            },
            Self::Print(operand) => Box::new([operand]),
            Self::Read {
                out_name: _,
                cell,
                index,
            } => Box::new([cell, index]),
            Self::Write { value, cell, index } => Box::new([value, cell, index]),
        }
    }

    pub(crate) fn operands_mut(&mut self) -> Vec<&mut Operand> {
        match self {
            Self::Jump(_) | Self::JumpLabel(_) | Self::End => vec![],
            Self::JumpIf(_label, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::Operator(_target, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::UnaryOperator(_target, _operator, value) => vec![value],
            Self::Set(_target, value) => vec![value],
            Self::Generic(_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            Self::GenericMut(_name, _out_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            Self::PrintFlush(cell) => vec![cell],
            Self::WorldPrintFlush(wpf) => match wpf {
                WorldPrintFlush::Notify => vec![],
                WorldPrintFlush::Mission => vec![],
                WorldPrintFlush::Announce(time) => vec![time],
                WorldPrintFlush::Toast(time) => vec![time],
            },
            Self::Print(operand) => vec![operand],
            Self::Read {
                out_name: _,
                cell,
                index,
            } => vec![cell, index],
            Self::Write { value, cell, index } => vec![value, cell, index],
        }
    }

    pub(crate) fn mutates(&self, var_name: &str) -> bool {
        match self {
            Self::JumpLabel(_)
            | Self::Jump(_)
            | Self::JumpIf(_, _, _, _)
            | Self::Generic(_, _)
            | Self::End
            | Self::Write {
                value: _,
                cell: _,
                index: _,
            } => false,

            Self::Operator(out_name, _, _, _)
            | Self::UnaryOperator(out_name, _, _)
            | Self::Set(out_name, _)
            | Self::GenericMut(_, out_name, _)
            | Self::Read {
                out_name,
                cell: _,
                index: _,
            } => out_name == var_name,

            Self::Print(_) | Self::PrintFlush(_) | Self::WorldPrintFlush(_) => {
                var_name == STRING_BUFFER
            }
        }
    }

    /// Returns true if the instruction could cause the next instruction to be executed to not be `@counter + 1`,
    /// or the previous instruction to be executed to not be `@counter - 1`.
    /// In this case, a jump label should be present at the site of jump, or else the optimizations could break things.
    pub fn breaks_flow(&self) -> bool {
        match self {
            Self::JumpLabel(_) | Self::Jump(_) | Self::JumpIf(_, _, _, _) | Self::End => true,

            Self::Read {
                out_name: _,
                cell: _,
                index: _,
            }
            | Self::Write {
                value: _,
                cell: _,
                index: _,
            }
            | Self::Print(_)
            | Self::PrintFlush(_)
            | Self::WorldPrintFlush(_)
            | Self::Generic(_, _)
            | Self::GenericMut(_, _, _) => false,

            Self::Set(var_name, _)
            | Self::Operator(var_name, _, _, _)
            | Self::UnaryOperator(var_name, _, _) => var_name == "@counter",
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
