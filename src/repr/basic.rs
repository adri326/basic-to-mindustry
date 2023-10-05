use crate::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum BasicAstExpression {
    Integer(i64),
    Float(f64),
    Variable(String),
    String(String),
    Binary(Operator, Box<BasicAstExpression>, Box<BasicAstExpression>),
    Unary(UnaryOperator, Box<BasicAstExpression>),
    BuiltinFunction(String, Vec<BasicAstExpression>),
}

/// The set of keys for `control` that accept one operand, and otherwise a key for `setprop`
#[derive(Clone, Debug, PartialEq)]
pub enum SetPropOrControlKey {
    ControlEnabled,
    ControlConfig,
    ControlColor,
    SetProp(String),
}

impl From<&str> for SetPropOrControlKey {
    fn from(value: &str) -> Self {
        match value {
            "enabled" => Self::ControlEnabled,
            "config" => Self::ControlConfig,
            "color" => Self::ControlColor,
            other => Self::SetProp(format!("@{}", other)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BasicAstInstruction {
    JumpLabel(String),
    /// Immediately and always jump to the label or line number stored
    Jump(String),
    /// Stores the return address, and jumps to the given label
    GoSub(String),
    /// Reads the return address and jumps to it
    Return,
    /// Stops the program, returning to the starting point
    End,
    Assign(String, BasicAstExpression),
    IfThenElse(BasicAstExpression, BasicAstBlock, BasicAstBlock),
    Print(Vec<(BasicAstExpression, bool)>),
    CallBuiltin(String, Vec<BasicAstExpression>),
    For {
        variable: String,
        start: BasicAstExpression,
        end: BasicAstExpression,
        step: BasicAstExpression,
        instructions: BasicAstBlock,
    },
    While(BasicAstExpression, BasicAstBlock),
    DoWhile(BasicAstExpression, BasicAstBlock),
    SetPropOrControl(SetPropOrControlKey, BasicAstExpression, BasicAstExpression),
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct BasicAstBlock {
    pub instructions: Vec<BasicAstInstruction>,
}

impl BasicAstBlock {
    pub fn new(instructions: impl IntoIterator<Item = BasicAstInstruction>) -> Self {
        Self {
            instructions: instructions.into_iter().collect(),
        }
    }
}
