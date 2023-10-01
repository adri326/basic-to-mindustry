use regex::Regex;

use crate::common::*;
use crate::parse::{BasicAstBlock, BasicAstExpression};

mod optimize;
pub use optimize::*;

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
                let escaped = string.replace('\\', r#"\\"#).replace('"', r#"\""#);
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
    fn operands<'a>(&'a self) -> Box<[&'a Operand]> {
        match self {
            Self::JumpIf(_label, _operator, lhs, rhs) => Box::new([lhs, rhs]),
            Self::Operator(_target, _operator, lhs, rhs) => Box::new([lhs, rhs]),
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

    fn operands_mut<'a>(&'a mut self) -> Vec<&'a mut Operand> {
        match self {
            Self::JumpIf(_label, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::Operator(_target, _operator, lhs, rhs) => vec![lhs, rhs],
            Self::Set(_target, value) => vec![value],
            Self::Generic(_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            Self::GenericMut(_name, _out_name, operands) => operands.iter_mut().collect::<Vec<_>>(),
            _ => vec![],
        }
    }

    fn mutates(&self, var_name: &str) -> bool {
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

#[derive(Debug, Clone)]
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

pub struct Namer {
    var_index: usize,
    label_index: usize,
    prefix: String,
}

impl Default for Namer {
    fn default() -> Self {
        Self {
            var_index: 0,
            label_index: 0,
            prefix: String::from("main"),
        }
    }
}

impl Namer {
    fn temporary(&mut self) -> String {
        let res = format!("{}__tmp_{}", self.prefix, self.var_index);
        self.var_index += 1;
        res
    }

    fn label(&mut self, suffix: &str) -> String {
        let res = format!("{}__label_{}_{}", self.prefix, self.label_index, suffix);
        self.label_index += 1;
        res
    }
}

fn translate_expression(
    expression: &BasicAstExpression,
    namer: &mut Namer,
    target_name: String,
) -> MindustryProgram {
    match expression {
        BasicAstExpression::Integer(int) => {
            vec![MindustryOperation::Set(target_name, Operand::Integer(*int))].into()
        }
        BasicAstExpression::Float(float) => {
            vec![MindustryOperation::Set(target_name, Operand::Float(*float))].into()
        }
        BasicAstExpression::Variable(name) => vec![MindustryOperation::Set(
            target_name,
            Operand::Variable(name.clone()),
        )]
        .into(),
        BasicAstExpression::String(string) => vec![MindustryOperation::Set(
            target_name,
            Operand::String(string.clone()),
        )]
        .into(),
        BasicAstExpression::Binary(op, left, right) => {
            let left_name = namer.temporary();
            let right_name = namer.temporary();

            let mut res = translate_expression(left.as_ref(), namer, left_name.clone());
            let mut right = translate_expression(right.as_ref(), namer, right_name.clone());

            res.append(&mut right);
            res.push(MindustryOperation::Operator(
                target_name,
                *op,
                Operand::Variable(left_name),
                Operand::Variable(right_name),
            ));

            res
        }
        BasicAstExpression::Unary(op, value) => {
            let mut res = translate_expression(value.as_ref(), namer, target_name.clone());

            res.push(MindustryOperation::UnaryOperator(
                target_name.clone(),
                *op,
                Operand::Variable(target_name),
            ));

            res
        }
    }
}

pub fn translate_ast(
    basic_ast: &BasicAstBlock,
    namer: &mut Namer,
    config: &Config,
) -> MindustryProgram {
    use crate::parse::BasicAstInstruction as Instr;
    let mut res = MindustryProgram::new();

    for instruction in basic_ast.instructions.iter() {
        match instruction {
            Instr::JumpLabel(label) => {
                res.push(MindustryOperation::JumpLabel(label.clone()));
            }
            Instr::Jump(to) => {
                res.push(MindustryOperation::Jump(to.clone()));
            }
            Instr::Assign(name, expression) => {
                let mut instructions = translate_expression(expression, namer, name.clone());
                res.append(&mut instructions);
            }
            Instr::IfThenElse(condition, true_branch, false_branch) => {
                let condition_name = namer.temporary();
                res.append(&mut translate_expression(
                    condition,
                    namer,
                    condition_name.clone(),
                ));

                if false_branch.instructions.len() > 0 {
                    let else_label = namer.label("else");
                    let end_label = namer.label("endif");
                    res.push(MindustryOperation::JumpIf(
                        else_label.clone(),
                        Operator::Neq,
                        Operand::Variable(condition_name),
                        Operand::Variable(String::from("true")),
                    ));

                    res.append(&mut translate_ast(true_branch, namer, config));

                    res.push(MindustryOperation::Jump(end_label.clone()));
                    res.push(MindustryOperation::JumpLabel(else_label));

                    res.append(&mut translate_ast(false_branch, namer, config));

                    res.push(MindustryOperation::JumpLabel(end_label));
                } else {
                    let end_label = namer.label("endif");
                    res.push(MindustryOperation::JumpIf(
                        end_label.clone(),
                        Operator::Neq,
                        Operand::Variable(condition_name),
                        Operand::Variable(String::from("true")),
                    ));

                    res.append(&mut translate_ast(true_branch, namer, config));

                    res.push(MindustryOperation::JumpLabel(end_label));
                }
            }
            Instr::Print(expressions) => {
                for expression in expressions {
                    let tmp_name = namer.temporary();
                    res.append(&mut translate_expression(
                        &expression.0,
                        namer,
                        tmp_name.clone(),
                    ));
                    res.push(MindustryOperation::Generic(
                        String::from("print"),
                        vec![Operand::Variable(tmp_name)],
                    ));
                }
            }
            Instr::CallBuiltin(name, arguments) => {
                let Some((target_name, mutating, _)) = config.builtin_functions.get(name) else {
                    unreachable!("CallBuilting constructed with unknown function name");
                };
                let mutating = *mutating;

                let first_index = mutating as usize;

                let argument_names = (first_index..arguments.len())
                    .map(|_| namer.temporary())
                    .collect::<Vec<_>>();
                for (i, argument) in arguments.iter().skip(first_index).enumerate() {
                    res.append(&mut translate_expression(
                        argument,
                        namer,
                        argument_names[i].clone(),
                    ));
                }

                if mutating {
                    let BasicAstExpression::Variable(out_name) = arguments[0].clone() else {
                        unreachable!(
                            "First argument to {} isn't a variable, got {:?}",
                            name, arguments[0]
                        );
                    };

                    res.push(MindustryOperation::GenericMut(
                        target_name.clone(),
                        out_name,
                        argument_names
                            .into_iter()
                            .map(|name| Operand::Variable(name))
                            .collect(),
                    ));
                } else {
                    res.push(MindustryOperation::Generic(
                        target_name.clone(),
                        argument_names
                            .into_iter()
                            .map(|name| Operand::Variable(name))
                            .collect(),
                    ));
                }
            }
        }
    }

    res
}

impl std::fmt::Display for MindustryProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let numeric = Regex::new(r"^[0-9]+$").unwrap();
        let numeric_jumps =
            self.0
                .iter()
                .filter_map(|op| match op {
                    MindustryOperation::Jump(target)
                    | MindustryOperation::JumpIf(target, _, _, _) => Some(target),
                    _ => None,
                })
                .filter(|target| numeric.is_match(target))
                .cloned()
                .collect::<Vec<_>>();

        for instruction in self.0.iter() {
            match instruction {
                MindustryOperation::JumpLabel(label) => {
                    if numeric.is_match(label) {
                        if numeric_jumps.contains(label) {
                            writeln!(f, "line__{}:", label)?;
                        }
                    } else {
                        writeln!(f, "{}:", label)?;
                    }
                }
                MindustryOperation::Jump(label) => {
                    if numeric.is_match(label) {
                        writeln!(f, "jump line__{} always 0 0", label)?;
                    } else {
                        writeln!(f, "jump {} always 0 0", label)?;
                    }
                }
                MindustryOperation::JumpIf(label, operator, lhs, rhs) => {
                    if numeric.is_match(label) {
                        writeln!(
                            f,
                            "jump line__{} {} {} {}",
                            label,
                            format_condition(*operator),
                            lhs,
                            rhs
                        )?;
                    } else {
                        writeln!(
                            f,
                            "jump {} {} {} {}",
                            label,
                            format_condition(*operator),
                            lhs,
                            rhs
                        )?;
                    }
                }
                MindustryOperation::Operator(name, operator, lhs, rhs) => {
                    writeln!(
                        f,
                        "op {} {} {} {}",
                        format_operator(*operator),
                        name,
                        lhs,
                        rhs
                    )?;
                }
                MindustryOperation::UnaryOperator(name, operator, lhs) => {
                    writeln!(
                        f,
                        "op {} {} {} 0",
                        format_unary_operator(*operator),
                        name,
                        lhs
                    )?;
                }
                MindustryOperation::Set(name, value) => {
                    writeln!(f, "set {} {}", name, value)?;
                }
                MindustryOperation::Generic(name, operands) => {
                    write!(f, "{}", name)?;
                    for operand in operands {
                        write!(f, " {}", operand)?;
                    }
                    write!(f, "\n")?;
                }
                MindustryOperation::GenericMut(name, out_name, operands) => {
                    write!(f, "{}", name)?;
                    write!(f, " {}", out_name)?;
                    for operand in operands {
                        write!(f, " {}", operand)?;
                    }
                    write!(f, "\n")?;
                }
            }
        }

        Ok(())
    }
}

fn format_condition(operator: Operator) -> &'static str {
    match operator {
        Operator::Eq => "equal",
        Operator::Neq => "notEqual",
        Operator::Lt => "lessThan",
        Operator::Lte => "lessThanEqual",
        Operator::Gt => "greaterThan",
        Operator::Gte => "greaterThanEqual",
        x => {
            panic!("Operator {:?} is not a condition!", x);
        }
    }
}
