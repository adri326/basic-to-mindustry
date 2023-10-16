use std::fmt::Write;
use std::{cell::RefCell, collections::HashMap};

use rand::Rng;

use crate::{
    prelude::{Operator, UnaryOperator},
    repr::mlog::*,
};

#[cfg(test)]
mod test;

/// Represents the instruction pointer
#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Debug)]
pub struct Counter(pub usize);

impl From<usize> for Counter {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<Counter> for usize {
    fn from(value: Counter) -> Self {
        value.0
    }
}

impl Counter {
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}

pub struct CompiledProgram<'a> {
    instructions: &'a MindustryProgram,

    /// Points to where the `n`-th (`@counter`-wise) instruction is
    instruction_indices: Vec<usize>,

    /// Contains the different `@counter` values corresponding to each label
    labels: HashMap<String, Counter>,
}

impl<'a> CompiledProgram<'a> {
    pub fn get_instruction(&self, counter: Counter) -> Option<&'a MindustryOperation> {
        let index = self.instruction_indices.get(counter.0)?;
        self.instructions.0.get(*index)
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
#[non_exhaustive]
pub enum Value {
    Number(f64),
    String(String),
    Symbol(String),
    #[default]
    Null,
}

impl From<Value> for f64 {
    fn from(value: Value) -> f64 {
        match value {
            Value::Number(x) => x,
            Value::String(_) => 1.0,
            Value::Symbol(_) => 1.0,
            Value::Null => 0.0,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Symbol(_) => Ok(()),
            Value::Null => Ok(()),
        }
    }
}

#[non_exhaustive]
#[derive(Clone, Debug)]
pub struct ProgramState<R: Rng> {
    pub variables: HashMap<String, Value>,
    pub counter: Counter,

    pub rng: RefCell<R>,

    pub ended: bool,
    // TODO: add errors array
}

#[non_exhaustive]
pub enum StopCondition {
    Steps(usize),
    End,
    Never,
}

impl StopCondition {
    fn should_stop(&self, steps: usize, state: &ProgramState<impl Rng>) -> bool {
        match self {
            Self::Steps(max_steps) => steps >= *max_steps,
            Self::End => state.ended,
            Self::Never => false,
        }
    }
}

fn compile<'a>(program: &'a MindustryProgram) -> CompiledProgram<'a> {
    let mut instruction_indices = Vec::with_capacity(program.0.len());
    let mut labels = HashMap::new();
    let mut current_counter = Counter(0);

    for (index, instr) in program.0.iter().enumerate() {
        match instr {
            MindustryOperation::JumpLabel(label) => {
                labels.insert(label.clone(), current_counter);
            }
            _ => {
                instruction_indices.push(index);

                current_counter.inc();
            }
        }
    }

    instruction_indices.shrink_to_fit();
    CompiledProgram {
        instructions: program,
        instruction_indices,
        labels,
    }
}

pub fn run(program: &MindustryProgram, stop_condition: StopCondition) -> HashMap<String, Value> {
    let compiled = compile(program);

    let mut state = ProgramState {
        counter: Counter(0),
        variables: HashMap::new(),
        rng: RefCell::new(rand::thread_rng()),
        ended: false,
    };
    let mut steps = 0;

    while !stop_condition.should_stop(steps, &state) {
        println!("{:?}", state);
        let _ = step(&compiled, &mut state);
        steps = steps.saturating_add(1);
    }

    state.variables
}

pub fn step(
    program: &CompiledProgram<'_>,
    state: &mut ProgramState<impl Rng>,
) -> Result<(), String> {
    let Some(instruction) = program.get_instruction(state.counter) else {
        state.counter = Counter(0);
        state.ended = true;
        return Ok(())
    };

    match instruction {
        MindustryOperation::JumpLabel(_) => {
            unreachable!("Jump label shouldn't be pointed to by `instruction_indices`");
        }
        MindustryOperation::Jump(label) => {
            if let Some(target) = program.labels.get(label) {
                state.counter = *target;
                return Ok(());
            } else {
                state.counter.inc();
                return Err(format!("Label {} is invalid", label));
            }
        }
        MindustryOperation::JumpIf(label, operation, lhs, rhs) => {
            if let Some(target) = program.labels.get(label) {
                if f64::from(eval_operation(*operation, lhs, rhs, state)) != 0.0f64 {
                    state.counter = *target;
                    return Ok(());
                }
            } else {
                state.counter.inc();
                return Err(format!("Label {} is invalid", label));
            }
        }
        MindustryOperation::Operator(out_name, operation, lhs, rhs) => {
            let result = eval_operation(*operation, lhs, rhs, state);
            state.variables.insert(out_name.clone(), result);
        }
        MindustryOperation::UnaryOperator(out_name, operation, value) => {
            let result = eval_unary_operation(*operation, value, state);
            state.variables.insert(out_name.clone(), result);
        }
        MindustryOperation::Set(out_name, value) => {
            let value = eval_operand(value, state);
            state.variables.insert(out_name.clone(), value);
        }
        MindustryOperation::End => {
            state.counter = Counter(0);
            state.ended = true;
        }
        MindustryOperation::Generic(_, _) => {}
        MindustryOperation::GenericMut(_, name, _) => {
            return Err(format!("Mutating generic operation '{}' encountered", name));
        }
        MindustryOperation::Control(property, operands) => {
            if matches!(property.as_str(), "enabled" | "control" | "config") {
                let Some(Operand::Variable(target)) = operands.get(0) else {
                    return Err(format!("Expected variable name as first operand to Control, got {:?}", operands.get(0)));
                };

                let value = eval_operand(
                    operands
                        .get(1)
                        .ok_or_else(|| String::from("Missing second operand to Control"))?,
                    state,
                );

                state
                    .variables
                    .insert(format!("{}.__{}", target, property), value);
            } else {
                return Err(format!("Control operation {} not supported", property));
            }
        }
        MindustryOperation::Print(value) => {
            let value = eval_operand(value, state);

            if !matches!(state.variables.get("@print"), Some(Value::String(_))) {
                state
                    .variables
                    .insert(String::from("@print"), Value::String(String::new()));
            }
            let Some(Value::String(ref mut buffer)) = state.variables.get_mut("@print") else {
                unreachable!();
            };

            write!(buffer, "{}", value).unwrap_or_else(|_| unreachable!());
        }
        MindustryOperation::PrintFlush(_) => {
            state
                .variables
                .insert(String::from("@print"), Value::String(String::new()));
        }
        _ => unimplemented!(),
    }

    state.counter.inc();

    Ok(())
}

fn eval_operand(operand: &Operand, state: &ProgramState<impl Rng>) -> Value {
    match operand {
        Operand::Variable(name) => {
            if name == "@counter" {
                Value::Number(state.counter.0 as f64)
            } else {
                state.variables.get(name).cloned().unwrap_or_default()
            }
        }
        Operand::String(string) => Value::String(string.clone()),
        Operand::Integer(int) => Value::Number(*int as f64),
        Operand::Float(float) => Value::Number(*float),
    }
}

fn eval_operation(
    op: Operator,
    lhs: &Operand,
    rhs: &Operand,
    state: &ProgramState<impl Rng>,
) -> Value {
    let lhs: f64 = eval_operand(lhs, state).into();
    let rhs: f64 = eval_operand(rhs, state).into();

    Value::Number(match op {
        Operator::Add => lhs + rhs,
        Operator::Sub => lhs - rhs,
        Operator::Mul => lhs * rhs,
        Operator::IDiv => (lhs / rhs).floor(),
        Operator::Div => lhs / rhs,
        Operator::Mod => lhs % rhs,
        Operator::RShift => ((lhs as i64) >> (rhs as i64)) as f64,
        Operator::LShift => ((lhs as i64) << (rhs as i64)) as f64,
        Operator::Gt => (lhs > rhs) as u64 as f64,
        Operator::Lt => (lhs < rhs) as u64 as f64,
        Operator::Gte => (lhs >= rhs) as u64 as f64,
        Operator::Lte => (lhs <= rhs) as u64 as f64,
        Operator::Eq => (lhs == rhs) as u64 as f64,
        Operator::Neq => (lhs != rhs) as u64 as f64,
        Operator::Max => lhs.max(rhs),
        Operator::Min => lhs.min(rhs),
        Operator::Pow => lhs.powf(rhs),
        Operator::And => (lhs != 0.0 && rhs != 0.0) as u64 as f64,
        Operator::Or => (lhs != 0.0 || rhs != 0.0) as u64 as f64,
    })
}

fn eval_unary_operation(
    op: UnaryOperator,
    value: &Operand,
    state: &ProgramState<impl Rng>,
) -> Value {
    let value: f64 = eval_operand(value, state).into();

    Value::Number(match op {
        UnaryOperator::Floor => value.floor(),
        UnaryOperator::Round => value.round(),
        UnaryOperator::Ceil => value.ceil(),
        UnaryOperator::Rand => state.rng.borrow_mut().gen_range(0.0..value),
        UnaryOperator::Sqrt => value.sqrt(),
        UnaryOperator::Not => todo!(),
    })
}
