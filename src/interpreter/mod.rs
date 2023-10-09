use std::collections::HashMap;

use crate::{prelude::Operator, repr::mlog::*};

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

#[non_exhaustive]
pub enum StopCondition {
    Steps(usize),
    Never,
}

impl StopCondition {
    fn should_stop(&self, steps: usize) -> bool {
        match self {
            Self::Steps(max_steps) => steps >= *max_steps,
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
    let mut counter = Counter(0);
    let mut steps = 0;
    let mut variables = HashMap::new();

    while !stop_condition.should_stop(steps) {
        step(&compiled, &mut variables, &mut counter);
        steps = steps.saturating_add(1);
    }

    variables
}

pub fn step(
    program: &CompiledProgram<'_>,
    variables: &mut HashMap<String, Value>,
    counter: &mut Counter,
) -> Result<(), String> {
    let Some(instruction) = program.get_instruction(*counter).or_else(|| {
        *counter = Counter(0);
        program.get_instruction(*counter)
    }) else {
        // Program is empty
        return Err(String::from("Program is empty"));
    };

    match instruction {
        MindustryOperation::JumpLabel(_) => {
            unreachable!("Jump label shouldn't be pointed to by `instruction_indices`");
        }
        MindustryOperation::Jump(label) => {
            if let Some(target) = program.labels.get(label) {
                *counter = *target;
                return Ok(());
            } else {
                counter.inc();
                return Err(format!("Label {} is invalid", label));
            }
        }
        MindustryOperation::JumpIf(label, operation, lhs, rhs) => {
            if let Some(target) = program.labels.get(label) {
                if f64::from(eval_operation(*operation, lhs, rhs, variables, counter)) != 0.0f64 {
                    *counter = *target;
                    return Ok(());
                }
            } else {
                counter.inc();
                return Err(format!("Label {} is invalid", label));
            }
        }
        _ => unimplemented!(),
    }

    Ok(())
}

fn eval_operand(operand: &Operand, variables: &HashMap<String, Value>, counter: &Counter) -> Value {
    match operand {
        Operand::Variable(name) => {
            if name == "@counter" {
                Value::Number(counter.0 as f64)
            } else {
                variables.get(name).cloned().unwrap_or_default()
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
    variables: &mut HashMap<String, Value>,
    counter: &mut Counter,
) -> Value {
    let lhs: f64 = eval_operand(lhs, variables, counter).into();
    let rhs: f64 = eval_operand(rhs, variables, counter).into();

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
