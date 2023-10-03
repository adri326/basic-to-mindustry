use std::collections::HashSet;

use super::*;
use crate::prelude::*;

pub(crate) fn optimize_dead_code(program: MindustryProgram) -> MindustryProgram {
    let mut needed_vars = HashSet::new();
    let mut needed_labels = HashSet::new();
    let mut push_var = |operand: &Operand| {
        if let Operand::Variable(name) = operand {
            needed_vars.insert(name.clone());
        }
    };

    for instruction in program.0.iter() {
        match instruction {
            MindustryOperation::JumpLabel(_) => {}
            MindustryOperation::Jump(label) => {
                needed_labels.insert(label.clone());
            }
            MindustryOperation::JumpIf(label, _, lhs, rhs) => {
                needed_labels.insert(label.clone());
                push_var(lhs);
                push_var(rhs);
            }
            other => other.operands().iter().copied().for_each(&mut push_var),
        }
    }

    // Remove unneeded `set`s and `op`s
    replace_if(
        program,
        |_instructions, instruction, _index| match instruction {
            MindustryOperation::Set(name, _) | MindustryOperation::Operator(name, _, _, _) => {
                if !is_temporary_variable(name) {
                    return None;
                }

                if needed_vars.contains(name) {
                    return None;
                }

                Some(vec![])
            }
            MindustryOperation::JumpLabel(label) => {
                if !is_automatic_label(label) {
                    return None;
                }

                if needed_labels.contains(label) {
                    return None;
                }

                Some(vec![])
            }
            _ => None,
        },
    )
}
