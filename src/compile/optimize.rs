use std::collections::HashSet;

use super::*;

/// Optimizes away unnecessary `sets`
pub fn optimize_set_use(program: MindustryProgram) -> MindustryProgram {
    let mut res = MindustryProgram::new();
    let instructions = program.0;
    let tmp_regex = Regex::new(r"__tmp_[0-9]+$").unwrap();

    // Find and replace references to constants
    // TODO: multiple rounds?
    for (use_index, instruction) in instructions.iter().enumerate() {
        let optimizable_operands = instruction
            .operands()
            .iter()
            .filter_map(|operand| match operand {
                Operand::Variable(name) => {
                    if tmp_regex.is_match(name) {
                        Some(name)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            // PERF: check when it would be better to deduplicate operands
            // .collect::<HashSet<_>>()
            // .into_iter()
            .filter_map(|name| {
                for (index, instr) in instructions[0..use_index].iter().enumerate().rev() {
                    match instr {
                        MindustryOperation::Set(set_name, value) if set_name == name => {
                            return Some((name.clone(), value.clone(), index))
                        }
                        MindustryOperation::Operator(op_name, _op, _lhs, _rhs)
                            if op_name == name =>
                        {
                            // Not optimizable
                            break;
                        }
                        MindustryOperation::JumpLabel(_label) => {
                            // Note: jump labels mark boundaries for constants. For instance:
                            // ```
                            // set __tmp_1 "this is not a constant"
                            // jump_label:
                            // set __tmp_2 "this is a constant"
                            // op add result __tmp_1 __tmp_2
                            // ```
                            //
                            // gets optimized to:
                            // ```
                            // set __tmp_1 "this is not a constant"
                            // jump_label:
                            // op add result __tmp_1 "this is a constant"
                            // ```
                            //
                            // A more complex algorithm could be used to check the flow of the program,
                            // but this usecase isn't needed yet.
                            break;
                        }
                        _ => {}
                    }
                }
                None
            })
            .filter(|(_name, value, set_index)| {
                // Don't optimize operands that refer to a mutating variable (either mutable @-variables or instructions that get updated in-between)
                if let Operand::Variable(assigned_var) = value {
                    if matches!(
                        assigned_var.as_str(),
                        "@this" | "@thisx" | "@thisy" | "@links"
                    ) || is_unit_constant(assigned_var.as_str())
                    {
                        return true;
                    }
                    if assigned_var.starts_with('@') {
                        return false;
                    }
                    for instr_between in &instructions[*set_index..use_index] {
                        match instr_between {
                            MindustryOperation::Set(var_name, _)
                            | MindustryOperation::Operator(var_name, _, _, _) => {
                                if var_name == assigned_var {
                                    return false;
                                }
                            }
                            _ => {}
                        }
                    }

                    true
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();

        if optimizable_operands.len() > 0 {
            let mut instruction = instruction.clone();
            for operand in instruction.operands_mut() {
                if let Operand::Variable(use_name) = operand {
                    if let Some((_, optimized_into, _)) = optimizable_operands
                        .iter()
                        .find(|(set_name, _, _)| set_name == use_name)
                    {
                        *operand = optimized_into.clone();
                    }
                }
            }
            res.push(instruction);
        } else {
            res.push(instruction.clone());
        }
    }

    optimize_dead_code(res)
}

fn is_unit_constant(name: &str) -> bool {
    matches!(
        name,
        "@stell"
            | "@locus"
            | "@precept"
            | "@vanquish"
            | "@conquer"
            | "@merui"
            | "@cleroi"
            | "@anthicus"
            | "@tecta"
            | "@collaris"
            | "@elude"
            | "@avert"
            | "@obviate"
            | "@quell"
            | "@disrupt"
    )
}

// TODO:
// - optimize jump(1)-label(2)-...instr-label(1) into ...instr-jump(2)
// - shorten temporary variable names

/// Tries to merge the condition in an `op` into the `jump` itself
pub fn optimize_jump_op(program: MindustryProgram) -> MindustryProgram {
    let tmp_regex = Regex::new(r"__tmp_[0-9]+$").unwrap();

    let mut res = MindustryProgram::new();
    let instructions = program.0;

    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            MindustryOperation::JumpIf(label, operator, lhs, rhs) => {
                let (truthiness, var_name) = match (
                    operator,
                    replace_constants(lhs.clone()),
                    replace_constants(rhs.clone()),
                ) {
                    (Operator::Neq, Operand::Variable(var_name), Operand::Integer(0))
                    | (Operator::Eq, Operand::Variable(var_name), Operand::Integer(1))
                    | (Operator::Neq, Operand::Integer(0), Operand::Variable(var_name))
                    | (Operator::Eq, Operand::Integer(1), Operand::Variable(var_name)) => {
                        (true, var_name)
                    }
                    (Operator::Eq, Operand::Variable(var_name), Operand::Integer(0))
                    | (Operator::Neq, Operand::Variable(var_name), Operand::Integer(1))
                    | (Operator::Eq, Operand::Integer(0), Operand::Variable(var_name))
                    | (Operator::Neq, Operand::Integer(1), Operand::Variable(var_name)) => {
                        (false, var_name)
                    }
                    _ => {
                        res.push(instruction.clone());
                        continue;
                    }
                };

                if !tmp_regex.is_match(&var_name) {
                    res.push(instruction.clone());
                    continue;
                }

                let mut last_op = None;
                for prev_instruction in instructions[0..=index].iter().rev().skip(1) {
                    match prev_instruction {
                        MindustryOperation::Operator(name, operator, lhs, rhs)
                            if *name == var_name && is_condition_op(*operator) =>
                        {
                            last_op = Some((*operator, lhs.clone(), rhs.clone()));
                        }
                        MindustryOperation::JumpLabel(_) => break,
                        _ => {}
                    }
                }

                let Some(last_op) = last_op else {
                    res.push(instruction.clone());
                    continue
                };

                let (operator, lhs, rhs) = if truthiness {
                    last_op
                } else {
                    (
                        match last_op.0 {
                            Operator::Gt => Operator::Lte,
                            Operator::Lt => Operator::Gte,
                            Operator::Gte => Operator::Lt,
                            Operator::Lte => Operator::Gt,
                            Operator::Eq => Operator::Neq,
                            Operator::Neq => Operator::Eq,
                            _ => unreachable!(),
                        },
                        last_op.1,
                        last_op.2,
                    )
                };

                res.push(MindustryOperation::JumpIf(
                    label.clone(),
                    operator,
                    lhs,
                    rhs,
                ));
            }
            _ => {
                res.push(instruction.clone());
            }
        }
    }

    return optimize_dead_code(res);

    fn replace_constants(value: Operand) -> Operand {
        if let Operand::Variable(var) = &value {
            match var.as_str() {
                "true" => Operand::Integer(1),
                "false" | "null" => Operand::Integer(0),
                _ => value,
            }
        } else {
            value
        }
    }

    fn is_condition_op(op: Operator) -> bool {
        matches!(
            op,
            Operator::Neq
                | Operator::Eq
                | Operator::Lt
                | Operator::Lte
                | Operator::Gt
                | Operator::Gte
        )
    }
}

/// Tries to remove unnecessary `jump always` instructions
pub fn optimize_jump_always(mut program: MindustryProgram) -> MindustryProgram {
    let instructions = &mut program.0;

    let mut substitutions = Vec::new();

    // Detect `label`-`jump always` pairs
    for (index, instruction) in instructions.iter().enumerate() {
        let MindustryOperation::JumpLabel(label_from) = instruction else {
            continue
        };

        for future_instruction in instructions[index..].iter() {
            match future_instruction {
                MindustryOperation::JumpLabel(_) => {}
                MindustryOperation::Jump(label_to) => {
                    substitutions.push((label_from.clone(), label_to.clone()));
                    break;
                }
                _ => break,
            }
        }
    }

    // Apply transitivity to the pairs
    let substitutions = substitutions
        .iter()
        .map(|(from, to)| {
            let mut new_to = to;
            let mut history = vec![to];

            loop {
                let mut found = false;

                for (other_from, other_to) in substitutions.iter() {
                    if other_from == new_to {
                        // Leave cycles untouched
                        if history.contains(&other_to) {
                            return (from.clone(), to.clone());
                        }
                        new_to = other_to;
                        history.push(other_to);
                        found = true;
                        break;
                    }
                }

                if !found {
                    break;
                }
            }

            (from.clone(), to.clone())
        })
        .collect::<Vec<_>>();

    for instruction in instructions.iter_mut() {
        match instruction {
            MindustryOperation::Jump(label) => {
                if let Some((_, new_label)) = substitutions.iter().find(|(from, _)| from == label) {
                    *label = new_label.clone();
                }
            }
            MindustryOperation::JumpIf(label, _, _, _) => {
                if let Some((_, new_label)) = substitutions.iter().find(|(from, _)| from == label) {
                    *label = new_label.clone();
                }
            }
            _ => {}
        }
    }

    optimize_dead_code(program)
}

fn optimize_dead_code(program: MindustryProgram) -> MindustryProgram {
    let instructions = program.0;
    let tmp_regex = Regex::new(r"__tmp_[0-9]+$").unwrap();
    let label_regex = Regex::new(r"__label_[0-9]+").unwrap();
    let mut res = MindustryProgram::new();

    let mut needed_vars = HashSet::new();
    let mut needed_labels = HashSet::new();
    let mut push_var = |operand: &Operand| match operand {
        Operand::Variable(name) => {
            needed_vars.insert(name.clone());
        }
        _ => {}
    };

    for instruction in instructions.iter() {
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
            MindustryOperation::Operator(_, _, lhs, rhs) => {
                push_var(lhs);
                push_var(rhs);
            }
            MindustryOperation::UnaryOperator(_, _, value) => {
                push_var(value);
            }
            MindustryOperation::Set(_, value) => {
                push_var(value);
            }
            MindustryOperation::Generic(_, values) => {
                values.iter().for_each(&mut push_var);
            }
        }
    }

    // Remove unneeded `set`s and `op`s
    for instruction in instructions.iter() {
        match instruction {
            MindustryOperation::Set(name, _) | MindustryOperation::Operator(name, _, _, _) => {
                if tmp_regex.is_match(name) {
                    if needed_vars.contains(name) {
                        res.push(instruction.clone());
                    }
                    // else don't push
                } else {
                    res.push(instruction.clone());
                }
            }
            MindustryOperation::JumpLabel(label) => {
                if label_regex.is_match(label) {
                    if needed_labels.contains(label) {
                        res.push(instruction.clone());
                    }
                    // else don't push
                } else {
                    res.push(instruction.clone());
                }
            }
            _ => {
                res.push(instruction.clone());
                continue;
            }
        };
    }

    res
}
