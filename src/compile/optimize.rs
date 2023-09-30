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

    let instructions = res.0;
    let mut res = MindustryProgram::new();

    // Remove unneeded `set`s
    // PERF: could be split into a search for all variable operands, and a removal of all unneeded `set`s
    for instruction in instructions.iter() {
        let MindustryOperation::Set(set_name, _) = instruction else {
            res.push(instruction.clone());
            continue;
        };

        if !tmp_regex.is_match(set_name) {
            res.push(instruction.clone());
            continue;
        }

        // Note: this will give false positives for temporary variable names that get re-used somewhere else
        let mut needed = false;
        for future_instruction in instructions.iter() {
            if future_instruction.operands().iter().any(|operand| {
                if let Operand::Variable(use_name) = operand {
                    if use_name == set_name {
                        return true;
                    }
                }
                false
            }) {
                needed = true;
                break;
            }
        }

        if needed {
            res.push(instruction.clone());
        }
        // else don't push
    }

    res
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
// - optimize op-jumpif
// - optimize jump(1)-label(2)-...instr-label(1) into ...instr-jump(2)
// - shorten temporary variable names
