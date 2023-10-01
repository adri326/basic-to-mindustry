use super::*;

/// Optimizes away unnecessary `sets`
pub fn optimize_constant(program: MindustryProgram) -> MindustryProgram {
    let tmp_regex = Regex::new(r"__tmp_[0-9]+$").unwrap();

    // Returns true if the variable is safe to substitute into an operand
    let is_safe_variable = |name: &str| -> bool {
        if matches!(name, "@this" | "@thisx" | "@thisy" | "@links") || is_unit_constant(name) {
            return true;
        }

        if name.starts_with('@') {
            false
        } else {
            true
        }
    };

    let res = replace_if(program, |instructions, instruction, use_index| {
        let optimizable_operands = instruction
            .operands()
            .iter()
            .filter_map(|operand| match operand {
                Operand::Variable(name) if tmp_regex.is_match(name) => Some(name),
                _ => None,
            })
            // PERF: check when it would be better to deduplicate operands
            // .collect::<HashSet<_>>()
            // .into_iter()
            .filter_map(|name| {
                lookbehind(instructions, use_index, |instr| {
                    match instr {
                        MindustryOperation::Set(set_name, value) if set_name == name => {
                            Lookaround::Stop((name.clone(), value.clone()))
                        }
                        MindustryOperation::Operator(op_name, _op, _lhs, _rhs)
                            if op_name == name =>
                        {
                            Lookaround::Abort
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
                            Lookaround::Abort
                        }
                        _ => Lookaround::Continue,
                    }
                })
                .map(|(index, (name, value))| (name, value, index))
            })
            .filter(|(_name, value, set_index)| {
                // Don't optimize operands that refer to a mutating variable (either mutable @-variables or instructions that get updated in-between)
                if let Operand::Variable(assigned_var) = value {
                    if !is_safe_variable(&assigned_var) {
                        return false;
                    }

                    for instr_between in &instructions[*set_index..use_index] {
                        if instr_between.mutates(&assigned_var) {
                            return false;
                        }
                    }

                    true
                } else {
                    true
                }
            })
            .map(|(name, value, _index)| (name, value))
            .collect::<Vec<_>>();

        if optimizable_operands.len() == 0 {
            return None
        }

        let mut instruction = instruction.clone();
        for operand in instruction.operands_mut() {
            if let Operand::Variable(use_name) = operand {
                if let Some((_, optimized_into)) = optimizable_operands
                    .iter()
                    .find(|(set_name, _)| set_name == use_name)
                {
                    *operand = optimized_into.clone();
                }
            }
        }
        Some(vec![instruction])
    });

    optimize_dead_code(res)
}

// TODO: add serpulo units
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
