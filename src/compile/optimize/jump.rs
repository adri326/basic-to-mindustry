use super::*;

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

                // Find the last operation defining `var_name`
                let Some((_, last_op)) =
                    lookbehind(
                        &*instructions,
                        index,
                        |prev_instruction| match prev_instruction {
                            MindustryOperation::Operator(name, operator, lhs, rhs)
                                if *name == var_name && is_condition_op(*operator) =>
                            {
                                Lookaround::Stop((*operator, lhs.clone(), rhs.clone()))
                            }
                            MindustryOperation::JumpLabel(_) => Lookaround::Abort,
                            x if x.mutates(&var_name) => Lookaround::Abort,
                            _ => Lookaround::Continue,
                        },
                    )
                else {
                    res.push(instruction.clone());
                    continue;
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
            continue;
        };

        if let Some((_, label_to)) =
            lookahead(
                &*instructions,
                index,
                |future_instruction| match future_instruction {
                    MindustryOperation::JumpLabel(_) => Lookaround::Continue,
                    MindustryOperation::Jump(label_to) => Lookaround::Stop(label_to),
                    _ => Lookaround::Abort,
                },
            )
        {
            substitutions.push((label_from.clone(), label_to.clone()));
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
