use super::*;

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
                MindustryOperation::End => {
                    writeln!(f, "end")?;
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
                MindustryOperation::Print(value) => {
                    writeln!(f, "print {}", value)?;
                }
                MindustryOperation::Read {
                    out_name,
                    cell,
                    index,
                } => {
                    writeln!(f, "read {} {} {}", out_name, cell, index)?;
                }
                MindustryOperation::Write { value, cell, index } => {
                    writeln!(f, "write {} {} {}", value, cell, index)?;
                }
                MindustryOperation::PrintFlush(cell) => {
                    writeln!(f, "printflush {}", cell)?;
                }
                MindustryOperation::Generic(name, operands) => {
                    write!(f, "{}", name)?;
                    for operand in operands {
                        write!(f, " {}", operand)?;
                    }
                    writeln!(f)?;
                }
                MindustryOperation::GenericMut(name, out_name, operands) => {
                    write!(f, "{}", name)?;
                    write!(f, " {}", out_name)?;
                    for operand in operands {
                        write!(f, " {}", operand)?;
                    }
                    writeln!(f)?;
                }
                MindustryOperation::WorldPrintFlush(config) => {
                    match config {
                        WorldPrintFlush::Notify => writeln!(f, "message notify")?,
                        WorldPrintFlush::Mission => writeln!(f, "message mission")?,
                        WorldPrintFlush::Announce(time) => {
                            writeln!(f, "message announce {}", time)?
                        }
                        WorldPrintFlush::Toast(time) => writeln!(f, "message toast {}", time)?,
                    };
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
