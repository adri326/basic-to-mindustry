use regex::Regex;

use crate::prelude::*;

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
            let tmp_name = namer.temporary();
            let mut res = translate_expression(value.as_ref(), namer, tmp_name.clone());

            res.push(MindustryOperation::UnaryOperator(
                target_name.clone(),
                *op,
                Operand::Variable(tmp_name),
            ));

            res
        }
    }
}

macro_rules! translate_call {
    (
        $name:expr,
        $arguments:expr,
        $res:expr,
        [ $( $matcher:pat ),* $(,)? ],
        $instruction:expr $(,)?
    ) => {{
        let [ $( $matcher ),* ] = &$arguments[..] else {
            let name_uppercase = String::from($name).to_uppercase();
            panic!("{}(...) called with invalid arguments", name_uppercase);
        };

        let instruction = $instruction;
        $res.push(instruction);
    }}
}

macro_rules! translate_operand {
    (
        $operand:expr,
        $res:expr,
        $namer:expr $(,)?
    ) => {{
        let name = $namer.temporary();
        $res.append(&mut translate_expression($operand, $namer, name.clone()));
        Operand::Variable(name)
    }};
}

pub fn translate_ast(
    basic_ast: &BasicAstBlock,
    namer: &mut Namer,
    config: &Config,
) -> MindustryProgram {
    use crate::repr::basic::BasicAstInstruction as Instr;
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

                if !false_branch.instructions.is_empty() {
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
            Instr::For {
                variable,
                start,
                end,
                step,
                instructions,
            } => {
                let start_label = namer.label("start");
                let end_name = namer.temporary();
                let step_name = namer.temporary();

                // Initialization: evaluate `start`, `end` and `step`
                res.append(&mut translate_expression(start, namer, variable.clone()));
                res.append(&mut translate_expression(end, namer, end_name.clone()));
                res.append(&mut translate_expression(step, namer, step_name.clone()));

                // Body
                res.push(MindustryOperation::JumpLabel(start_label.clone()));
                res.append(&mut translate_ast(instructions, namer, config));

                // Loop condition: increment variable and jump
                res.push(MindustryOperation::Operator(
                    variable.clone(),
                    Operator::Add,
                    Operand::Variable(variable.clone()),
                    Operand::Variable(step_name),
                ));
                res.push(MindustryOperation::JumpIf(
                    start_label,
                    Operator::Lte,
                    Operand::Variable(variable.clone()),
                    Operand::Variable(end_name),
                ));
            }
            Instr::Print(expressions) => {
                for expression in expressions {
                    let tmp_name = namer.temporary();
                    res.append(&mut translate_expression(
                        &expression.0,
                        namer,
                        tmp_name.clone(),
                    ));
                    res.push(MindustryOperation::Print(Operand::Variable(tmp_name)));
                }
            }
            Instr::CallBuiltin(name, arguments) if name == "read" => translate_call!(
                "read",
                arguments,
                res,
                [BasicAstExpression::Variable(out_name), cell, index],
                MindustryOperation::Read {
                    out_name: out_name.clone(),
                    cell: translate_operand!(cell, res, namer),
                    index: translate_operand!(index, res, namer),
                }
            ),
            Instr::CallBuiltin(name, arguments) if name == "write" => translate_call!(
                "write",
                arguments,
                res,
                [value, cell, index],
                MindustryOperation::Write {
                    value: translate_operand!(value, res, namer),
                    cell: translate_operand!(cell, res, namer),
                    index: translate_operand!(index, res, namer),
                }
            ),
            Instr::CallBuiltin(name, arguments) if name == "print_flush" => translate_call!(
                "print_flush",
                arguments,
                res,
                [cell],
                MindustryOperation::PrintFlush(translate_operand!(cell, res, namer))
            ),
            Instr::CallBuiltin(name, arguments) if name == "print_flush_global" => {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    unreachable!("print_flush_global constructed with invalid arguments");
                };

                let instruction = MindustryOperation::WorldPrintFlush(match buffer.as_str() {
                    "mission" => WorldPrintFlush::Mission,
                    "notify" => WorldPrintFlush::Notify,
                    "announce" => {
                        WorldPrintFlush::Announce(translate_operand!(&arguments[1], res, namer))
                    }
                    "toast" => {
                        WorldPrintFlush::Toast(translate_operand!(&arguments[1], res, namer))
                    }
                    _ => unreachable!("print_flush_global constructed with invalid arguments"),
                });

                res.push(instruction);
            }
            Instr::CallBuiltin(name, arguments) => {
                let Some((Some(target_name), mutating, _)) = config.builtin_functions.get(name)
                else {
                    unreachable!("CallBuiltin constructed with unknown function name");
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
                        argument_names.into_iter().map(Operand::Variable).collect(),
                    ));
                } else {
                    res.push(MindustryOperation::Generic(
                        target_name.clone(),
                        argument_names.into_iter().map(Operand::Variable).collect(),
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
