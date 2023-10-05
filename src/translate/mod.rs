use regex::Regex;

use crate::prelude::*;

mod display;

const MAX_INSTRUCTION_COUNT: usize = 1000;

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
    pub fn temporary(&mut self) -> String {
        let res = format!("{}__tmp_{}", self.prefix, self.var_index);
        self.var_index += 1;
        res
    }

    pub fn label(&mut self, suffix: &str) -> String {
        let res = format!("{}__label_{}_{}", self.prefix, self.label_index, suffix);
        self.label_index += 1;
        res
    }
}

pub(crate) fn translate_expression(
    expression: &BasicAstExpression,
    namer: &mut Namer,
    target_name: String,
    config: &Config,
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

            let mut res = translate_expression(left.as_ref(), namer, left_name.clone(), config);
            let mut right = translate_expression(right.as_ref(), namer, right_name.clone(), config);

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
            let mut res = translate_expression(value.as_ref(), namer, tmp_name.clone(), config);

            res.push(MindustryOperation::UnaryOperator(
                target_name.clone(),
                *op,
                Operand::Variable(tmp_name),
            ));

            res
        }
        BasicAstExpression::BuiltinFunction(name, arguments) => {
            let Some(fn_config) = config.builtin_functions.get(name) else {
                unreachable!("Builtin function {} not found", name);
            };

            fn_config.translate(arguments, namer, config, target_name)
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
        $namer:expr,
        $config:expr $(,)?
    ) => {{
        let name = $namer.temporary();
        $res.append(&mut translate_expression(
            $operand,
            $namer,
            name.clone(),
            $config,
        ));
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
    let mut has_return = false;

    for instruction in basic_ast.instructions.iter() {
        match instruction {
            Instr::JumpLabel(label) => {
                res.push(MindustryOperation::JumpLabel(label.clone()));
            }
            Instr::Jump(to) => {
                res.push(MindustryOperation::Jump(to.clone()));
            }
            Instr::GoSub(to) => {
                let return_label = namer.label("return__phantom");
                res.push(MindustryOperation::Operator(
                    String::from("__gosub_retaddr"),
                    Operator::Mul,
                    Operand::Variable(String::from("__gosub_retaddr")),
                    Operand::Integer(MAX_INSTRUCTION_COUNT as i64),
                ));
                res.push(MindustryOperation::Operator(
                    String::from("__gosub_retaddr"),
                    Operator::Add,
                    Operand::Variable(String::from("__gosub_retaddr")),
                    Operand::Variable(String::from("@counter")),
                ));
                res.push(MindustryOperation::Jump(to.clone()));
                res.push(MindustryOperation::JumpLabel(return_label));
            }
            Instr::Return => {
                res.push(MindustryOperation::Operator(
                    String::from("__return"),
                    Operator::Mod,
                    Operand::Variable(String::from("__gosub_retaddr")),
                    Operand::Integer(MAX_INSTRUCTION_COUNT as i64),
                ));
                res.push(MindustryOperation::Operator(
                    String::from("__gosub_retaddr"),
                    Operator::IDiv,
                    Operand::Variable(String::from("__gosub_retaddr")),
                    Operand::Integer(MAX_INSTRUCTION_COUNT as i64),
                ));
                res.push(MindustryOperation::Operator(
                    String::from("@counter"),
                    Operator::Add,
                    Operand::Variable(String::from("__return")),
                    Operand::Integer(1),
                ));
                // Add a guard at the beginning of the program, to clear out the return address
                has_return = true;
            }
            Instr::End => {
                res.push(MindustryOperation::End);
            }
            Instr::Assign(name, expression) => {
                let mut instructions =
                    translate_expression(expression, namer, name.clone(), config);
                res.append(&mut instructions);
            }
            Instr::IfThenElse(condition, true_branch, false_branch) => {
                let condition_name = namer.temporary();
                res.append(&mut translate_expression(
                    condition,
                    namer,
                    condition_name.clone(),
                    config,
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
                let end_label = namer.label("end");
                let end_name = namer.temporary();
                let step_name = namer.temporary();

                // Initialization: evaluate `start`, `end` and `step`
                res.append(&mut translate_expression(
                    start,
                    namer,
                    variable.clone(),
                    config,
                ));
                res.append(&mut translate_expression(
                    end,
                    namer,
                    end_name.clone(),
                    config,
                ));
                res.append(&mut translate_expression(
                    step,
                    namer,
                    step_name.clone(),
                    config,
                ));

                // Condition
                res.push(MindustryOperation::JumpLabel(start_label.clone()));
                res.push(MindustryOperation::JumpIf(
                    end_label.clone(),
                    Operator::Gt,
                    Operand::Variable(variable.clone()),
                    Operand::Variable(end_name),
                ));

                // Body
                res.append(&mut translate_ast(instructions, namer, config));

                // Increment variable and jump
                res.push(MindustryOperation::Operator(
                    variable.clone(),
                    Operator::Add,
                    Operand::Variable(variable.clone()),
                    Operand::Variable(step_name),
                ));
                res.push(MindustryOperation::Jump(start_label));
                res.push(MindustryOperation::JumpLabel(end_label));
            }
            Instr::While(condition, instructions) => {
                let start_label = namer.label("start");
                let end_label = namer.label("end");
                let condition_name = namer.temporary();

                // Loop condition
                res.push(MindustryOperation::JumpLabel(start_label.clone()));
                res.append(&mut translate_expression(
                    condition,
                    namer,
                    condition_name.clone(),
                    config,
                ));
                res.push(MindustryOperation::JumpIf(
                    end_label.clone(),
                    Operator::Eq,
                    Operand::Variable(condition_name),
                    Operand::Variable(String::from("false")),
                ));

                // Loop body
                res.append(&mut translate_ast(instructions, namer, config));

                // Loop end
                res.push(MindustryOperation::Jump(start_label));
                res.push(MindustryOperation::JumpLabel(end_label));
            }
            Instr::DoWhile(condition, instructions) => {
                let start_label = namer.label("start");
                let condition_name = namer.temporary();

                // Loop start
                res.push(MindustryOperation::JumpLabel(start_label.clone()));

                // Loop body
                res.append(&mut translate_ast(instructions, namer, config));

                // Loop condition
                res.append(&mut translate_expression(
                    condition,
                    namer,
                    condition_name.clone(),
                    config,
                ));
                res.push(MindustryOperation::JumpIf(
                    start_label,
                    Operator::Eq,
                    Operand::Variable(condition_name),
                    Operand::Variable(String::from("true")),
                ));
            }
            Instr::Print(expressions) => {
                for expression in expressions {
                    let tmp_name = namer.temporary();
                    res.append(&mut translate_expression(
                        &expression.0,
                        namer,
                        tmp_name.clone(),
                        config,
                    ));
                    res.push(MindustryOperation::Print(Operand::Variable(tmp_name)));
                }
            }
            Instr::SetPropOrControl(key, object, value) => {
                let object_name = namer.temporary();
                res.append(&mut translate_expression(
                    object,
                    namer,
                    object_name.clone(),
                    config,
                ));
                let value_name = namer.temporary();
                res.append(&mut translate_expression(
                    value,
                    namer,
                    value_name.clone(),
                    config,
                ));

                match key {
                    SetPropOrControlKey::ControlEnabled
                    | SetPropOrControlKey::ControlConfig
                    | SetPropOrControlKey::ControlColor => {
                        let key = match key {
                            SetPropOrControlKey::ControlEnabled => String::from("enabled"),
                            SetPropOrControlKey::ControlConfig => String::from("config"),
                            SetPropOrControlKey::ControlColor => String::from("color"),
                            _ => unreachable!(),
                        };

                        res.push(MindustryOperation::Control(
                            key,
                            vec![
                                Operand::Variable(object_name),
                                Operand::Variable(value_name),
                            ],
                        ));
                    }
                    SetPropOrControlKey::SetProp(key) => {
                        res.push(MindustryOperation::WorldSetProp {
                            key: Operand::Variable(key.clone()),
                            object: Operand::Variable(object_name),
                            value: Operand::Variable(value_name),
                        });
                    }
                }
            }
            Instr::CallBuiltin(name, arguments) if name == "read" => translate_call!(
                "read",
                arguments,
                res,
                [BasicAstExpression::Variable(out_name), cell, index],
                MindustryOperation::Read {
                    out_name: out_name.clone(),
                    cell: translate_operand!(cell, res, namer, config),
                    index: translate_operand!(index, res, namer, config),
                }
            ),
            Instr::CallBuiltin(name, arguments) if name == "write" => translate_call!(
                "write",
                arguments,
                res,
                [value, cell, index],
                MindustryOperation::Write {
                    value: translate_operand!(value, res, namer, config),
                    cell: translate_operand!(cell, res, namer, config),
                    index: translate_operand!(index, res, namer, config),
                }
            ),
            Instr::CallBuiltin(name, arguments) if name == "print_flush" => translate_call!(
                "print_flush",
                arguments,
                res,
                [cell],
                MindustryOperation::PrintFlush(translate_operand!(cell, res, namer, config))
            ),
            Instr::CallBuiltin(name, arguments) if name == "print_flush_global" => {
                let BasicAstExpression::Variable(buffer) = &arguments[0] else {
                    unreachable!("print_flush_global constructed with invalid arguments");
                };

                let instruction = MindustryOperation::WorldPrintFlush(match buffer.as_str() {
                    "mission" => WorldPrintFlush::Mission,
                    "notify" => WorldPrintFlush::Notify,
                    "announce" => WorldPrintFlush::Announce(translate_operand!(
                        &arguments[1],
                        res,
                        namer,
                        config
                    )),
                    "toast" => WorldPrintFlush::Toast(translate_operand!(
                        &arguments[1],
                        res,
                        namer,
                        config
                    )),
                    _ => unreachable!("print_flush_global constructed with invalid arguments"),
                });

                res.push(instruction);
            }
            Instr::CallBuiltin(name, arguments) if name == "control" => translate_call!(
                "control",
                arguments,
                res,
                [BasicAstExpression::Variable(key), ..],
                MindustryOperation::Control(
                    key.clone(),
                    arguments
                        .iter()
                        .skip(1)
                        .map(|arg| translate_operand!(arg, res, namer, config))
                        .collect()
                )
            ),
            Instr::CallBuiltin(name, arguments) => {
                let Some((Some(target_name), mutating, _)) = config.builtin_routines.get(name)
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
                        config,
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

    if has_return {
        res.0.insert(
            0,
            MindustryOperation::Set(String::from("__gosub_retaddr"), Operand::Integer(0)),
        );
    }

    res
}
