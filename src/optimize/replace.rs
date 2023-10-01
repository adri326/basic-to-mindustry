use crate::prelude::*;

pub(crate) fn replace_if<F>(program: MindustryProgram, callback: F) -> MindustryProgram
where
    F: for<'c> Fn(
        &'c Vec<MindustryOperation>,
        &'c MindustryOperation,
        usize,
    ) -> Option<Vec<MindustryOperation>>,
{
    let mut res = MindustryProgram::new();

    for (index, instruction) in program.0.iter().enumerate() {
        match callback(&program.0, instruction, index) {
            Some(mut to_replace) => {
                res.0.append(&mut to_replace);
            }
            None => {
                res.push(instruction.clone());
            }
        }
    }

    res
}
