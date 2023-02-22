use std::collections::HashMap;

use crate::{Export, ExportType, Instruction};
use anyhow::bail;

use super::Binary;

pub fn interpret(binary: &Binary, main: &str) -> anyhow::Result<()> {
    let exports: HashMap<_, _> = binary.export.iter().map(|exp| (&*exp.name, exp)).collect();

    let Some(exp) = exports.get(&main) else {
        bail!("function {} is not exported", main)
    };
    let ExportType::Func = exp.tag else {
        bail!("{} is not a function", main)
    };
    let Some(func) = binary.functions.get(exp.index as usize) else {
        bail!("exported function missing")
    };
    let Some(code) = binary.code.get(exp.index as usize) else {
        bail!("exported function missing")
    };

    let mut stack = Stack::default();

    exec(&mut stack, &code.code);

    Ok(())
}

#[derive(Default)]
struct Stack(Vec<i32>);

impl Stack {
    fn push(&mut self, v: i32) {
        self.0.push(v)
    }
    fn pop(&mut self) -> i32 {
        self.0.pop().unwrap()
    }
}

fn exec(stack: &mut Stack, code: &[Instruction]) {
    use Instruction::*;
    for i in code {
        match i {
            Block { typ, expr } => {
                exec(stack, expr);
            }
            End => {}
            I32Const(val) => stack.push(*val),
            other => todo!("interpreter {other:?}"),
        }
    }
}
