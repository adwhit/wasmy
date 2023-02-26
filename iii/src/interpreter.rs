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
    let Some(func_ix) = binary.functions.get(exp.index as usize) else {
        bail!("exported function missing")
    };
    let Some(func_sig) = binary.types.get(*func_ix as usize) else {
        bail!("exported function missing")
    };
    let Some(code) = binary.code.get(exp.index as usize) else {
        bail!("exported function missing")
    };

    let n_locals =
        func_sig.params.len() + code.locals.iter().fold(0u32, |acc, (ct, _)| acc + ct) as usize;
    let mut state = State::with_n_locals(n_locals);

    exec(binary, &mut state, &code.code);

    while let Some(v) = state.try_pop() {
        println!("Value on stack: {v}")
    }

    Ok(())
}

#[derive(Default)]
struct State {
    stack: Vec<i32>,
    locals: Vec<i32>,
}

impl State {
    fn with_n_locals(n: usize) -> Self {
        Self {
            stack: vec![],
            locals: vec![0; n],
        }
    }

    fn push(&mut self, v: i32) {
        self.stack.push(v)
    }
    fn pop(&mut self) -> i32 {
        self.stack.pop().unwrap()
    }
    fn try_pop(&mut self) -> Option<i32> {
        self.stack.pop()
    }
    fn get_local(&self, ix: u32) -> i32 {
        self.locals[ix as usize]
    }
    fn set_local(&mut self, ix: u32, val: i32) {
        self.locals[ix as usize] = val;
    }
}

struct Branch(u32);

fn exec(binary: &Binary, state: &mut State, code: &[Instruction]) -> Option<Branch> {
    use Instruction::*;
    for i in code {
        println!("exec: {i:?}");
        match i {
            Block { typ, expr } => match exec(binary, state, expr) {
                Some(Branch(0)) | None => {}
                Some(Branch(v)) => return Some(Branch(v - 1)),
            },
            End => { /* Do nothing? */ }
            BrTable {
                branch_ixs,
                default_ix,
            } => {
                let val = state.pop();
                if branch_ixs.len() < val as usize {
                    return Some(Branch(*default_ix));
                }
                return Some(Branch(branch_ixs[val as usize]));
            }
            Return => todo!(),
            Call(ix) => {
                let func_ix = &binary.functions[*ix as usize];
                let func_sig = &binary.types[*func_ix as usize];
                let func_code = &binary.code[*ix as usize];

                let nparams = func_sig.params.len();
                let nlocals = func_code.locals.len();

                let mut args = Vec::with_capacity(nparams);
                for _ in 0..nparams {
                    args.push(state.pop());
                }
                for _ in 0..nlocals {
                    args.push(0);
                }

                let mut new_state = State::default();
                new_state.locals = args;
                exec(binary, &mut new_state, &func_code.code);
                todo!()
                // if func_sig.results
            }
            Select => todo!(),
            LocalGet(ix) => {
                state.push(state.get_local(*ix));
            }
            LocalSet(ix) => {
                let val = state.pop();
                state.set_local(*ix, val);
            }
            GlobalGet(ix) => todo!(),
            GlobalSet(ix) => todo!(),
            I32Const(val) => state.push(*val),
            Add => {
                let val1 = state.pop();
                let val2 = state.pop();
                state.push(val1 + val2);
            }
            other => todo!("interpreter {other:?}"),
        }
    }
    None
}
