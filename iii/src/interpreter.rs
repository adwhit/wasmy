use std::{collections::HashMap, fmt::Debug};

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
    let globals = binary
        .global
        .iter()
        .map(|g| {
            let mut fake_state = State::new();
            exec(binary, &mut fake_state, &g.expr);
            fake_state.pop()
        })
        .collect();

    let mut state = State::new();
    state.locals.push(vec![0; n_locals]);
    state.globals = globals;

    for d in &binary.data {
        let offset = {
            let mut fake_state = State::new();
            exec(binary, &mut fake_state, &d.offset);
            fake_state.pop() as usize
        };
        let extent = offset + d.init.len();
        if state.memory.len() < extent {
            state.memory.resize(extent, 0);
        }
        state.memory[offset..].copy_from_slice(&d.init);
    }

    exec(binary, &mut state, &code.code);

    while let Some(v) = state.try_pop() {
        println!("Value on stack: {v}")
    }

    println!("{state:?}");

    Ok(())
}

#[derive(custom_debug::Debug)]
struct State {
    stack: Vec<i32>,
    locals: Vec<Vec<i32>>,
    globals: Vec<i32>,
    #[debug(with = "hex_fmt")]
    memory: Vec<u8>,
}

fn hex_fmt(n: &Vec<u8>, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut max_nonzero = 0;
    for (ix, val) in n.iter().enumerate().take(100) {
        if *val != 0 {
            max_nonzero = ix + 1;
        }
    }
    if max_nonzero % 8 != 0 {
        max_nonzero += 8 - (max_nonzero % 8);
    }
    write!(f, "{:x?}", &n[..max_nonzero])?;
    Ok(())
}

impl State {
    fn new() -> Self {
        Self {
            stack: vec![],
            locals: vec![],
            globals: vec![],
            memory: vec![0; 64 * 1024],
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
    fn peek(&mut self) -> i32 {
        *self.stack.last().unwrap()
    }
    fn get_local(&self, ix: u32) -> i32 {
        self.locals.last().unwrap()[ix as usize]
    }
    fn set_local(&mut self, ix: u32, val: i32) {
        self.locals.last_mut().unwrap()[ix as usize] = val;
    }
    fn get_global(&self, ix: u32) -> i32 {
        self.globals[ix as usize]
    }
    fn set_global(&mut self, ix: u32, val: i32) {
        self.globals[ix as usize] = val;
    }
}

struct Branch(u32);

fn exec(binary: &Binary, state: &mut State, code: &[Instruction]) -> Option<Branch> {
    use Instruction::*;
    for i in code {
        // println!("exec: {i:?}");
        match i {
            Block { typ, expr } => match exec(binary, state, expr) {
                Some(Branch(0)) | None => {}
                Some(Branch(v)) => return Some(Branch(v - 1)),
            },
            Loop { typ, expr } => {
                loop {
                    match exec(binary, state, expr) {
                        None => break,                                 // exit the loop
                        Some(Branch(0)) => {}                          // repeat the loop
                        Some(Branch(v)) => return Some(Branch(v - 1)), // branch outside the loop
                    }
                }
            }
            End => { /* Do nothing? */ }
            BrIf(ix) => {
                let val = state.pop();
                if val != 0 {
                    return Some(Branch(*ix));
                }
            }
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

                // push the new frame's locals
                state.locals.push(args);
                // call
                exec(binary, state, &func_code.code);
                // restore the curent frame's locals
                state.locals.pop().unwrap();
            }
            Select => {
                let sel = state.pop();
                let c2 = state.pop();
                let c1 = state.pop();
                if sel != 0 {
                    state.push(c1)
                } else {
                    state.push(c2)
                }
            }
            LocalGet(ix) => {
                state.push(state.get_local(*ix));
            }
            LocalSet(ix) => {
                let val = state.pop();
                // println!("set local {ix} to {val}");
                state.set_local(*ix, val);
            }
            LocalTee(ix) => {
                let val = state.peek();
                // println!("tee local {ix} to {val}");
                state.set_local(*ix, val);
            }
            GlobalGet(ix) => {
                state.push(state.get_global(*ix));
            }
            GlobalSet(ix) => {
                let val = state.pop();
                state.set_global(*ix, val);
            }
            I32Store { offset, alignment } => {
                let val = state.pop();
                let base_loc = state.pop();
                let loc = (base_loc + *offset as i32) as usize;
                let bytes: [u8; 4] = unsafe { std::mem::transmute(val) };
                state.memory[loc..loc + 4].copy_from_slice(&bytes);
            }
            I32Load { offset, alignment } => {
                let base_loc = state.pop();
                let loc = (base_loc + *offset as i32) as usize;
                let slice: &[u8] = &state.memory[loc..loc + 4];
                let arr: [u8; 4] = slice.try_into().unwrap();
                let val: i32 = unsafe { std::mem::transmute(arr) };
                state.push(val);
            }
            I32Const(val) => {
                state.push(*val);
            }
            Add => {
                let val1 = state.pop();
                let val2 = state.pop();
                // println!("add {val1} + {val2}");
                state.push(val1 + val2);
            }
            Sub => {
                let val2 = state.pop();
                let val1 = state.pop();
                state.push(val1 - val2);
            }
            Ge => {
                let val2 = state.pop();
                let val1 = state.pop();
                state.push(if val1 >= val2 { 1 } else { 0 });
            }
            Gt => {
                let val2 = state.pop();
                let val1 = state.pop();
                state.push(if val1 > val2 { 1 } else { 0 });
            }
            other => todo!("interpreter {other:?}"),
        }
    }
    None
}
