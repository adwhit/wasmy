use std::collections::HashMap;

use crate::{ExportType, Instruction, MemOp, Sign, Size};
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

    println!("Load binary data");

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

    println!("*** Execute '{main}'");

    exec(binary, &mut state, &code.code);

    println!("*** Finished");

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

enum ExitBlock {
    Branch(u32),
    FallThrough,
    Return,
}

fn exec(binary: &Binary, state: &mut State, code: &[Instruction]) -> ExitBlock {
    use Instruction::*;
    for i in code {
        println!("{state:?}");
        println!("exec: {i:?}");
        match i {
            Block { typ: _, expr } => match exec(binary, state, expr) {
                ExitBlock::Branch(0) | ExitBlock::FallThrough => {}
                ExitBlock::Return => return ExitBlock::Return,
                ExitBlock::Branch(v) => return ExitBlock::Branch(v - 1),
            },
            Loop { typ: _, expr } => {
                loop {
                    match exec(binary, state, expr) {
                        ExitBlock::FallThrough => break, // exit the loop
                        ExitBlock::Branch(0) => {}       // repeat the loop
                        ExitBlock::Return => return ExitBlock::Return,
                        ExitBlock::Branch(v) => return ExitBlock::Branch(v - 1), // branch outside loop
                    }
                }
            }
            End => { /* Do nothing? */ }
            Br(ix) => return ExitBlock::Branch(*ix),
            BrIf(ix) => {
                let val = state.pop();
                if val != 0 {
                    return ExitBlock::Branch(*ix);
                }
            }
            BrTable {
                branch_ixs,
                default_ix,
            } => {
                let val = state.pop();
                if branch_ixs.len() < val as usize {
                    return ExitBlock::Branch(*default_ix);
                }
                return ExitBlock::Branch(branch_ixs[val as usize]);
            }
            Return => return ExitBlock::Return,
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
            I32Const(val) => state.push(*val),
            MemOp {
                offset,
                alignment: _,
                op,
            } => match op {
                crate::MemOp::I32Store => {
                    let val = state.pop();
                    let base_loc = state.pop();
                    let loc = (base_loc + *offset as i32) as usize;
                    let bytes: [u8; 4] = unsafe { std::mem::transmute(val) };
                    state.memory[loc..loc + 4].copy_from_slice(&bytes);
                }
                crate::MemOp::I32Load => {
                    let base_loc = state.pop();
                    let loc = (base_loc + *offset as i32) as usize;
                    let slice: &[u8] = &state.memory[loc..loc + 4];
                    let arr: [u8; 4] = slice.try_into().unwrap();
                    let val: i32 = unsafe { std::mem::transmute(arr) };
                    state.push(val);
                }
                _ => todo!("{i:?}"),
            },
            BinOp { op, size } => {
                assert_eq!(*size, Size::X32);
                let val2 = state.pop();
                let val1 = state.pop();
                use crate::BinOp::*;
                let res = match op {
                    Add => val1 + val2,
                    Sub => val1 - val2,
                    Shl => val1 << val2,
                    _ => todo!("cannot eval: {op:?}"),
                };
                state.push(res);
            }
            BinOpSigned { op, size, sign } => {
                use crate::BinOpSigned::*;
                assert_eq!(*size, Size::X32);
                assert_eq!(*sign, Sign::Signed);
                let val2 = state.pop();
                let val1 = state.pop();
                let res = match op {
                    Ge => {
                        if val1 >= val2 {
                            1
                        } else {
                            0
                        }
                    }
                    Gt => {
                        if val1 > val2 {
                            1
                        } else {
                            0
                        }
                    }
                    Lt => {
                        if val1 < val2 {
                            1
                        } else {
                            0
                        }
                    }
                    _ => todo!("cannot eval: {op:?}"),
                };
                state.push(res);
            }
            other => todo!("interpreter {other:?}"),
        }
    }
    ExitBlock::FallThrough
}

fn u(v: i32) -> u32 {
    unsafe { std::mem::transmute(v) }
}
