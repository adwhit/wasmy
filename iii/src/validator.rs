use crate::{Binary, Instruction, Size};
use anyhow::{bail, format_err, Result};

pub fn validate(binary: &Binary) -> Result<()> {
    for (func_ix, func_ty_ix) in binary.functions.iter().enumerate() {
        let Some(func_sig) = binary.types.get(*func_ty_ix as usize) else {
            bail!("exported function missing")
        };
        let Some(code) = binary.code.get(func_ix) else {
            bail!("exported function missing")
        };
        let mut state = State::new();
        symbolic_exec(&code.code, &mut state, binary)?;
    }
    Ok(())
}

#[derive(Debug)]
struct State {
    stack: Vec<Vec<Size>>,
    locals: Vec<Size>,
    globals: Vec<Size>,
}

impl State {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            globals: Vec::new(),
        }
    }
    fn push(&mut self, s: Size) {
        self.stack.last_mut().unwrap().push(s);
    }
    fn pop(&mut self) -> Result<Size> {
        if let Some(x) = self.stack.last_mut().unwrap().pop() {
            Ok(x)
        } else {
            bail!("stack empty")
        }
    }
    fn pop_expect(&mut self, sz: Size) -> Result<()> {
        match self.stack.last_mut().unwrap().pop() {
            Some(x) if x == sz => Ok(()),
            Some(x) => bail!("mismatched type {x:?}"),
            None => bail!("stack empty"),
        }
    }
    fn get_local(&self, ix: u32) -> Result<Size> {
        self.locals
            .get(ix as usize)
            .copied()
            .ok_or_else(|| format_err!("missing local {ix}"))
    }
    fn set_local(&self, ix: u32, typ: Size) -> Result<()> {
        let expect = self
            .locals
            .get(ix as usize)
            .copied()
            .ok_or_else(|| format_err!("missing local {ix}"))?;
        if typ != expect {
            bail!("mismatched types")
        }
        Ok(())
    }
}

fn symbolic_exec(code: &[Instruction], state: &mut State, binary: &Binary) -> Result<()> {
    use Instruction::*;
    use Size::*;
    for i in code {
        match i {
            Select => {
                state.pop_expect(X32)?;
                let s2 = state.pop()?;
                let s1 = state.pop()?;
                if s1 != s2 {
                    bail!("mismatched types")
                }
            }
            LocalGet(ix) => {
                state.push(state.get_local(*ix)?);
            }
            LocalSet(ix) => {
                let ty = state.pop()?;
                state.set_local(*ix, ty)?;
            }
            _ => todo!("{i:?}"),
        }
    }
    Ok(())
}
