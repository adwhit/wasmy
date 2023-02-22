use num_traits::FromPrimitive;

use nom::{
    bytes::complete::{tag, take_while},
    combinator::{eof, map, map_opt, map_res},
    multi::{count, length_count, length_data, many1},
    number::complete::le_u32 as the_u32,
    number::complete::u8,
    sequence::{terminated, tuple},
    Finish, IResult,
};

type Result<'a, T> = IResult<&'a [u8], T>;

use crate::{Binary, Code, Data, Export, ExportType, FuncSig, Global, Limits, Mutability};

use super::{Instruction, Value};

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum OpCode {
    NoOp = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    BrTable = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    Drop = 0x1A,
    Select = 0x1B,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    I32Load = 0x28,
    I32Store = 0x36,
    I32Const = 0x41,
    Gt = 0x4b,
    Ge = 0x4e,
    Add = 0x6a,
    Sub = 0x6b,
}

fn opcode(input: &[u8]) -> Result<OpCode> {
    map_opt(u8, OpCode::from_u8)(input)
}

fn ast(mut input: &[u8]) -> Result<Vec<Instruction>> {
    let mut output = Vec::new();
    let mut instr;
    loop {
        (input, instr) = instruction(input)?;
        if let Instruction::End = instr {
            output.push(instr);
            return Ok((input, output));
        }
        output.push(instr);
    }
}

fn instruction(input: &[u8]) -> Result<Instruction> {
    if !input.is_empty() {
        println!("{:x?}", input[0])
    }
    use Instruction as I;
    use OpCode as O;
    let (input, op) = opcode(input)?;
    // println!("raw: {:?}", op);
    let op = match op {
        O::Block => {
            let (input, typ) = value(input)?;
            let (input, expr) = ast(&input)?;
            Ok((input, I::Block { typ, expr }))
        }
        O::End => Ok((input, I::End)),
        O::Br => map(leb128_u32, I::Br)(input),
        O::BrIf => map(leb128_u32, I::BrIf)(input),
        O::BrTable => map(
            tuple((length_count(leb128_u32, leb128_u32), leb128_u32)),
            |(branch_ixs, default_ix)| I::BrTable {
                branch_ixs,
                default_ix,
            },
        )(input),
        O::Return => Ok((input, I::Return)),
        O::Call => map(leb128_u32, I::Call)(input),
        O::Select => Ok((input, I::Select)),
        O::LocalGet => map(leb128_u32, I::LocalGet)(input),
        O::LocalSet => map(leb128_u32, I::LocalSet)(input),
        O::LocalTee => map(leb128_u32, I::LocalTee)(input),
        O::GlobalGet => map(leb128_u32, I::GlobalGet)(input),
        O::GlobalSet => map(leb128_u32, I::GlobalSet)(input),
        O::I32Store => map(tuple((leb128_u32, leb128_u32)), |(offset, alignment)| {
            I::I32Store { offset, alignment }
        })(input),
        O::I32Load => map(tuple((leb128_u32, leb128_u32)), |(offset, alignment)| {
            I::I32Load { offset, alignment }
        })(input),
        O::I32Const => map(leb128_i32, I::I32Const)(input),
        O::Gt => Ok((input, I::Gt)),
        O::Ge => Ok((input, I::Ge)),
        O::Add => Ok((input, I::Add)),
        O::Sub => Ok((input, I::Sub)),
        s => todo!("{s:?}"),
    };
    // println!("parsed: {:?}", op.as_ref().map(|v| &v.1).unwrap());
    op
}

#[derive(Debug)]
pub struct AnonSection {
    pub id: SectionId,
    pub data: Vec<u8>,
}

fn limits(input: &[u8]) -> Result<Limits> {
    let (input, has_max) = map_opt(u8, |v| match v {
        0 => Some(false),
        1 => Some(true),
        _ => None,
    })(input)?;
    let (input, min) = leb128(input)?;
    if has_max {
        let (input, max) = leb128(input)?;
        Ok((
            input,
            Limits {
                min,
                max: Some(max),
            },
        ))
    } else {
        Ok((input, Limits { min, max: None }))
    }
}

fn mutability(input: &[u8]) -> Result<Mutability> {
    map_opt(u8, Mutability::from_u8)(input)
}

fn global(input: &[u8]) -> Result<Global> {
    let (input, (typ, mutable, expr)) = tuple((value, mutability, ast))(input)?;
    Ok((input, Global { typ, mutable, expr }))
}

fn value(input: &[u8]) -> Result<Value> {
    map_opt(u8, Value::from_u8)(input)
}

fn export_type(input: &[u8]) -> Result<ExportType> {
    map_opt(u8, ExportType::from_u8)(input)
}

fn export(input: &[u8]) -> Result<Export> {
    let (input, slice) = map_res(length_data(leb128), std::str::from_utf8)(input)?;
    let (input, (tag, index)) = tuple((export_type, leb128_u32))(input)?;
    Ok((
        input,
        Export {
            name: slice.to_string(),
            tag,
            index,
        },
    ))
}

fn code(input: &[u8]) -> Result<Code> {
    let (input, _size) = leb128(input)?;
    let (input, locals) = length_count(leb128, tuple((map(leb128, |v| v as u32), value)))(input)?;
    let (input, code) = ast(input)?;
    Ok((input, Code { locals, code }))
}

#[derive(Debug)]
pub enum Section {
    Types(Vec<FuncSig>),
    Functions(Vec<u32>),
    Memory(Vec<Limits>),
    Global(Vec<Global>),
    Export(Vec<Export>),
    Code(Vec<Code>),
    Data(Vec<Data>),
    Anon(AnonSection),
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Func = 3,
    Table = 4,
    Mem = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Elem = 9,
    Code = 10,
    Data = 11,
}

fn func_sig(input: &[u8]) -> Result<FuncSig> {
    let (input, _) = tag(&[0x60])(input)?;
    let (input, nparams) = leb128(input)?;
    let (input, params) = count(value, nparams as usize)(input)?;
    let (input, nresults) = leb128(input)?;
    let (input, results) = count(value, nresults as usize)(input)?;
    Ok((input, FuncSig { params, results }))
}

fn data(input: &[u8]) -> Result<Data> {
    let (input, (mem_idx, offset)) = tuple((map(u8, u64::from), ast))(input)?;
    let (input, init) = length_data(leb128)(input)?;
    Ok((
        input,
        Data {
            mem_idx,
            offset,
            init: init.to_vec(),
        },
    ))
}

const MASK: u8 = 0b01111111;

// todo handle overflows
fn leb128(input: &[u8]) -> Result<u64> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u64) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u64) << (ix * 7)
    }
    Ok((input, val))
}

fn leb128_u32(input: &[u8]) -> Result<u32> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u32) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u32) << (ix * 7)
    }
    Ok((input, val))
}

fn leb128_i32(input: &[u8]) -> Result<i32> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u32) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u32) << (ix * 7)
    }
    Ok((input, unsafe { std::mem::transmute(val) }))
}

fn function_sigs(input: &[u8]) -> Result<Vec<FuncSig>> {
    length_count(leb128, func_sig)(input)
}

fn function_indices(input: &[u8]) -> Result<Vec<u32>> {
    length_count(leb128_u32, leb128_u32)(input)
}

fn memory(input: &[u8]) -> Result<Vec<Limits>> {
    length_count(leb128, limits)(input)
}

fn globals(input: &[u8]) -> Result<Vec<Global>> {
    length_count(leb128, global)(input)
}

fn section(input: &[u8]) -> Result<Section> {
    let (input, id) = map_opt(u8, SectionId::from_u8)(input)?;
    let (input, section_data) = length_data(leb128)(input)?;
    println!("Segment: {id:?}");

    // TODO if segment parsing fails, should be a HARD error

    let section = match id {
        SectionId::Type => {
            let (_, funcs) = function_sigs(section_data)?;
            Section::Types(funcs)
        }
        SectionId::Func => {
            let (_, func_ixs) = function_indices(section_data)?;
            Section::Functions(func_ixs)
        }
        SectionId::Mem => {
            let (_, mem) = memory(section_data)?;
            Section::Memory(mem)
        }
        SectionId::Global => {
            let (_, globals) = globals(section_data)?;
            Section::Global(globals)
        }
        SectionId::Export => {
            let (_, exports) = length_count(leb128, export)(section_data)?;
            Section::Export(exports)
        }
        SectionId::Code => {
            let (_, code) = length_count(leb128, code)(section_data)?;
            Section::Code(code)
        }
        SectionId::Data => {
            let (_, data) = length_count(leb128, data)(section_data)?;
            Section::Data(data)
        }
        _ => Section::Anon(AnonSection {
            id,
            data: section_data.to_vec(),
        }),
    };
    Ok((input, section))
}

fn binary(input: &[u8]) -> Result<Binary> {
    let mut bin = Binary::default();
    let (input, _) = tag(b"\0asm")(input)?;
    let (input, version) = the_u32(input)?;
    bin.version = version;
    let (input, sections) = many1(section)(input)?;
    for s in sections {
        match s {
            Section::Types(s) => bin.types = s,
            Section::Functions(s) => bin.functions = s,
            Section::Memory(s) => bin.memory = s,
            Section::Global(s) => bin.global = s,
            Section::Export(s) => bin.export = s,
            Section::Code(s) => bin.code = s,
            Section::Data(s) => bin.data = s,
            Section::Anon(_) => { /* TODO */ }
        }
    }
    Ok((input, bin))
}

pub fn parse_wasm(input: &[u8]) -> anyhow::Result<Binary, nom::error::Error<&[u8]>> {
    let (_, out) = terminated(binary, eof)(input).finish()?;
    Ok(out)
}
