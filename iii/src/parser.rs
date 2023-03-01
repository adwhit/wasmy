use num_traits::FromPrimitive;

use nom::{
    branch,
    bytes::complete::{tag, take_while},
    combinator::{eof, map, map_opt, map_res, opt},
    multi::{count, length_count, length_data, many1},
    number::complete::le_u32 as the_u32,
    number::complete::u8,
    sequence::{preceded, terminated, tuple},
    Finish, IResult,
};

type Result<'a, T> = IResult<&'a [u8], T>;

use crate::{
    BinOp, BinOpSigned, Binary, Code, Data, Export, ExportType, FuncSig, Global, Limits, MemOp,
    Mutability, Names, UnaryOp,
};

use super::{Instruction, Value};

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum OpCode {
    // Control
    Unreachable  = 0x00,
    NoOp         = 0x01,
    Block        = 0x02,
    Loop         = 0x03,
    If           = 0x04,
    Else         = 0x05,
    End          = 0x0B,
    Br           = 0x0C,
    BrIf         = 0x0D,
    BrTable      = 0x0E,
    Return       = 0x0F,
    Call         = 0x10,
    CallIndirect = 0x11,
    Drop         = 0x1A,
    Select       = 0x1B,

    // Mem
    LocalGet     = 0x20,
    LocalSet     = 0x21,
    LocalTee     = 0x22,
    GlobalGet    = 0x23,
    GlobalSet    = 0x24,
    I32Load      = 0x28,
    I64Load      = 0x29,
    F32Load      = 0x2a,
    F64Load      = 0x2b,
    I32Load8     = 0x2c,
    U32Load8     = 0x2d,
    I32Load16    = 0x2e,
    U32Load16    = 0x2f,
    I64Load8     = 0x30,
    U64Load8     = 0x31,
    I64Load16    = 0x32,
    U64Load16    = 0x33,
    I64Load32    = 0x34,
    U64Load32    = 0x35,
    I32Store     = 0x36,
    I64Store     = 0x37,
    F32Store     = 0x38,
    F64Store     = 0x39,
    I32Store8    = 0x3a,
    I32Store16   = 0x3b,
    I64Store8    = 0x3c,
    I64Store16   = 0x3d,
    I64Store32   = 0x3e,
    MemorySize   = 0x3f,
    MemoryGrow   = 0x40,
    I32Const     = 0x41,
    I64Const     = 0x42,
    F32Const     = 0x43,
    F64Const     = 0x44,

    // Operations
    Eqz          = 0x45,
    Eq           = 0x46,
    Neq          = 0x47,
    Lt           = 0x48,
    LtU          = 0x49,
    Gt           = 0x4a,
    GtU          = 0x4b,
    Le           = 0x4c,
    LeU          = 0x4d,
    Ge           = 0x4e,
    GeU          = 0x4f,
    I64Eqz       = 0x50,
    I64Eq        = 0x51,
    I64Neq       = 0x52,
    I64Lt        = 0x53,
    I64LtU       = 0x54,
    I64Gt        = 0x55,
    I64GtU       = 0x56,
    I64Le        = 0x57,
    I64LeU       = 0x58,
    I64Ge        = 0x59,
    I64GeU       = 0x5a,
    Clz          = 0x67,
    Ctz          = 0x68,
    Add          = 0x6a,
    Sub          = 0x6b,
    Mul          = 0x6c,
    Div          = 0x6d,
    DivU         = 0x6e,
    Rem          = 0x6f,
    RemU         = 0x70,
    And          = 0x71,
    Or           = 0x72,
    Xor          = 0x73,
    Shl          = 0x74,
    Shr          = 0x75,
    ShrU         = 0x76,
    Rotl         = 0x77,
    Rotr         = 0x78,
    I64Clz       = 0x79,
    I64Ctz       = 0x7a,
    I64Add       = 0x7c,
    I64Sub       = 0x7d,
    I64Mul       = 0x7e,
    I64Div       = 0x7f,
    I64DivU      = 0x80,
    I64Rem       = 0x81,
    I64RemU      = 0x82,
    I64And       = 0x83,
    I64Or        = 0x84,
    I64Xor       = 0x85,
    I64Shl       = 0x86,
    I64Shr       = 0x87,
    I64ShrU      = 0x88,
    I64Rotl      = 0x89,
    I64Rotr      = 0x8a,

    Wrap         = 0xa7
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

#[rustfmt::skip]
fn instruction(input: &[u8]) -> Result<Instruction> {
    if !input.is_empty() {
        println!("byte: {:x?}", input[0])
    }
    use Instruction as I;
    use OpCode as O;
    use BinOpSigned as B;
    use crate::Sign::*;
    use crate::Size::*;
    let (input, op) = opcode(input)?;
    println!("op: {op:?}");
    let op = match op {
        O::Unreachable => I::Unreachable,
        O::NoOp => I::NoOp,
        O::Block => {
            let (input, typ) = branch::alt((map(value, Option::Some), map(tag(&[0x40]), |_| None)))(input)?;
            let (input, expr) = ast(input)?;
            return Ok((input, I::Block { typ, expr }));
        }
        O::Loop => {
            let (input, typ) = branch::alt((map(value, Option::Some), map(tag(&[0x40]), |_| None)))(input)?;
            let (input, expr) = ast(input)?;
            return Ok((input, I::Loop { typ, expr }));
        }
        O::End => I::End,
        O::Br => return map(leb128_u32, I::Br)(input),
        O::BrIf => return map(leb128_u32, I::BrIf)(input),
        O::BrTable => {
            return map(
                tuple((length_count(leb128_u32, leb128_u32), leb128_u32)),
                |(branch_ixs, default_ix)| I::BrTable {
                    branch_ixs,
                    default_ix,
                },
            )(input)
        }
        O::Return => I::Return,
        O::Call => return map(leb128_u32, I::Call)(input),
        O::CallIndirect => {
            return map(terminated(leb128_u32, tag(&[0x00])), I::CallIndirect)(input)
        }
        O::Drop   => I::Drop,
        O::Select => I::Select,
        O::LocalGet  => return map(leb128_u32, I::LocalGet )(input),
        O::LocalSet  => return map(leb128_u32, I::LocalSet )(input),
        O::LocalTee  => return map(leb128_u32, I::LocalTee )(input),
        O::GlobalGet => return map(leb128_u32, I::GlobalGet)(input),
        O::GlobalSet => return map(leb128_u32, I::GlobalSet)(input),
        | O::I32Store | O::I32Store8 | O::I32Store16
        | O::I64Store
        | O::I32Load  | O::I32Load8
        | O::U32Load8 | O::U32Load16
        | O::I64Load  | O::I64Load32
        | O::U64Load32 => {
            return map(tuple((leb128_u32, leb128_u32)), |(offset, alignment)| {
                I::MemOp {
                    offset,
                    alignment,
                    op: MemOp::try_from(op).unwrap(),
                }
            })(input)
        }
        O::MemoryGrow => I::MemoryGrow,
        O::I32Const => return map(leb128_i32, I::I32Const)(input),
        O::I64Const => return map(leb128_i64, I::I64Const)(input),

        O::Eqz    => I::UnaryOp { op: UnaryOp::Eqz, size: X32 },
        O::I64Eqz => I::UnaryOp { op: UnaryOp::Eqz, size: X64 },
        O::Ctz    => I::UnaryOp { op: UnaryOp::Ctz, size: X32 },
        O::Clz    => I::UnaryOp { op: UnaryOp::Clz, size: X32 },

        O::Gt   => I::BinOpSigned { op: B::Gt,  size: X32, sign: Signed   },
        O::GtU  => I::BinOpSigned { op: B::Gt,  size: X32, sign: Unsigned },
        O::Ge   => I::BinOpSigned { op: B::Ge,  size: X32, sign: Signed   },
        O::GeU  => I::BinOpSigned { op: B::Ge,  size: X32, sign: Unsigned },
        O::Lt   => I::BinOpSigned { op: B::Lt,  size: X32, sign: Signed   },
        O::LtU  => I::BinOpSigned { op: B::Lt,  size: X32, sign: Unsigned },
        O::Le   => I::BinOpSigned { op: B::Le,  size: X32, sign: Signed   },
        O::LeU  => I::BinOpSigned { op: B::Le,  size: X32, sign: Unsigned },
        O::Shr  => I::BinOpSigned { op: B::Shr, size: X32, sign: Signed   },
        O::ShrU => I::BinOpSigned { op: B::Shr, size: X32, sign: Unsigned },
        O::Div  => I::BinOpSigned { op: B::Div, size: X32, sign: Signed   },
        O::DivU => I::BinOpSigned { op: B::Div, size: X32, sign: Unsigned },

        O::I64Gt   => I::BinOpSigned { op: B::Gt,  size: X64, sign: Signed   },
        O::I64GtU  => I::BinOpSigned { op: B::Gt,  size: X64, sign: Unsigned },
        O::I64Ge   => I::BinOpSigned { op: B::Ge,  size: X64, sign: Signed   },
        O::I64GeU  => I::BinOpSigned { op: B::Ge,  size: X64, sign: Unsigned },
        O::I64Lt   => I::BinOpSigned { op: B::Lt,  size: X64, sign: Signed   },
        O::I64LtU  => I::BinOpSigned { op: B::Lt,  size: X64, sign: Unsigned },
        O::I64Le   => I::BinOpSigned { op: B::Le,  size: X64, sign: Signed   },
        O::I64LeU  => I::BinOpSigned { op: B::Le,  size: X64, sign: Unsigned },
        O::I64Shr  => I::BinOpSigned { op: B::Shr, size: X64, sign: Signed   },
        O::I64ShrU => I::BinOpSigned { op: B::Shr, size: X64, sign: Unsigned },
        O::I64Div  => I::BinOpSigned { op: B::Div, size: X64, sign: Signed   },
        O::I64DivU => I::BinOpSigned { op: B::Div, size: X64, sign: Unsigned },

        O::Add  => I::BinOp { op: BinOp::Add,  size: X32 },
        O::And  => I::BinOp { op: BinOp::And,  size: X32 },
        O::Sub  => I::BinOp { op: BinOp::Sub,  size: X32 },
        O::Shl  => I::BinOp { op: BinOp::Shl,  size: X32 },
        O::Mul  => I::BinOp { op: BinOp::Mul,  size: X32 },
        O::Eq   => I::BinOp { op: BinOp::Eq,   size: X32 },
        O::Neq  => I::BinOp { op: BinOp::Neq,  size: X32 },
        O::Or   => I::BinOp { op: BinOp::Or,   size: X32 },
        O::Xor  => I::BinOp { op: BinOp::Xor,  size: X32 },
        O::Rotl => I::BinOp { op: BinOp::Rotl, size: X32 },
        O::Rotr => I::BinOp { op: BinOp::Rotr, size: X32 },

        O::I64Add  => I::BinOp { op: BinOp::Add,  size: X64 },
        O::I64And  => I::BinOp { op: BinOp::And,  size: X64 },
        O::I64Sub  => I::BinOp { op: BinOp::Sub,  size: X64 },
        O::I64Shl  => I::BinOp { op: BinOp::Shl,  size: X64 },
        O::I64Mul  => I::BinOp { op: BinOp::Mul,  size: X64 },
        O::I64Eq   => I::BinOp { op: BinOp::Eq,   size: X64 },
        O::I64Neq  => I::BinOp { op: BinOp::Neq,  size: X64 },
        O::I64Or   => I::BinOp { op: BinOp::Or,   size: X64 },
        O::I64Xor  => I::BinOp { op: BinOp::Xor,  size: X64 },
        O::I64Rotl => I::BinOp { op: BinOp::Rotl, size: X64 },
        O::I64Rotr => I::BinOp { op: BinOp::Rotr, size: X64 },

        O::Wrap => I::Wrap,

        s => todo!("{s:?}"),
    };
    // println!("parsed: {:?}", op.as_ref().map(|v| &v.1).unwrap());
    Ok((input, op))
}

impl TryFrom<OpCode> for MemOp {
    type Error = ();
    fn try_from(value: OpCode) -> std::result::Result<Self, Self::Error> {
        let ok = match value {
            OpCode::I32Load => MemOp::I32Load,
            OpCode::I64Load => MemOp::I64Load,
            OpCode::I32Load8 => MemOp::I32Load8,
            OpCode::U32Load8 => MemOp::U32Load8,
            OpCode::U32Load16 => MemOp::U32Load16,
            OpCode::I32Store => MemOp::I32Store,
            OpCode::I32Store16 => MemOp::I32Store16,
            OpCode::I64Store => MemOp::I64Store,
            OpCode::U64Load32 => MemOp::U64Load32,
            OpCode::I32Store8 => MemOp::I32Store8,
            _ => return Err(()),
        };
        Ok(ok)
    }
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
    let (input, (slice, tag, index)) = tuple((str_slice, export_type, leb128_u32))(input)?;
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
    Names(Names),
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

// TODO handle overflows
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

fn leb128_i32(mut input: &[u8]) -> Result<i32> {
    let mut result = 0;
    let mut shift = 0;
    let mut byte;
    let size = 32;
    loop {
        (input, byte) = u8(input)?;
        result |= ((MASK & byte) as u32) << shift;
        shift += 7;
        if byte >> 7 == 0 {
            break;
        }
    }
    if (shift < size) && (0x40 & byte != 0) {
        // sign-extend
        result |= !0 << shift;
    }
    Ok((input, unsafe { std::mem::transmute(result) }))
}

fn leb128_i64(mut input: &[u8]) -> Result<i64> {
    let mut result = 0;
    let mut shift = 0;
    let mut byte;
    let size = 64;
    loop {
        (input, byte) = u8(input)?;
        result |= ((MASK & byte) as u64) << shift;
        shift += 7;
        if byte >> 7 == 0 {
            break;
        }
    }
    if (shift < size) && (0x40 & byte != 0) {
        // sign-extend
        result |= !0 << shift;
    }
    Ok((input, unsafe { std::mem::transmute(result) }))
}

fn str_slice(input: &[u8]) -> Result<&str> {
    map_res(length_data(leb128), std::str::from_utf8)(input)
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

fn namemap(input: &[u8]) -> Result<Vec<(u32, String)>> {
    let (input, _size) = leb128_u32(input)?;
    length_count(
        leb128_u32,
        tuple((leb128_u32, map(str_slice, String::from))),
    )(input)
}

fn names(input: &[u8]) -> Result<Names> {
    let (input, mod_name) = opt(preceded(tag([0x0]), str_slice))(input)?;
    let (input, func_names) = opt(preceded(tag([0x1]), namemap))(input)?;
    let (input, global_names) = opt(preceded(tag([0x7]), namemap))(input)?;
    let (input, data_names) = opt(preceded(tag([0x9]), namemap))(input)?;

    Ok((
        input,
        Names {
            module_name: mod_name.map(String::from),
            func_names: func_names.unwrap_or(vec![]),
            global_names: global_names.unwrap_or(vec![]),
            data_names: data_names.unwrap_or(vec![]),
        },
    ))
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
        SectionId::Custom => {
            let (input, slice) = str_slice(section_data)?;
            match slice {
                "name" => {
                    let (_, names) = names(input)?;
                    Section::Names(names)
                }
                ".debug_info" | ".debug_pubtypes" | ".debug_ranges" | ".debug_abbrev"
                | ".debug_line" | ".debug_str" | ".debug_pubnames" | "producers" => {
                    println!("Debug info {}", section_data.len());
                    Section::Anon(AnonSection {
                        id,
                        data: section_data.to_vec(),
                    })
                }
                _ => todo!("{slice}"),
            }
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
            Section::Names(s) => bin.names = s,
            Section::Anon(s) => {
                println!("{s:?}")
            }
        }
    }
    Ok((input, bin))
}

pub fn parse_wasm(input: &[u8]) -> anyhow::Result<Binary, nom::error::Error<&[u8]>> {
    let (_, out) = terminated(binary, eof)(input).finish()?;
    Ok(out)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_u32() {
        let input = &[0xE5, 0x8E, 0x26];
        assert_eq!(leb128_u32(input).unwrap(), (&[][..], 624485));
    }

    #[test]
    fn test_parse_i32() {
        let input = &[0x00];
        assert_eq!(leb128_i32(input).unwrap(), (&[][..], 0));

        let input = &[0x01];
        assert_eq!(leb128_i32(input).unwrap(), (&[][..], 1));

        let input = &[0xC0, 0xBB, 0x78];
        assert_eq!(leb128_i32(input).unwrap(), (&[][..], -123456));
    }
}
