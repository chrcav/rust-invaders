#[cfg(test)]
mod test;

pub mod debug;

use std::io::prelude::*;
use std::ops::Range;
use std::thread;
use std::time::Duration;
use std::time::Instant;

#[derive(Default)]
pub struct ConditionCodes {
    z: u8,
    s: u8,
    p: u8,
    cy: u8,
    ac: u8,
    _pad: u8,
}

impl std::fmt::Display for ConditionCodes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            r#"z {:#02x} cy {:#02x} s {:#02x} p {:#02x}
"#,
            self.z, self.cy, self.s, self.p,
        )
    }
}

pub struct State8080 {
    pub a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pub pc: u16,
    pub memory: Vec<u8>,
    pub assembly: Vec<debug::Instruction8080>,
    ram_region: std::ops::Range<u16>,
    cc: ConditionCodes,
    int_enable: u8,
    program_len: usize,
    last_op_time: Instant,
}

impl State8080 {
    pub fn new() -> Self {
        Self {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0,
            memory: Vec::new(),
            assembly: Vec::new(),
            ram_region: 0x0000..0xffff,
            cc: ConditionCodes::default(),
            int_enable: 0,
            program_len: 0,
            last_op_time: Instant::now(),
        }
    }
}

impl std::fmt::Display for State8080 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            r#"a {:#04x}
b {:#04x} c {:#04x}
d {:#04x} e {:#04x}
h {:#04x} l {:#04x}
pc {:04x} sp {:04x}
"#,
            self.a, self.b, self.c, self.d, self.e, self.h, self.l, self.pc, self.sp,
        )
    }
}

pub fn create_initial_emustate(
    filename: &str,
    start_pc: u16,
    ram_region: Range<u16>,
) -> std::io::Result<State8080> {
    let mut f = std::fs::File::open(filename)?;
    let mut state = State8080::new();
    state
        .memory
        .resize_with(start_pc as usize, Default::default);
    // read the whole file
    f.read_to_end(&mut state.memory)?;
    state.program_len = state.memory.len();
    state.memory.resize_with(65535, Default::default);
    state.assembly = debug::disassemble(&state.memory);
    state.pc = start_pc;
    state.ram_region = ram_region;
    Ok(state)
}

pub fn generate_interrupt(mut state: State8080, interrupt_num: u8) -> State8080 {
    if state.int_enable == 0x1 {
        state.int_enable = 0x0;
        let rst_opcode = (interrupt_num << 3) | 0xc7;
        emu8080_opcode(state, rst_opcode)
    } else {
        state
    }
}

pub fn emu8080_opcode(mut state: State8080, op: u8) -> State8080 {
    let time_since_last_op = state.last_op_time.elapsed();
    if time_since_last_op < Duration::new(0, 500) {
        thread::sleep(Duration::new(0, 500) - time_since_last_op);
    }
    state.last_op_time = Instant::now();

    match op {
        0x00 => {} // NOP
        0x01 => {
            // LXI b,D16
            state.b = state.memory[state.pc as usize + 1];
            state.c = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x02 | 0x12 | 0x22 | 0x32 => {
            // STAX *
            state = emu8080_store(state, op);
        }
        0x03 | 0x13 | 0x23 | 0x33 => {
            // INX *
            state = emu8080_inx(state, op);
        }
        0x04 | 0x0c | 0x14 | 0x1c | 0x24 | 0x2c | 0x34 | 0x3c => {
            //INR *
            state = emu8080_inr(state, op);
        }
        0x05 | 0x0d | 0x15 | 0x1d | 0x25 | 0x2d | 0x35 | 0x3d => {
            //DCR *
            state = emu8080_dcr(state, op);
        }
        0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x36 | 0x3e => {
            // MVI *, D8
            state = emu8080_mvi(state, op);
        }
        0x07 => {
            // RLC
            let mut a = state.a as u16;
            let bit7 = (a >> 7) & 0x1;
            a = a << 1;
            a |= bit7;
            state.cc.cy = bit7 as u8;
            state.a = a as u8;
        }
        0x09 => {
            // DAD B
            let bc = addr_from_reg_pair(state.b, state.c) as u32;
            let hl = addr_from_reg_pair(state.h, state.l) as u32;
            let answer = hl + bc;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x0a | 0x1a | 0x2a | 0x3a => {
            // LDAX *
            state = emu8080_load(state, op);
        }
        0x0b | 0x1b | 0x2b | 0x3b => {
            // DCX *
            state = emu8080_dcx(state, op);
        }
        0x0f => {
            // RRC
            let mut a = state.a;
            let bit0 = a & 0x1;
            a = a >> 1;
            a |= bit0 << 7;
            state.cc.cy = bit0;
            state.a = a;
        }
        0x11 => {
            // LXI D, D16
            state.d = state.memory[state.pc as usize + 1];
            state.e = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x17 => {
            // RAL
            let mut a = state.a as u16;
            let bit7 = (a >> 7) & 0x1;
            let cy = state.cc.cy as u16;
            a = a << 1;
            a |= cy & 0x1;
            state.cc.cy = bit7 as u8;
            state.a = a as u8;
        }
        0x19 => {
            // DAD D
            let de = (state.d as u32) << 8 | state.e as u32;
            let hl = (state.h as u32) << 8 | state.l as u32;
            let answer = hl + de;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x1f => {
            // RAR
            let mut a = state.a;
            let bit0 = a & 0x1;
            let cy = state.cc.cy;
            a = a >> 1;
            a |= (cy << 7) & 0x80;
            state.cc.cy = bit0 as u8;
            state.a = a as u8;
        }
        0x21 => {
            // LXI H, D16
            state.h = state.memory[state.pc as usize + 1];
            state.l = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x27 => {
            // DAA
            let mut a_lsb = state.a & 0xf;
            let mut a_msb = (state.a >> 4) & 0xf;
            if a_lsb > 0x9 || state.cc.ac == 1 {
                a_lsb += 0x6;
            }
            if a_lsb > 0xf {
                state.cc.ac = 1;
                a_msb += (a_lsb >> 4) & 0xf;
            }
            if a_msb > 0x9 {
                a_msb += 6;
            }
            if a_lsb > 0xf {
                state.cc.cy = 1;
            }
            state.a = (a_msb << 4) & 0xf0 | (a_lsb & 0xf)
        }
        0x29 => {
            // DAD H
            let hl = (state.h as u32) << 8 | state.l as u32;
            let answer = hl + hl;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x2f => {
            // CMA
            state.a ^= 0xff;
        }
        0x31 => {
            // LXI SP, D16
            state.sp = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
        }
        0x37 => {
            // STC
            state.cc.cy = 1;
        }
        0x39 => {
            // DAD SP
            let hl = (state.h as u32) << 8 | state.l as u32;
            let sp = state.sp as u32;
            let answer = hl + sp;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x3f => {
            // CMC
            state.cc.cy ^= 0x1;
        }
        0x40..=0x75 | 0x77..=0x7f => {
            // MOV
            state = emu8080_mov(state, op);
        }
        0x80..=0x87 => {
            // ADD
            state = emu8080_add(state, op - 0x80);
        }
        0x88..=0x8f => {
            // ADC
            state = emu8080_adc(state, op - 0x88);
        }
        0x90..=0x97 => {
            // SUB
            state = emu8080_sub(state, op - 0x90);
        }
        0x98..=0x9f => {
            // SBB
            state = emu8080_sbb(state, op - 0x98);
        }
        0xa0..=0xa7 => {
            // ANA
            state = emu8080_ana(state, op - 0xa0);
        }
        0xa8..=0xaf => {
            // XRA
            state = emu8080_xra(state, op - 0xa8);
        }
        0xb0..=0xb7 => {
            // ORA
            state = emu8080_ora(state, op - 0xb0);
        }
        0xb8..=0xbf => {
            // CMP
            state = emu8080_cmp(state, op - 0xb8);
        }
        0xc1 => {
            // POP B
            state.c = state.memory[state.sp as usize];
            state.b = state.memory[state.sp as usize + 1];
            state.sp += 2;
        }
        0xc0 | 0xc8 | 0xc9 | 0xd0 | 0xd8 | 0xe0 | 0xe8 | 0xf0 | 0xf8 => {
            // RET
            state = emu8080_ret(state, op);
        }
        0xc2 | 0xc3 | 0xca | 0xd2 | 0xda | 0xe2 | 0xea | 0xf2 | 0xfa => {
            // JMP ${addr}
            state = emu8080_jmp(state, op);
        }
        0xc4 | 0xcc | 0xcd | 0xd4 | 0xdc | 0xe4 | 0xec | 0xf4 | 0xfc => {
            // CALL ${addr}
            state = emu8080_call(state, op);
        }
        0xc5 => {
            // PUSH B
            state.memory =
                emu8080_write_mem(state.memory, &state.ram_region, state.sp - 2, state.c);
            state.memory =
                emu8080_write_mem(state.memory, &state.ram_region, state.sp - 1, state.b);
            state.sp -= 2;
        }
        0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe => {
            // ADI D8
            state = emu8080_immediate(state, op);
        }
        0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff => {
            // RST
            state = do_rst(state, op);
        }
        0xd1 => {
            // POP D
            state.e = state.memory[state.sp as usize];
            state.d = state.memory[state.sp as usize + 1];
            state.sp += 2;
        }
        0xd3 => {
            // OUT D8
            // TODO implement
            state.pc += 1;
        }
        0xd5 => {
            // PUSH D
            state.memory =
                emu8080_write_mem(state.memory, &state.ram_region, state.sp - 2, state.e);
            state.memory =
                emu8080_write_mem(state.memory, &state.ram_region, state.sp - 1, state.d);
            state.sp -= 2;
        }
        0xe1 => {
            // POP H
            state.l = state.memory[state.sp as usize];
            state.h = state.memory[state.sp as usize + 1];
            state.sp += 2;
        }
        0xe3 => {
            // XTHL
            let h = state.h;
            let l = state.l;
            state.l = state.memory[state.sp as usize];
            state.h = state.memory[state.sp as usize + 1];
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, state.sp, l);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, state.sp + 1, h);
        }
        0xe5 => {
            // PUSH H
            let h = state.h;
            let l = state.l;
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, state.sp - 2, l);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, state.sp - 1, h);
            state.sp -= 2;
        }
        0xe9 => {
            // PCHL
            let addr = addr_from_reg_pair(state.h, state.l);
            state.pc = addr;
        }
        0xeb => {
            // XCHG
            let lreg = state.l;
            let hreg = state.h;
            state.l = state.e;
            state.h = state.d;
            state.e = lreg;
            state.d = hreg;
        }
        0xf1 => {
            // POP PSW
            let psw = state.memory[state.sp as usize];
            state.a = state.memory[state.sp as usize + 1];
            state.cc.z = (0x01 == (psw & 0x01)) as u8;
            state.cc.s = (0x02 == (psw & 0x02)) as u8;
            state.cc.p = (0x04 == (psw & 0x04)) as u8;
            state.cc.cy = (0x08 == (psw & 0x08)) as u8;
            state.cc.ac = (0x10 == (psw & 0x10)) as u8;
            state.sp += 2;
        }
        0xf3 => {
            // DI
            state.int_enable = 0;
        }
        0xf5 => {
            // PUSH PSW
            state.memory = emu8080_write_mem(
                state.memory,
                &state.ram_region,
                state.sp - 2,
                state.cc.z
                    | state.cc.s << 1
                    | state.cc.p << 2
                    | state.cc.cy << 3
                    | state.cc.ac << 4,
            );
            state.memory =
                emu8080_write_mem(state.memory, &state.ram_region, state.sp - 1, state.a);
            state.sp -= 2;
        }
        0xf9 => {
            // SPHL
            let addr = addr_from_reg_pair(state.h, state.l);
            state.sp = addr;
        }
        0xfb => {
            // EI
            state.int_enable = 1;
        }
        _ => {
            let end = debug::index_instruction_containing_addr(&state.assembly[..], state.pc - 1);
            debug::dump_assembly(&state.assembly[end - 5..=end]);
            panic!(
                "Unimplemented Op code {:#04x} at addr: {:#04x}",
                op,
                state.pc - 1
            )
        }
    };
    state
}

fn emu8080_add(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = a + src;
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_inx(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x03 => {
            // INX B
            let mut bc = (state.b as u16) << 8;
            bc |= state.c as u16;
            let answer = bc + 1;
            state.b = (answer >> 8) as u8;
            state.c = answer as u8;
        }
        0x13 => {
            // INX D
            let mut de = (state.d as u16) << 8;
            de |= state.e as u16;
            let answer = de + 1;
            state.d = (answer >> 8) as u8;
            state.e = answer as u8;
        }
        0x23 => {
            // INX H
            let mut hl = (state.h as u16) << 8;
            hl |= state.l as u16;
            let answer = hl + 1;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
        }
        0x33 => {
            // INX SP
            let sp = state.sp;
            let answer = sp + 1;
            state.sp = answer;
        }
        _ => panic!("Unimplemented INX Op code {:#04x}", op),
    }
    state
}

fn emu8080_dcx(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x0b => {
            // DCX B
            let mut bc = (state.b as u16) << 8;
            bc |= state.c as u16;
            let answer = do_sub(bc, 1);
            state.b = (answer >> 8) as u8;
            state.c = answer as u8;
        }
        0x1b => {
            // DCX D
            let mut de = (state.d as u16) << 8;
            de |= state.e as u16;
            let answer = do_sub(de, 1);
            state.d = (answer >> 8) as u8;
            state.e = answer as u8;
        }
        0x2b => {
            // DCX H
            let mut hl = (state.h as u16) << 8;
            hl |= state.l as u16;
            let answer = do_sub(hl, 1);
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
        }
        0x3b => {
            // DCX SP
            let sp = state.sp;
            let answer = do_sub(sp, 1);
            state.sp = answer;
        }
        _ => panic!("Unimplemented DCX Op code {:#04x}", op),
    }
    state
}

fn emu8080_inr(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x04 => {
            // INR B
            let b = state.b as u16;
            let answer = b + 1;
            state.b = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x0c => {
            // INR C
            let c = state.c as u16;
            let answer = c + 1;
            state.c = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x14 => {
            // INR D
            let d = state.d as u16;
            let answer = d + 1;
            state.d = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x1c => {
            // INR E
            let e = state.e as u16;
            let answer = e + 1;
            state.e = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x24 => {
            // INR H
            let h = state.h as u16;
            let answer = h + 1;
            state.h = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x2c => {
            // INR L
            let l = state.l as u16;
            let answer = l + 1;
            state.l = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x34 => {
            // INR M
            let addr = addr_from_reg_pair(state.h, state.l);
            let m = state.memory[addr as usize] as u16;
            let answer = m + 1;
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, answer as u8);
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x3c => {
            // INR A
            let a = state.a as u16;
            let answer = a + 1;
            state.a = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        _ => panic!("Unimplemented INR Op code {:#04x}", op),
    }
    state
}

fn emu8080_write_mem(mut memory: Vec<u8>, ram_region: &Range<u16>, addr: u16, val: u8) -> Vec<u8> {
    if addr < ram_region.start {
        return memory;
    }
    if addr > ram_region.end {
        return memory;
    }
    memory[addr as usize] = val;
    memory
}

fn emu8080_dcr(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x05 => {
            // DCR B
            let b = state.b as u16;
            let answer = do_sub(b, 1);
            state.b = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x0d => {
            // DCR C
            let c = state.c as u16;
            let answer = do_sub(c, 1);
            state.c = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x15 => {
            // DCR D
            let d = state.d as u16;
            let answer = do_sub(d, 1);
            state.d = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x1d => {
            // DCR E
            let e = state.e as u16;
            let answer = do_sub(e, 1);
            state.e = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x25 => {
            // DCR H
            let h = state.h as u16;
            let answer = do_sub(h, 1);
            state.h = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x2d => {
            // DCR L
            let l = state.l as u16;
            let answer = do_sub(l, 1);
            state.l = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x35 => {
            // DCR M
            let addr = addr_from_reg_pair(state.h, state.l);
            let m = state.memory[addr as usize] as u16;
            let answer = do_sub(m, 1);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, answer as u8);
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        0x3d => {
            // DCR A
            let a = state.a as u16;
            let answer = do_sub(a, 1);
            state.a = answer as u8;
            state.cc = calc_conditions(state.cc, answer & 0xff);
        }
        _ => panic!("Unimplemented DCR Op code {:#04x}", op),
    }
    state
}

fn emu8080_adc(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let cy = state.cc.cy as u16;
    let answer = a + src + cy;
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_sub(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = do_sub(a, src);
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_sbb(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let cy = state.cc.cy as u16;
    let answer = do_sub(a, src + cy);
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_ana(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = a & src;
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_xra(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = a ^ src;
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_ora(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = a | src;
    state.a = answer as u8;
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_cmp(mut state: State8080, reg_index: u8) -> State8080 {
    let a = state.a as u16;
    let src = emu8080_mov_reg(&state, reg_index) as u16;
    let answer = do_sub(a, src);
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_mvi(mut state: State8080, op: u8) -> State8080 {
    let byte = state.memory[state.pc as usize];
    state.pc += 1;
    match op {
        0x06 => {
            state.b = byte;
        }
        0x0e => {
            state.c = byte;
        }
        0x16 => {
            state.d = byte;
        }
        0x1e => {
            state.e = byte;
        }
        0x26 => {
            state.h = byte;
        }
        0x2e => {
            state.l = byte;
        }
        0x36 => {
            let addr = addr_from_reg_pair(state.h, state.l);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, byte);
        }
        0x3e => {
            state.a = byte;
        }
        _ => panic!("Unimplemented MVI Op code {:#04x}", op),
    }
    state
}

fn emu8080_immediate(mut state: State8080, op: u8) -> State8080 {
    let a = state.a as u16;
    let byte = state.memory[state.pc as usize] as u16;
    state.pc += 1;
    let answer = match op {
        0xc6 => {
            // ADI D8
            a + byte
        }
        0xce => {
            // ACI D8
            let cy = state.cc.cy as u16;
            a + byte + cy
        }
        0xd6 => {
            // SUI D8
            do_sub(a, byte)
        }
        0xde => {
            // SBI D8
            let cy = state.cc.cy as u16;
            do_sub(a, byte + cy)
        }
        0xe6 => {
            // ANI D8
            a & byte
        }
        0xee => {
            // XRI D8
            a ^ byte
        }
        0xf6 => {
            // ORI D8
            a | byte
        }
        0xfe => {
            // CPI D8  2 Z, S, P, CY, AC A - data
            do_sub(a, byte)
        }
        _ => panic!("Unimplemented Immediate Op code {:#04x}", op),
    };
    if op != 0xfe {
        state.a = answer as u8;
    }
    state.cc = calc_conditions(state.cc, answer);
    state
}

fn emu8080_call(mut state: State8080, op: u8) -> State8080 {
    let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
    state.pc += 2;
    match op {
        0xc4 => {
            // CNZ addr
            if state.cc.z == 0 {
                state = do_call(state, addr);
            }
        }
        0xcc => {
            // CZ addr
            if state.cc.z == 1 {
                state = do_call(state, addr);
            }
        }
        0xcd => {
            // CALL ${addr}
            state = do_call(state, addr);
        }
        0xd4 => {
            // CNC addr
            if state.cc.cy == 0 {
                state = do_call(state, addr);
            }
        }
        0xdc => {
            // CC addr
            if state.cc.cy == 1 {
                state = do_call(state, addr);
            }
        }
        0xe4 => {
            // CPO addr
            if state.cc.p == 0 {
                state = do_call(state, addr);
            }
        }
        0xec => {
            // CPE addr
            if state.cc.p == 1 {
                state = do_call(state, addr);
            }
        }
        0xf4 => {
            // CP addr
            if state.cc.s == 0 {
                state = do_call(state, addr);
            }
        }
        0xfc => {
            // CM addr
            if state.cc.s == 1 {
                state = do_call(state, addr);
            }
        }
        _ => panic!("Unimplemented CALL Op code {:#04x}", op),
    }
    state
}

fn emu8080_jmp(mut state: State8080, op: u8) -> State8080 {
    let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
    state.pc += 2;
    match op {
        0xc2 => {
            // JNZ addr
            if state.cc.z == 0 {
                state.pc = addr;
            }
        }
        0xc3 => {
            // JMP ${addr}
            state.pc = addr;
        }
        0xca => {
            // JZ addr
            if state.cc.z == 1 {
                state.pc = addr;
            }
        }
        0xd2 => {
            // JNC addr
            if state.cc.cy == 0 {
                state.pc = addr;
            }
        }
        0xda => {
            // JC addr
            if state.cc.cy == 1 {
                state.pc = addr;
            }
        }
        0xe2 => {
            // JPO addr
            if state.cc.p == 0 {
                state.pc = addr;
            }
        }
        0xea => {
            // JPE addr
            if state.cc.p == 1 {
                state.pc = addr;
            }
        }
        0xf2 => {
            // JP addr
            if state.cc.s == 0 {
                state.pc = addr;
            }
        }
        0xfa => {
            // JM addr
            if state.cc.s == 1 {
                state.pc = addr;
            }
        }
        _ => panic!("Unimplemented JMP Op code {:#04x}", op),
    }
    state
}

fn emu8080_ret(mut state: State8080, op: u8) -> State8080 {
    match op {
        0xc0 => {
            // RNZ
            if state.cc.z == 0 {
                state = do_return(state);
            }
        }
        0xc8 => {
            // RZ
            if state.cc.z == 1 {
                state = do_return(state);
            }
        }
        0xc9 => {
            // RET
            state = do_return(state);
        }
        0xd0 => {
            // RNC
            if state.cc.cy == 0 {
                state = do_return(state);
            }
        }
        0xd8 => {
            // RC
            if state.cc.cy == 1 {
                state = do_return(state);
            }
        }
        0xe0 => {
            // RPO
            if state.cc.p == 0 {
                state = do_return(state);
            }
        }
        0xe8 => {
            // RPE
            if state.cc.p == 1 {
                state = do_return(state);
            }
        }
        0xf0 => {
            // RP
            if state.cc.s == 0 {
                state = do_return(state);
            }
        }
        0xf8 => {
            // RM
            if state.cc.s == 1 {
                state = do_return(state);
            }
        }
        _ => panic!("Unimplemented RET Op code {:#04x}", op),
    }
    state
}

fn do_rst(state: State8080, op: u8) -> State8080 {
    let addr = ((op >> 3) & 0x7) * 8;
    do_call(state, addr as u16)
}

fn emu8080_mov(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x40..=0x47 => {
            // MOV B, *
            state.b = emu8080_mov_reg(&state, op - 0x40);
        }
        0x48..=0x4f => {
            // MOV C, *
            state.c = emu8080_mov_reg(&state, op - 0x48);
        }
        0x50..=0x57 => {
            // MOV D, *
            state.d = emu8080_mov_reg(&state, op - 0x50);
        }
        0x58..=0x5f => {
            // MOV E, *
            state.e = emu8080_mov_reg(&state, op - 0x58);
        }
        0x60..=0x67 => {
            // MOV H, *
            state.h = emu8080_mov_reg(&state, op - 0x60);
        }
        0x68..=0x6f => {
            // MOV L, *
            state.l = emu8080_mov_reg(&state, op - 0x68);
        }
        0x70..=0x77 => {
            // MOV M, *
            let addr = addr_from_reg_pair(state.h, state.l);
            let val = emu8080_mov_reg(&state, op - 0x70);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, val);
        }
        0x78..=0x7f => {
            // MOV A, *
            state.a = emu8080_mov_reg(&state, op - 0x78);
        }
        _ => panic!("Unimplemented MOV Op code {:#04x}", op),
    }

    state
}

fn emu8080_store(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x02 => {
            // STAX B
            let a = state.a;
            let addr = addr_from_reg_pair(state.b, state.c);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, a);
        }
        0x12 => {
            // STAX D
            let a = state.a;
            let addr = addr_from_reg_pair(state.d, state.e);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, a);
        }
        0x22 => {
            // SHLD ${addr}
            let h = state.h;
            let l = state.l;
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, l);
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr + 1, h);
        }
        0x32 => {
            // STA adr
            let a = state.a;
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state.memory = emu8080_write_mem(state.memory, &state.ram_region, addr, a);
        }
        _ => panic!("Unimplemented STORE Op code {:#04x}", op),
    }
    state
}

fn emu8080_load(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x0a => {
            // LDAX B
            let addr = addr_from_reg_pair(state.b, state.c);
            state.a = state.memory[addr as usize];
        }
        0x1a => {
            // LDAX D
            let addr = addr_from_reg_pair(state.d, state.e);
            state.a = state.memory[addr as usize];
        }
        0x2a => {
            // LHLD ${addr}
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory) as usize;
            state.pc += 2;
            state.l = state.memory[addr];
            state.h = state.memory[addr + 1];
        }
        0x3a => {
            // LDA adr
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state.a = state.memory[addr as usize];
        }
        _ => panic!("Unimplemented LOAD Op code {:#04x}", op),
    }
    state
}

fn emu8080_mov_reg(state: &State8080, reg_index: u8) -> u8 {
    match reg_index {
        0x0 => {
            // MOV _, B
            state.b
        }
        0x1 => {
            // MOV _, C
            state.c
        }
        0x2 => {
            // MOV _, D
            state.d
        }
        0x3 => {
            // MOV _, E
            state.e
        }
        0x4 => {
            // MOV _, H
            state.h
        }
        0x5 => {
            // MOV _, L
            state.l
        }
        0x6 => {
            // MOV _, M
            let addr = addr_from_reg_pair(state.h, state.l);
            state.memory[addr as usize]
        }
        0x7 => {
            // MOV _, A
            state.a
        }
        _ => panic!("Non-existent reg index {:#04x}", reg_index),
    }
}

fn do_sub(mut a: u16, b: u16) -> u16 {
    if b > a {
        a += 0x1000;
    }
    a - b
}

fn do_call(mut state: State8080, addr: u16) -> State8080 {
    state.memory = emu8080_write_mem(
        state.memory,
        &state.ram_region,
        state.sp - 1,
        (state.pc >> 8) as u8,
    );
    state.memory = emu8080_write_mem(
        state.memory,
        &state.ram_region,
        state.sp - 2,
        state.pc as u8,
    );
    state.sp -= 2;
    state.pc = addr;
    state
}
fn do_return(mut state: State8080) -> State8080 {
    let addr = get_addr_from_bytes(state.sp as usize, &state.memory);
    state.pc = addr;
    state.sp += 2;
    state
}
fn calc_conditions(mut cc: ConditionCodes, answer: u16) -> ConditionCodes {
    cc.z = (answer & 0xff == 0) as u8;
    cc.s = (answer & 0x80 != 0) as u8;
    cc.cy = (answer > 0xff) as u8;
    cc.p = calc_parity((answer & 0xff) as u8);
    cc
}

fn addr_from_reg_pair(hi: u8, lo: u8) -> u16 {
    ((hi as u16) << 8) | lo as u16
}

fn calc_parity(val: u8) -> u8 {
    let mut one_bits = 0;
    for x in 0..8 {
        if (val >> x) & 1 == 1 {
            one_bits += 1;
        }
    }
    (one_bits % 2 == 0) as u8
}

pub fn get_addr_from_bytes(i: usize, memory: &Vec<u8>) -> u16 {
    let mut addr: u16 = memory[i + 1] as u16;
    addr = addr << 8;
    addr |= memory[i] as u16;
    addr
}
