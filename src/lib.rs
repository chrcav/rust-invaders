use std::io::prelude::*;

#[derive(Default)]
pub struct ConditionCodes {
    z: u8,
    s: u8,
    p: u8,
    cy: u8,
    ac: u8,
    pad: u8,
}

impl std::fmt::Display for ConditionCodes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            r#"z {:#04x} cy {:#04x}
s {:#04x} p {:#04x}
"#,
            self.z, self.cy, self.s, self.p,
        )
    }
}

#[derive(Default)]
pub struct State8080 {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pub pc: u16,
    memory: Vec<u8>,
    cc: ConditionCodes,
    int_enable: u8,
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
            cc: ConditionCodes::default(),
            int_enable: 0,
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

pub fn create_initial_emustate(filename: &str, start_pc: u16) -> std::io::Result<State8080> {
    let mut f = std::fs::File::open(filename)?;
    let mut state = State8080::new();
    // read the whole file
    f.read_to_end(&mut state.memory)?;
    state.memory.resize_with(65535, Default::default);
    state.pc = start_pc;
    Ok(state)
}

pub fn emu8080(mut state: State8080) -> std::io::Result<()> {
    loop {
        let op = state.memory[state.pc as usize];
        state.pc += 1;
        match op {
            _ => {
                state = emu8080_opcode(state, op);
            }
        };
        println!("{:02x} {}", op, state);
        if state.pc as usize >= state.memory.len() {
            break;
        }
    }
    Ok(())
}

fn emu8080_opcode(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x00 => {} // NOP
        0x01 => {
            // LXI b,D16
            state.b = state.memory[state.pc as usize + 1];
            state.c = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x04 => {
            //INR B
            let answer = state.b as u16 + 1;
            state.b = answer as u8;
            state.cc = calc_conditions(state.cc, answer);
        }
        0x05 => {
            //DCR B
            let answer = do_sub(state.b as u16, 1);
            state.b = answer as u8;
            state.cc = calc_conditions(state.cc, answer);
        }
        0x06 => {
            // MVI B, D8
            state.b = state.memory[state.pc as usize];
            state.pc += 1;
        }
        0x09 => {
            // DAD B
            let bc = (state.b as u32) << 8 | state.c as u32;
            let hl = (state.h as u32) << 8 | state.l as u32;
            let answer = hl + bc;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x0d => {
            // DCR C
            let answer = if state.c == 0 {
                255 as u16
            } else {
                state.c as u16 - 1
            };
            state.cc.z = (answer & 0xff == 0) as u8;
            state.cc.s = (answer & 0x08 != 1) as u8;
            //state.cc.cy = (answer > 0xff) as u8;
            state.cc.p = calc_parity((answer & 0xff) as u8);
            state.c = answer as u8;
        }
        0x0e => {
            // MVI C,D8
            state.c = state.memory[state.pc as usize];
            state.pc += 1;
        }
        0x0f => {
            // RRC
            let bit0 = 0x1 & state.a;
            state.a = state.a >> 1;
            state.a = state.a | bit0 << 7;
            state.cc.cy = bit0;
        }
        0x11 => {
            // LXI D, D16
            state.d = state.memory[state.pc as usize + 1];
            state.e = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x13 => {
            //INX D
            let mut answer = (state.d as u16) << 8;
            answer |= state.e as u16;
            answer += 1;
            state.e = answer as u8;
            state.d = (answer >> 8) as u8
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
        0x1a => {
            // LDAX D
            let mut addr = (state.d as u16) << 8;
            addr |= state.e as u16;
            state.a = state.memory[addr as usize];
        }
        0x21 => {
            // LXI H, D16
            state.h = state.memory[state.pc as usize + 1];
            state.l = state.memory[state.pc as usize];
            state.pc += 2;
        }
        0x23 => {
            //INX H
            state.h = if state.h == 255 { 0 } else { state.h + 1 }
        }
        0x26 => {
            // MVI H,D8
            state.h = state.memory[state.pc as usize];
            state.pc += 1;
        }
        0x29 => {
            // DAD H
            let hl = (state.h as u32) << 8 | state.l as u32;
            let answer = hl + hl;
            state.h = (answer >> 8) as u8;
            state.l = answer as u8;
            state.cc.cy = (answer > 0xff) as u8;
        }
        0x31 => {
            // LXI SP, D16
            state.sp = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
        }
        0x32 => {
            // STA adr
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.memory[addr as usize] = state.a;
            state.pc += 2;
        }
        0x36 => {
            // MVI M, D8
            state.memory[addr_from_reg_pair(state.h, state.l) as usize] =
                state.memory[state.pc as usize];
            state.pc += 1;
        }
        0x3a => {
            // LDA adr
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.a = state.memory[addr as usize];
            state.pc += 2;
        }
        0x3e => {
            // MVI A, D8
            state.a = state.memory[state.pc as usize];
            state.pc += 1;
        }
        0x40..=0x75 | 0x77..=0x7f => {
            state = emu8080_mov(state, op);
        }
        0xa7 => {
            // ANA A
            let answer = state.a as u16 & state.a as u16;
            state.a = answer as u8;
            state.cc.z = (answer & 0xff == 0) as u8;
            state.cc.s = (answer & 0x08 != 1) as u8;
            state.cc.cy = (answer > 0xff) as u8;
            state.cc.p = calc_parity((answer & 0xff) as u8);
        }
        0xaf => {
            // XRA A
            let answer = state.a as u16 ^ state.a as u16;
            state.a = answer as u8;
            state.cc.z = (answer & 0xff == 0) as u8;
            state.cc.s = (answer & 0x08 != 1) as u8;
            state.cc.cy = (answer > 0xff) as u8;
            state.cc.p = calc_parity((answer & 0xff) as u8);
        }
        0xc1 => {
            // POP B
            state.c = state.memory[state.sp as usize];
            state.b = state.memory[state.sp as usize + 1];
            state.sp += 2;
        }
        0xc2 => {
            // JNZ addr
            if state.cc.z == 0 {
                state.pc = get_addr_from_bytes(state.pc as usize, &state.memory);
            } else {
                state.pc += 2;
            }
        }
        0xc3 => {
            // JMP ${addr}
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc = addr;
        }
        0xc5 => {
            // PUSH B
            state.memory[state.sp as usize - 2] = state.c;
            state.memory[state.sp as usize - 1] = state.b;
            state.sp -= 2;
        }
        0xc6 => {
            // ADI D8
            let a = state.a as u16;
            let byte = state.memory[state.pc as usize] as u16;
            let answer = a + byte;
            state.a = answer as u8;
            state.cc = calc_conditions(state.cc, answer);
        }
        0xc9 => {
            // RET
            state = do_return(state);
        }
        0xcd => {
            // CALL ${addr}
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state = do_call(state, addr);
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
            state.memory[state.sp as usize - 2] = state.e;
            state.memory[state.sp as usize - 1] = state.d;
            state.sp -= 2;
        }
        0xe1 => {
            // POP H
            state.l = state.memory[state.sp as usize];
            state.h = state.memory[state.sp as usize + 1];
            state.sp += 2;
        }
        0xe5 => {
            // PUSH H
            state.memory[state.sp as usize - 2] = state.l;
            state.memory[state.sp as usize - 1] = state.h;
            state.sp -= 2;
        }
        0xe6 => {
            // ANI D8
            let answer = (state.memory[state.pc as usize] & state.a);
            state.pc += 1;
            state.a = answer as u8;
            state.cc = calc_conditions(state.cc, answer as u16);
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
            state.memory[state.sp as usize - 2] = state.cc.z
                | state.cc.s << 1
                | state.cc.p << 2
                | state.cc.cy << 3
                | state.cc.ac << 4;
            state.memory[state.sp as usize - 1] = state.a;
            state.sp -= 2;
        }
        0xfb => {
            // EI
            state.int_enable = 1;
        }
        0xfe => {
            // CPI D8  2 Z, S, P, CY, AC A - data
            let a = state.a as u16;
            let data = state.memory[state.pc as usize] as u16;
            state.pc += 1;
            let answer = do_sub(a, data);
            state.cc = calc_conditions(state.cc, answer);
        }
        _ => panic!("Unimplemented Op code {:#04x}", op),
    };
    state
}

fn emu8080_mov(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x56 => {
            // MOV D, M
            state.d = emu8080_mov_reg(&state, op - 0x50);
        }
        0x5e => {
            // MOV E, M
            state.e = emu8080_mov_reg(&state, op - 0x58);
        }
        0x66 => {
            // MOV H, M
            state.h = emu8080_mov_reg(&state, op - 0x60);
        }
        0x6f => {
            // MOV L, A
            state.l = emu8080_mov_reg(&state, op - 0x68);
        }
        0x77 => {
            // MOV M, A
            let mut addr = (state.h as u16) << 8;
            addr |= state.l as u16;
            state.memory[addr as usize] = emu8080_mov_reg(&state, op - 0x70);
        }
        0x78..=0x7f => {
            // MOV A, *
            state.a = emu8080_mov_reg(&state, op - 0x78);
        }
        _ => panic!("Unimplemented MOV Op code {:#04x}", op),
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
            let mut addr = (state.h as u16) << 8;
            addr |= state.l as u16;
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
    state.memory[state.sp as usize - 1] = (state.pc >> 8) as u8;
    state.memory[state.sp as usize - 2] = state.pc as u8;
    state.sp -= 2;
    state.pc = addr;
    state
}
fn do_return(mut state: State8080) -> State8080 {
    let mut addr = (state.memory[(state.sp as usize) + 1] as u16) << 8;
    addr |= state.memory[state.sp as usize] as u16;
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
    one_bits / 2
}

fn get_addr_from_bytes(i: usize, memory: &Vec<u8>) -> u16 {
    let mut addr: u16 = memory[i + 1] as u16;
    addr = addr << 8;
    addr |= memory[i] as u16;
    addr
}
