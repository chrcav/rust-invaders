#[cfg(test)]
mod test;
extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Texture;
use sdl2::surface::Surface;
use std::io::prelude::*;
use std::path::Path;
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
    pad: u8,
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
    pc: u16,
    pub memory: Vec<u8>,
    cc: ConditionCodes,
    int_enable: u8,
    program_len: usize,
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
            program_len: 0,
        }
    }
}

#[derive(Default)]
pub struct Machine8080 {
    pause: u8,
    shift_offset: u8,
    shift0: u8,
    shift1: u8,
}

impl Machine8080 {
    pub fn new() -> Self {
        Self {
            pause: 0,
            shift_offset: 0,
            shift0: 0,
            shift1: 0,
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
    state
        .memory
        .resize_with(start_pc as usize, Default::default);
    // read the whole file
    f.read_to_end(&mut state.memory)?;
    state.program_len = state.memory.len();
    state.memory.resize_with(65535, Default::default);
    state.pc = start_pc;
    Ok(state)
}

pub fn emu8080(mut state: State8080) -> std::io::Result<()> {
    let mut machine = Machine8080::new();
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("rust-sdl2 demo", 1024, 896)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut last_interrupt = Instant::now();
    let mut last_op = Instant::now();
    'emu: loop {
        let now = Instant::now();
        let time_since_last_op = now.duration_since(last_op);
        if time_since_last_op < Duration::new(0, 500) {
            thread::sleep(Duration::new(0, 500) - time_since_last_op);
        }

        let mut event_pump = sdl_context.event_pump().unwrap();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'emu,
                Event::KeyDown {
                    keycode: Some(Keycode::P),
                    ..
                } => machine.pause = (machine.pause ^ 0x1) & 0x1,
                _ => {}
            }
        }

        if machine.pause == 0x1 {
            continue 'emu;
        }

        last_op = Instant::now();
        let now = Instant::now();
        if now.duration_since(last_interrupt) > Duration::new(0, 16666667) {
            if state.int_enable == 0x1 {
                state = generate_interupt(state, 2);
                canvas.clear();
                // The rest of the game loop goes here...
                let texture_creator = canvas.texture_creator();

                let mut data = pixeldata_from_memory(&state.memory, 0x2400, 0x3fff);
                let surface =
                    Surface::from_data(&mut data[..], 256, 224, 256 * 3, PixelFormatEnum::RGB24)
                        .unwrap();
                surface
                    .save_bmp(Path::new("./invaders.bmp"))
                    .expect("unable to write out bmp");
                let texture = Texture::from_surface(&surface, &texture_creator).unwrap();
                canvas
                    .copy(&texture, None, None)
                    .expect("couldn't render texture");
                canvas.present();
                last_interrupt = Instant::now();
            }
        }

        let op = state.memory[state.pc as usize];
        /*println!(
            "start state:\n{:02x} {} {}flags:\n{}",
            op,
            command_format(&state.memory, state.pc as usize, op),
            state,
            state.cc
        );*/
        state.pc += 1;
        match op {
            0xd3 => {
                // OUT d8
                let port = state.memory[state.pc as usize];
                state.pc += 1;
                machine = machine_output(machine, port, state.a);
            }
            0xdb => {
                // IN d8
                let port = state.memory[state.pc as usize];
                state.pc += 1;
                state.a = machine_input(port, machine.shift0, machine.shift1, machine.shift_offset);
            }
            _ => {
                state = emu8080_opcode(state, op);
            }
        };
        //println!("end state:\n{}flags:\n{}", state, state.cc);
        if state.pc as usize >= state.memory.len() {
            break;
        }
    }
    Ok(())
}

fn pixeldata_from_memory(memory: &Vec<u8>, start: u16, end: u16) -> Vec<u8> {
    let mut data = Vec::new();
    let start_hi = start >> 8;
    let end_hi = end >> 8;
    let start_lo = start & 0xff;
    let end_lo = end & 0xff;
    for hi in start_hi..=end_hi {
        for lo in start_lo..=end_lo {
            //println!("{:04x} {:02x}", i, memory[i as usize]);
            let addr = (hi as u16) << 8 | (lo as u16);
            for bit in 0..=7 {
                let mut pixel = if (memory[addr as usize] & (1 << bit)) != 0x0 {
                    vec![255, 255, 255]
                } else {
                    vec![0, 0, 0]
                };
                data.append(&mut pixel);
            }
        }
    }
    data
}

fn machine_input(port: u8, shift0: u8, shift1: u8, shift_offset: u8) -> u8 {
    match port {
        0x01 => 0x01,
        0x02 => 0x00,
        0x03 => {
            let val = ((shift1 as u16) << 8) | (shift0 as u16);
            ((val >> (8 - shift_offset)) & 0xff) as u8
        }
        _ => panic!("invalid port {}", port),
    }
}

fn machine_output(mut machine: Machine8080, port: u8, value: u8) -> Machine8080 {
    match port {
        0x02 => {
            machine.shift_offset = value & 0x7;
        }
        0x04 => {
            machine.shift0 = machine.shift1;
            machine.shift1 = value;
        }
        0x03 | 0x05 | 0x06 => {}
        _ => panic!("invalid port {}", port),
    }
    machine
}

fn generate_interupt(state: State8080, interupt_num: u16) -> State8080 {
    do_call(state, interupt_num * 8)
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
            // TODO: check this
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
            // TODO: check this
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
            // TODO: check this
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
            // TODO: check this
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
            state.memory[state.sp as usize - 2] = state.c;
            state.memory[state.sp as usize - 1] = state.b;
            state.sp -= 2;
        }
        0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe => {
            // ADI D8
            state = emu8080_immediate(state, op);
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
        0xe3 => {
            // XTHL
            let h = state.h;
            let l = state.l;
            state.l = state.memory[state.sp as usize];
            state.h = state.memory[state.sp as usize + 1];
            state.memory[state.sp as usize] = l;
            state.memory[state.sp as usize + 1] = h;
        }
        0xe5 => {
            // PUSH H
            state.memory[state.sp as usize - 2] = state.l;
            state.memory[state.sp as usize - 1] = state.h;
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
            state.memory[state.sp as usize - 2] = state.cc.z
                | state.cc.s << 1
                | state.cc.p << 2
                | state.cc.cy << 3
                | state.cc.ac << 4;
            state.memory[state.sp as usize - 1] = state.a;
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
        _ => panic!("Unimplemented Op code {:#04x}", op),
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
            state.memory[addr as usize] = answer as u8;
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
            state.memory[addr as usize] = answer as u8;
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
            state.memory[addr as usize] = byte;
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

fn emu8080_store(mut state: State8080, op: u8) -> State8080 {
    match op {
        0x02 => {
            // STAX B
            let addr = addr_from_reg_pair(state.b, state.c);
            state.memory[addr as usize] = state.a;
        }
        0x12 => {
            // STAX D
            let addr = addr_from_reg_pair(state.d, state.e);
            state.memory[addr as usize] = state.a;
        }
        0x22 => {
            // SHLD ${addr}
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state.memory[addr as usize] = state.l;
            state.memory[addr as usize + 1] = state.h;
        }
        0x32 => {
            // STA adr
            let addr = get_addr_from_bytes(state.pc as usize, &state.memory);
            state.pc += 2;
            state.memory[addr as usize] = state.a;
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
    state.memory[state.sp as usize - 1] = (state.pc >> 8) as u8;
    state.memory[state.sp as usize - 2] = state.pc as u8;
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

fn get_addr_from_bytes(i: usize, memory: &Vec<u8>) -> u16 {
    let mut addr: u16 = memory[i + 1] as u16;
    addr = addr << 8;
    addr |= memory[i] as u16;
    //println!("{:04x}", addr);
    addr
}

fn diassemble() -> std::io::Result<()> {
    let mut f = std::fs::File::open("invaders")?;
    let mut buffer = Vec::new();
    // read the whole file
    f.read_to_end(&mut buffer)?;

    let mut i = 0;
    loop {
        let op = buffer[i];
        let incr = 1;
        let command = command_format(&buffer, i, op);
        println!("{:04x} {:#04x} {}", i, op, command);
        i += incr;
        if i >= buffer.len() {
            break;
        }
    }
    Ok(())
}

pub fn command_format(buffer: &Vec<u8>, pc: usize, op: u8) -> String {
    match op {
        0x00 => format!("NOP 1"),
        0x01 => {
            format!(
                "LXI B,D16 3  B <- {:#04x}, C <- {:#04x}",
                buffer[pc + 2],
                buffer[pc + 1]
            )
        }
        0x02 => format!("STAX B 1  (BC) <- A"),
        0x03 => format!("INX B 1  BC <- BC+1"),
        0x04 => format!("INR B 1 Z, S, P, AC B <- B+1"),
        0x05 => format!("DCR B 1 Z, S, P, AC B <- B-1"),
        0x06 => {
            format!("MVI B, D8 2  B <- {:#04x}", buffer[pc + 1])
        }
        0x07 => format!("RLC 1 CY A = A << 1; bit 0 = prev bit 7; CY = prev bit 7"),
        0x09 => format!("DAD B 1 CY HL = HL + BC"),
        0x0a => format!("LDAX B 1  A <- (BC)"),
        0x0b => format!("DCX B 1  BC = BC-1"),
        0x0c => format!("INR C 1 Z, S, P, AC C <- C+1"),
        0x0d => format!("DCR C 1 Z, S, P, AC C <-C-1"),
        0x0e => {
            format!("MVI C,D8 2  C <- {:#04x}", buffer[pc + 1])
        }
        0x0f => format!("RRC 1 CY A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0"),
        0x11 => {
            format!(
                "LXI D,D16 3  D <- {:#04x}, E <- {:#04x}",
                buffer[pc + 2],
                buffer[pc + 1]
            )
        }
        0x12 => format!("STAX D 1  (DE) <- A"),
        0x13 => format!("INX D 1  DE <- DE + 1"),
        0x14 => format!("INR D 1 Z, S, P, AC D <- D+1"),
        0x15 => format!("DCR D 1 Z, S, P, AC D <- D-1"),
        0x16 => {
            format!("MVI D, D8 2  D <- {:#04x}", buffer[pc + 1])
        }
        0x17 => format!("RAL 1 CY A = A << 1; bit 0 = prev CY; CY = prev bit 7"),
        0x19 => format!("DAD D 1 CY HL = HL + DE"),
        0x1a => format!("LDAX D 1  A <- (DE)"),
        0x1b => format!("DCX D 1  DE = DE-1"),
        0x1c => format!("INR E 1 Z, S, P, AC E <-E+1"),
        0x1d => format!("DCR E 1 Z, S, P, AC E <- E-1"),
        0x1e => {
            format!("MVI E,D8 2  E <- {:#04x}", buffer[pc + 1])
        }
        0x1f => format!("RAR 1 CY A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0"),
        0x20 => format!("RIM 1  special"),
        0x21 => {
            format!(
                "LXI H,D16 3  H <- {:#04x}, L <- {:#04x}",
                buffer[pc + 2],
                buffer[pc + 1]
            )
        }
        0x22 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("SHLD adr 3  ({:#04x}) <-L; ({:#04x}+1)<-H", addr, addr)
        }
        0x23 => format!("INX H 1  HL <- HL + 1"),
        0x24 => format!("INR H 1 Z, S, P, AC H <- H+1"),
        0x25 => format!("DCR H 1 Z, S, P, AC H <- H-1"),
        0x26 => {
            format!("MVI H,D8 2  L <- {:#04x}", buffer[pc + 1])
        }
        0x27 => format!("DAA 1  special"),
        0x29 => format!("DAD H 1 CY HL = HL + HI"),
        0x2a => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("LHLD adr 3  L <- ({:#04x}); H<-({:#04x}+1)", addr, addr)
        }
        0x2b => format!("DCX H 1  HL = HL-1"),
        0x2c => format!("INR L 1 Z, S, P, AC L <- L+1"),
        0x2d => format!("DCR L 1 Z, S, P, AC L <- L-1"),
        0x2e => {
            format!("MVI L, D8 2  L <- {:#04x}", buffer[pc + 1])
        }
        0x2f => format!("CMA 1  A <- !A"),
        0x30 => format!("SIM 1  special"),
        0x31 => {
            format!(
                "LXI SP, D16 3  SP.hi <- {:#04x}, SP.lo <- {:#04x}",
                buffer[pc + 2],
                buffer[pc + 1]
            )
        }
        0x32 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("STA adr 3  ({:#04x}) <- A", addr)
        }
        0x33 => format!("INX SP 1  SP = SP + 1"),
        0x34 => format!("INR M 1 Z, S, P, AC (HL) <- (HL)+1"),
        0x35 => format!("DCR M 1 Z, S, P, AC (HL) <- (HL)-1"),
        0x36 => {
            format!("MVI M,D8 2  (HL) <- {:#04x}", buffer[pc + 1])
        }
        0x37 => format!("STC 1 CY CY = 1"),
        0x39 => format!("DAD SP 1 CY HL = HL + SP"),
        0x3a => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("LDA adr 3  A <- ({:#04x})", addr)
        }
        0x3b => format!("DCX SP 1  SP = SP-1"),
        0x3c => format!("INR A 1 Z, S, P, AC A <- A+1"),
        0x3d => format!("DCR A 1 Z, S, P, AC A <- A-1"),
        0x3e => {
            format!("MVI A,D8 2  A <- {:#04x}", buffer[pc + 1])
        }
        0x3f => format!("CMC 1 CY CY=!CY"),
        0x40 => format!("MOV B,B 1  B <- B"),
        0x41 => format!("MOV B,C 1  B <- C"),
        0x42 => format!("MOV B,D 1  B <- D"),
        0x43 => format!("MOV B,E 1  B <- E"),
        0x44 => format!("MOV B,H 1  B <- H"),
        0x45 => format!("MOV B,L 1  B <- L"),
        0x46 => format!("MOV B,M 1  B <- (HL)"),
        0x47 => format!("MOV B,A 1  B <- A"),
        0x48 => format!("MOV C,B 1  C <- B"),
        0x49 => format!("MOV C,C 1  C <- C"),
        0x4a => format!("MOV C,D 1  C <- D"),
        0x4b => format!("MOV C,E 1  C <- E"),
        0x4c => format!("MOV C,H 1  C <- H"),
        0x4d => format!("MOV C,L 1  C <- L"),
        0x4e => format!("MOV C,M 1  C <- (HL)"),
        0x4f => format!("MOV C,A 1  C <- A"),
        0x50 => format!("MOV D,B 1  D <- B"),
        0x51 => format!("MOV D,C 1  D <- C"),
        0x52 => format!("MOV D,D 1  D <- D"),
        0x53 => format!("MOV D,E 1  D <- E"),
        0x54 => format!("MOV D,H 1  D <- H"),
        0x55 => format!("MOV D,L 1  D <- L"),
        0x56 => format!("MOV D,M 1  D <- (HL)"),
        0x57 => format!("MOV D,A 1  D <- A"),
        0x58 => format!("MOV E,B 1  E <- B"),
        0x59 => format!("MOV E,C 1  E <- C"),
        0x5a => format!("MOV E,D 1  E <- D"),
        0x5b => format!("MOV E,E 1  E <- E"),
        0x5c => format!("MOV E,H 1  E <- H"),
        0x5d => format!("MOV E,L 1  E <- L"),
        0x5e => format!("MOV E,M 1  E <- (HL)"),
        0x5f => format!("MOV E,A 1  E <- A"),
        0x60 => format!("MOV H,B 1  H <- B"),
        0x61 => format!("MOV H,C 1  H <- C"),
        0x62 => format!("MOV H,D 1  H <- D"),
        0x63 => format!("MOV H,E 1  H <- E"),
        0x64 => format!("MOV H,H 1  H <- H"),
        0x65 => format!("MOV H,L 1  H <- L"),
        0x66 => format!("MOV H,M 1  H <- (HL)"),
        0x67 => format!("MOV H,A 1  H <- A"),
        0x68 => format!("MOV L,B 1  L <- B"),
        0x69 => format!("MOV L,C 1  L <- C"),
        0x6a => format!("MOV L,D 1  L <- D"),
        0x6b => format!("MOV L,E 1  L <- E"),
        0x6c => format!("MOV L,H 1  L <- H"),
        0x6d => format!("MOV L,L 1  L <- L"),
        0x6e => format!("MOV L,M 1  L <- (HL)"),
        0x6f => format!("MOV L,A 1  L <- A"),
        0x70 => format!("MOV M,B 1  (HL) <- B"),
        0x71 => format!("MOV M,C 1  (HL) <- C"),
        0x72 => format!("MOV M,D 1  (HL) <- D"),
        0x73 => format!("MOV M,E 1  (HL) <- E"),
        0x74 => format!("MOV M,H 1  (HL) <- H"),
        0x75 => format!("MOV M,L 1  (HL) <- L"),
        0x76 => format!("HLT 1  special"),
        0x77 => format!("MOV M,A 1  (HL) <- C"),
        0x78 => format!("MOV A,B 1  A <- B"),
        0x79 => format!("MOV A,C 1  A <- C"),
        0x7a => format!("MOV A,D 1  A <- D"),
        0x7b => format!("MOV A,E 1  A <- E"),
        0x7c => format!("MOV A,H 1  A <- H"),
        0x7d => format!("MOV A,L 1  A <- L"),
        0x7e => format!("MOV A,M 1  A <- (HL)"),
        0x7f => format!("MOV A,A 1  A <- A"),
        0x80 => format!("ADD B 1 Z, S, P, CY, AC A <- A + B"),
        0x81 => format!("ADD C 1 Z, S, P, CY, AC A <- A + C"),
        0x82 => format!("ADD D 1 Z, S, P, CY, AC A <- A + D"),
        0x83 => format!("ADD E 1 Z, S, P, CY, AC A <- A + E"),
        0x84 => format!("ADD H 1 Z, S, P, CY, AC A <- A + H"),
        0x85 => format!("ADD L 1 Z, S, P, CY, AC A <- A + L"),
        0x86 => format!("ADD M 1 Z, S, P, CY, AC A <- A + (HL)"),
        0x87 => format!("ADD A 1 Z, S, P, CY, AC A <- A + A"),
        0x88 => format!("ADC B 1 Z, S, P, CY, AC A <- A + B + CY"),
        0x89 => format!("ADC C 1 Z, S, P, CY, AC A <- A + C + CY"),
        0x8a => format!("ADC D 1 Z, S, P, CY, AC A <- A + D + CY"),
        0x8b => format!("ADC E 1 Z, S, P, CY, AC A <- A + E + CY"),
        0x8c => format!("ADC H 1 Z, S, P, CY, AC A <- A + H + CY"),
        0x8d => format!("ADC L 1 Z, S, P, CY, AC A <- A + L + CY"),
        0x8e => format!("ADC M 1 Z, S, P, CY, AC A <- A + (HL) + CY"),
        0x8f => format!("ADC A 1 Z, S, P, CY, AC A <- A + A + CY"),
        0x90 => format!("SUB B 1 Z, S, P, CY, AC A <- A - B"),
        0x91 => format!("SUB C 1 Z, S, P, CY, AC A <- A - C"),
        0x92 => format!("SUB D 1 Z, S, P, CY, AC A <- A + D"),
        0x93 => format!("SUB E 1 Z, S, P, CY, AC A <- A - E"),
        0x94 => format!("SUB H 1 Z, S, P, CY, AC A <- A + H"),
        0x95 => format!("SUB L 1 Z, S, P, CY, AC A <- A - L"),
        0x96 => format!("SUB M 1 Z, S, P, CY, AC A <- A + (HL)"),
        0x97 => format!("SUB A 1 Z, S, P, CY, AC A <- A - A"),
        0x98 => format!("SBB B 1 Z, S, P, CY, AC A <- A - B - CY"),
        0x99 => format!("SBB C 1 Z, S, P, CY, AC A <- A - C - CY"),
        0x9a => format!("SBB D 1 Z, S, P, CY, AC A <- A - D - CY"),
        0x9b => format!("SBB E 1 Z, S, P, CY, AC A <- A - E - CY"),
        0x9c => format!("SBB H 1 Z, S, P, CY, AC A <- A - H - CY"),
        0x9d => format!("SBB L 1 Z, S, P, CY, AC A <- A - L - CY"),
        0x9e => format!("SBB M 1 Z, S, P, CY, AC A <- A - (HL) - CY"),
        0x9f => format!("SBB A 1 Z, S, P, CY, AC A <- A - A - CY"),
        0xa0 => format!("ANA B 1 Z, S, P, CY, AC A <- A & B"),
        0xa1 => format!("ANA C 1 Z, S, P, CY, AC A <- A & C"),
        0xa2 => format!("ANA D 1 Z, S, P, CY, AC A <- A & D"),
        0xa3 => format!("ANA E 1 Z, S, P, CY, AC A <- A & E"),
        0xa4 => format!("ANA H 1 Z, S, P, CY, AC A <- A & H"),
        0xa5 => format!("ANA L 1 Z, S, P, CY, AC A <- A & L"),
        0xa6 => format!("ANA M 1 Z, S, P, CY, AC A <- A & (HL)"),
        0xa7 => format!("ANA A 1 Z, S, P, CY, AC A <- A & A"),
        0xa8 => format!("XRA B 1 Z, S, P, CY, AC A <- A ^ B"),
        0xa9 => format!("XRA C 1 Z, S, P, CY, AC A <- A ^ C"),
        0xaa => format!("XRA D 1 Z, S, P, CY, AC A <- A ^ D"),
        0xab => format!("XRA E 1 Z, S, P, CY, AC A <- A ^ E"),
        0xac => format!("XRA H 1 Z, S, P, CY, AC A <- A ^ H"),
        0xad => format!("XRA L 1 Z, S, P, CY, AC A <- A ^ L"),
        0xae => format!("XRA M 1 Z, S, P, CY, AC A <- A ^ (HL)"),
        0xaf => format!("XRA A 1 Z, S, P, CY, AC A <- A ^ A"),
        //ORA
        0xb0 => format!("ORA B 1 Z, S, P, CY, AC A <- A | B"),
        0xb1 => format!("ORA C 1 Z, S, P, CY, AC A <- A | C"),
        0xb2 => format!("ORA D 1 Z, S, P, CY, AC A <- A | D"),
        0xb3 => format!("ORA E 1 Z, S, P, CY, AC A <- A | E"),
        0xb4 => format!("ORA H 1 Z, S, P, CY, AC A <- A | H"),
        0xb5 => format!("ORA L 1 Z, S, P, CY, AC A <- A | L"),
        0xb6 => format!("ORA M 1 Z, S, P, CY, AC A <- A | (HL)"),
        0xb7 => format!("ORA A 1 Z, S, P, CY, AC A <- A | A"),
        // CMP
        0xb8 => format!("CMP B 1 Z, S, P, CY, AC A - B"),
        0xb9 => format!("CMP C 1 Z, S, P, CY, AC A - C"),
        0xba => format!("CMP D 1 Z, S, P, CY, AC A - D"),
        0xbb => format!("CMP E 1 Z, S, P, CY, AC A - E"),
        0xbc => format!("CMP H 1 Z, S, P, CY, AC A - H"),
        0xbd => format!("CMP L 1 Z, S, P, CY, AC A - L"),
        0xbe => format!("CMP M 1 Z, S, P, CY, AC A - (HL)"),
        0xbf => format!("CMP A 1 Z, S, P, CY, AC A - A"),
        0xc0 => format!("RNZ 1  if NZ, RET"),
        0xc1 => format!("POP B 1  C <- (sp); B <- (sp+1); sp <- sp+2"),
        0xc2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JNZ adr 3  if NZ, PC <- {:#04x}", addr)
        }
        0xc3 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JMP adr 3  PC <= {:#04x}", addr)
        }
        0xc4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CNZ adr 3  if NZ, CALL {:#04x}", addr)
        }
        0xc5 => format!("PUSH B 1  (sp-2)<-C; (sp-1)<-B; sp <- sp - 2"),
        0xc6 => {
            format!("ADI D8 2 Z, S, P, CY, AC A <- A + {:#04x}", buffer[pc + 1])
        }
        0xc7 => format!("RST 0 1  CALL $0"),
        0xc8 => format!("RZ 1  if Z, RET"),
        0xc9 => format!("RET 1  PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2"),
        0xca => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JZ adr 3  if Z, PC <- {:#04x}", addr)
        }
        0xcc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CZ adr 3  if Z, CALL {:#04x}", addr)
        }
        0xcd => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!(
                "CALL adr 3  (SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP+2;PC={:#04x}",
                addr
            )
        }
        0xce => {
            format!(
                "ACI D8 2 Z, S, P, CY, AC A <- A + {:#04x} + CY",
                buffer[pc + 1]
            )
        }
        0xcf => format!("RST 1 1  CALL $8"),
        0xd0 => format!("RNC 1  if NCY, RET"),
        0xd1 => format!("POP D 1  E <- (sp); D <- (sp+1); sp <- sp+2"),
        0xd2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JNC adr 3  if NCY, PC<-{:#04x}", addr)
        }
        0xd3 => {
            format!("OUT D8 2  special {:#04x}", buffer[pc + 1])
        }
        0xd4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CNC adr 3  if NCY, CALL {:#04x}", addr)
        }
        0xd5 => format!("PUSH D 1  (sp-2)<-E; (sp-1)<-D; sp <- sp - 2"),
        0xd6 => {
            format!("SUI D8 2 Z, S, P, CY, AC A <- A - {:#04x}", buffer[pc + 1])
        }
        0xd7 => format!("RST 2 1  CALL $10"),
        0xd8 => format!("RC 1  if CY, RET"),
        0xda => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JC adr 3  if CY, PC<-{:#04x}", addr)
        }
        0xdb => {
            format!("IN D8 2  special {:#04x}", buffer[pc + 1])
        }
        0xdc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CC adr 3  if CY, CALL {:#04x}", addr)
        }
        0xde => {
            format!(
                "SBI D8 2 Z, S, P, CY, AC A <- A - {:#04x} - CY",
                buffer[pc + 1]
            )
        }
        0xdf => {
            format!("RST 3 1  CALL $18")
        }
        0xe0 => format!("RPO 1  if PO, RET"),
        0xe1 => format!("POP H 1  L <- (sp); H <- (sp+1); sp <- sp+2"),
        0xe2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JPO adr 3  if PO, PC <- {:#04x}", addr)
        }
        0xe3 => format!("XTHL 1  L <-> (SP); H <-> (SP+1)"),
        0xe4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CPO adr 3  if PO, CALL {:#04x}", addr)
        }
        0xe5 => format!("PUSH H 1  (sp-2)<-L; (sp-1)<-H; sp <- sp - 2"),
        0xe6 => {
            format!("ANI D8 2 Z, S, P, CY, AC A <- A & {:#04x}", buffer[pc + 1])
        }
        0xe7 => format!("RST 4 1  CALL $20"),
        0xe8 => format!("RPE 1  if PE, RET"),
        0xe9 => format!("PCHL 1  PC.hi <- H; PC.lo <- L"),
        0xea => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JPE adr 3  if PE, PC <- {:#04x}", addr)
        }
        0xeb => format!("XCHG 1  H <-> D; L <-> E"),
        0xec => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CPE adr 3  if PE, CALL {:#04x}", addr)
        }
        0xee => {
            format!("XRI D8 2 Z, S, P, CY, AC A <- A ^ {:#04x}", buffer[pc + 1])
        }
        0xef => format!("RST 5 1  CALL $28"),
        0xf0 => format!("RP 1  if P, RET"),
        0xf1 => format!("POP PSW 1  flags <- (sp); A <- (sp+1); sp <- sp+2"),
        0xf2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JP adr 3  if P=1 PC <- {:#04x}", addr)
        }
        0xf3 => format!("DI 1  special"),
        0xf4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CP adr 3  if P, PC <- {:#04x}", addr)
        }
        0xf5 => format!("PUSH PSW 1  (sp-2)<-flags; (sp-1)<-A; sp <- sp - 2"),
        0xf6 => {
            format!("ORI D8 2 Z, S, P, CY, AC A <- A | {:#04x}", buffer[pc + 1])
        }
        0xf7 => format!("RST 6 1  CALL $30"),
        0xf8 => format!("RM 1  if M, RET"),
        0xf9 => format!("SPHL 1  SP=HL"),
        0xfa => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("JM adr 3  if M, PC <- {:#04x}", addr)
        }
        0xfb => format!("EI 1  special"),
        0xfc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            format!("CM adr 3  if M, CALL {:#04x}", addr)
        }
        0xfe => {
            format!("CPI D8 2 Z, S, P, CY, AC A - {:#04x}", buffer[pc + 1])
        }
        0xff => format!("RST 7 1  CALL $38"),
        _ => format!("Invalid Op code {:#04x}", op),
    }
}
