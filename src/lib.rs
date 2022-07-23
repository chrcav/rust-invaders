#[cfg(test)]
mod test;
extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Texture;
use sdl2::surface::Surface;
use std::io::prelude::*;
use std::ops::Range;
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
    ram_region: std::ops::Range<u16>,
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
            ram_region: 0x0000..0xffff,
            cc: ConditionCodes::default(),
            int_enable: 0,
            program_len: 0,
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

#[derive(Default)]
pub struct MachineInvaders {
    pause: u8,
    shift_offset: u8,
    shift0: u8,
    shift1: u8,
    read1: u8,
    read2: u8,
    state8080: State8080,
}

impl MachineInvaders {
    pub fn new() -> Self {
        Self {
            pause: 0,
            shift_offset: 0,
            shift0: 0,
            shift1: 0,
            read1: 1,
            read2: 0,
            state8080: State8080::new(),
        }
    }
}

#[derive(Default)]
pub struct Instruction8080 {
    addr: u16,
    op: u8,
    asm: String,
    num_bytes: u16,
}

impl Instruction8080 {
    pub fn new(addr: u16, op: u8, asm: String) -> Self {
        Self {
            addr: addr,
            op: op,
            asm: asm,
            num_bytes: 1,
        }
    }
}

impl std::fmt::Display for Instruction8080 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:#04x} {:#02x} {} {}",
            self.addr, self.op, self.asm, self.num_bytes,
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

pub fn create_initial_invaders_machine(
    filename: &str,
    start_pc: u16,
) -> std::io::Result<MachineInvaders> {
    let mut machine = MachineInvaders::new();
    machine.state8080 =
        create_initial_emustate(filename, start_pc).expect("couldn't initialize 8080 cpu state");
    machine.state8080.ram_region = 0x2000..0x3fff;
    Ok(machine)
}

pub fn emu8080(mut machine: MachineInvaders) -> std::io::Result<()> {
    let assembly = disassemble(&machine.state8080.memory);
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("rust-sdl2 demo", 896, 1024)
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
                    keycode: Some(Keycode::G),
                    ..
                } => {
                    let end = index_instruction_containing_addr(&assembly[..], 0x23ff);
                    dump_assembly(&assembly[..end]);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::P),
                    ..
                } => machine.pause = (machine.pause ^ 0x1) & 0x1,
                Event::KeyDown {
                    keycode: Some(Keycode::Return),
                    ..
                } => machine.read1 |= 0x1 << 2,
                Event::KeyDown {
                    keycode: Some(Keycode::Space),
                    ..
                } => machine.read1 |= 0x1 << 4,
                Event::KeyDown {
                    keycode: Some(Keycode::A),
                    ..
                } => machine.read1 |= 0x1 << 5,
                Event::KeyDown {
                    keycode: Some(Keycode::D),
                    ..
                } => machine.read1 |= 0x1 << 6,
                _ => {}
            }
        }

        if machine.pause == 0x1 {
            continue 'emu;
        }

        let now = Instant::now();
        if now.duration_since(last_interrupt) > Duration::new(0, 16666667) {
            if machine.state8080.int_enable == 0x1 {
                machine.state8080 = generate_interupt(machine.state8080, 2);
                canvas.clear();
                // The rest of the game loop goes here...
                let texture_creator = canvas.texture_creator();

                let mut data = pixeldata_from_memory(&machine.state8080.memory, 0x2400, 0x3fff);
                let surface =
                    Surface::from_data(&mut data[..], 224, 256, 224 * 3, PixelFormatEnum::RGB24)
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

        last_op = Instant::now();
        let op = machine.state8080.memory[machine.state8080.pc as usize];
        /*println!(
            "start state:\n{:02x} {}\n{}flags:\n{}",
            op,
            instruction_format(&machine.state8080.memory, machine.state8080.pc as usize, op),
            machine.state8080,
            machine.state8080.cc
        );*/
        machine.state8080.pc += 1;
        match op {
            0xd3 => {
                // OUT d8
                let port = machine.state8080.memory[machine.state8080.pc as usize];
                machine.state8080.pc += 1;
                machine = machine_output(machine, port);
            }
            0xdb => {
                // IN d8
                let port = machine.state8080.memory[machine.state8080.pc as usize];
                machine.state8080.pc += 1;
                machine = machine_input(machine, port);
            }
            _ => {
                machine.state8080 = emu8080_opcode(machine.state8080, op);
            }
        };
        //println!("end state:\n{}flags:\n{}", machine.state8080, machine.state8080.cc);
        if machine.state8080.pc as usize >= machine.state8080.memory.len() {
            break;
        }
    }
    Ok(())
}

fn pixeldata_from_memory(memory: &Vec<u8>, start: u16, end: u16) -> Vec<u8> {
    let mut data = Vec::new();
    let start_hi = start >> 5;
    let end_hi = end >> 5;
    let start_lo = start & 0x1f;
    let end_lo = (end | 0x10) & 0x1f;
    for lo in (start_lo..=end_lo).rev() {
        for bit in (0..=7).rev() {
            for hi in start_hi..=end_hi {
                let addr = (hi as u16) << 5 | (lo as u16);
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

fn machine_input(mut machine: MachineInvaders, port: u8) -> MachineInvaders {
    machine.state8080.a = match port {
        0x01 => {
            let read1 = machine.read1;
            machine.read1 = 0x1;
            read1
        }
        0x02 => machine.read2,
        0x03 => {
            let val = ((machine.shift1 as u16) << 8) | (machine.shift0 as u16);
            ((val >> (8 - machine.shift_offset)) & 0xff) as u8
        }
        _ => panic!("invalid port {}", port),
    };
    machine
}

fn machine_output(mut machine: MachineInvaders, port: u8) -> MachineInvaders {
    let value = machine.state8080.a;
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

fn generate_interupt(mut state: State8080, interupt_num: u16) -> State8080 {
    state.int_enable = 0x0;
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
        0x27 => {
            // DAA
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
        _ => panic!(
            "Unimplemented Op code {:#04x} at addr: {:#04x}",
            op,
            state.pc - 1
        ),
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

fn get_addr_from_bytes(i: usize, memory: &Vec<u8>) -> u16 {
    let mut addr: u16 = memory[i + 1] as u16;
    addr = addr << 8;
    addr |= memory[i] as u16;
    //println!("{:04x}", addr);
    addr
}

fn dump_assembly(intrs: &[Instruction8080]) {
    for instr in intrs {
        println!("{}", instr);
    }
}

fn index_instruction_containing_addr(instrs: &[Instruction8080], addr: u16) -> usize {
    let mut i = 1;
    for instr in instrs {
        if (instr.addr..instr.addr + instr.num_bytes).contains(&addr) {
            break;
        }
        i += 1;
    }
    i
}

fn disassemble(buffer: &Vec<u8>) -> Vec<Instruction8080> {
    let mut instrs = Vec::new();
    let mut i = 0;
    loop {
        let op = buffer[i];
        let instr = instruction_from_pc(&buffer, i, op);
        i += instr.num_bytes as usize;
        instrs.push(instr);
        if i >= buffer.len() {
            break;
        }
    }
    instrs
}

pub fn instruction_from_pc(buffer: &Vec<u8>, pc: usize, op: u8) -> Instruction8080 {
    match op {
        0x00 => Instruction8080::new(pc as u16, op, format!("NOP ")),
        0x01 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("LXI B,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x02 => Instruction8080::new(pc as u16, op, format!("STAX B")),
        0x03 => Instruction8080::new(pc as u16, op, format!("INX B")),
        0x04 => Instruction8080::new(pc as u16, op, format!("INR B")),
        0x05 => Instruction8080::new(pc as u16, op, format!("DCR B")),
        0x06 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI B,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x07 => Instruction8080::new(pc as u16, op, format!("RLC")),
        0x09 => Instruction8080::new(pc as u16, op, format!("DAD B")),
        0x0a => Instruction8080::new(pc as u16, op, format!("LDAX B")),
        0x0b => Instruction8080::new(pc as u16, op, format!("DCX B")),
        0x0c => Instruction8080::new(pc as u16, op, format!("INR C")),
        0x0d => Instruction8080::new(pc as u16, op, format!("DCR C")),
        0x0e => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI C,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x0f => Instruction8080::new(pc as u16, op, format!("RRC")),
        0x11 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("LXI D,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x12 => Instruction8080::new(pc as u16, op, format!("STAX D")),
        0x13 => Instruction8080::new(pc as u16, op, format!("INX D")),
        0x14 => Instruction8080::new(pc as u16, op, format!("INR D")),
        0x15 => Instruction8080::new(pc as u16, op, format!("DCR D")),
        0x16 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI D, {:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x17 => Instruction8080::new(pc as u16, op, format!("RAL")),
        0x19 => Instruction8080::new(pc as u16, op, format!("DAD D")),
        0x1a => Instruction8080::new(pc as u16, op, format!("LDAX D")),
        0x1b => Instruction8080::new(pc as u16, op, format!("DCX D")),
        0x1c => Instruction8080::new(pc as u16, op, format!("INR E")),
        0x1d => Instruction8080::new(pc as u16, op, format!("DCR E")),
        0x1e => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI E,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x1f => Instruction8080::new(pc as u16, op, format!("RAR")),
        0x20 => Instruction8080::new(pc as u16, op, format!("RIM")),
        0x21 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("LXI H,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x22 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("SHLD {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0x23 => Instruction8080::new(pc as u16, op, format!("INX H")),
        0x24 => Instruction8080::new(pc as u16, op, format!("INR H")),
        0x25 => Instruction8080::new(pc as u16, op, format!("DCR H")),
        0x26 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI H,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x27 => Instruction8080::new(pc as u16, op, format!("DAA")),
        0x29 => Instruction8080::new(pc as u16, op, format!("DAD H")),
        0x2a => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("LHLD {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0x2b => Instruction8080::new(pc as u16, op, format!("DCX H")),
        0x2c => Instruction8080::new(pc as u16, op, format!("INR L")),
        0x2d => Instruction8080::new(pc as u16, op, format!("DCR L")),
        0x2e => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI L,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x2f => Instruction8080::new(pc as u16, op, format!("CMA")),
        0x30 => Instruction8080::new(pc as u16, op, format!("SIM")),
        0x31 => Instruction8080::new(
            pc as u16,
            op,
            format!(
                "LXI SP, D16 3  SP.hi <- {:#04x}, SP.lo <- {:#04x}",
                buffer[pc + 2],
                buffer[pc + 1]
            ),
        ),
        0x32 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("STA {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0x33 => Instruction8080::new(pc as u16, op, format!("INX SP")),
        0x34 => Instruction8080::new(pc as u16, op, format!("INR M")),
        0x35 => Instruction8080::new(pc as u16, op, format!("DCR M")),
        0x36 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI M,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x37 => Instruction8080::new(pc as u16, op, format!("STC")),
        0x39 => Instruction8080::new(pc as u16, op, format!("DAD SP")),
        0x3a => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("LDA {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0x3b => Instruction8080::new(pc as u16, op, format!("DCX SP")),
        0x3c => Instruction8080::new(pc as u16, op, format!("INR A")),
        0x3d => Instruction8080::new(pc as u16, op, format!("DCR A")),
        0x3e => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("MVI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0x3f => Instruction8080::new(pc as u16, op, format!("CMC")),
        0x40 => Instruction8080::new(pc as u16, op, format!("MOV B,B")),
        0x41 => Instruction8080::new(pc as u16, op, format!("MOV B,C")),
        0x42 => Instruction8080::new(pc as u16, op, format!("MOV B,D")),
        0x43 => Instruction8080::new(pc as u16, op, format!("MOV B,E")),
        0x44 => Instruction8080::new(pc as u16, op, format!("MOV B,H")),
        0x45 => Instruction8080::new(pc as u16, op, format!("MOV B,L")),
        0x46 => Instruction8080::new(pc as u16, op, format!("MOV B,M")),
        0x47 => Instruction8080::new(pc as u16, op, format!("MOV B,A")),
        0x48 => Instruction8080::new(pc as u16, op, format!("MOV C,B")),
        0x49 => Instruction8080::new(pc as u16, op, format!("MOV C,C")),
        0x4a => Instruction8080::new(pc as u16, op, format!("MOV C,D")),
        0x4b => Instruction8080::new(pc as u16, op, format!("MOV C,E")),
        0x4c => Instruction8080::new(pc as u16, op, format!("MOV C,H")),
        0x4d => Instruction8080::new(pc as u16, op, format!("MOV C,L")),
        0x4e => Instruction8080::new(pc as u16, op, format!("MOV C,M")),
        0x4f => Instruction8080::new(pc as u16, op, format!("MOV C,A")),
        0x50 => Instruction8080::new(pc as u16, op, format!("MOV D,B")),
        0x51 => Instruction8080::new(pc as u16, op, format!("MOV D,C")),
        0x52 => Instruction8080::new(pc as u16, op, format!("MOV D,D")),
        0x53 => Instruction8080::new(pc as u16, op, format!("MOV D,E")),
        0x54 => Instruction8080::new(pc as u16, op, format!("MOV D,H")),
        0x55 => Instruction8080::new(pc as u16, op, format!("MOV D,L")),
        0x56 => Instruction8080::new(pc as u16, op, format!("MOV D,M")),
        0x57 => Instruction8080::new(pc as u16, op, format!("MOV D,A")),
        0x58 => Instruction8080::new(pc as u16, op, format!("MOV E,B")),
        0x59 => Instruction8080::new(pc as u16, op, format!("MOV E,C")),
        0x5a => Instruction8080::new(pc as u16, op, format!("MOV E,D")),
        0x5b => Instruction8080::new(pc as u16, op, format!("MOV E,E")),
        0x5c => Instruction8080::new(pc as u16, op, format!("MOV E,H")),
        0x5d => Instruction8080::new(pc as u16, op, format!("MOV E,L")),
        0x5e => Instruction8080::new(pc as u16, op, format!("MOV E,M")),
        0x5f => Instruction8080::new(pc as u16, op, format!("MOV E,A")),
        0x60 => Instruction8080::new(pc as u16, op, format!("MOV H,B")),
        0x61 => Instruction8080::new(pc as u16, op, format!("MOV H,C")),
        0x62 => Instruction8080::new(pc as u16, op, format!("MOV H,D")),
        0x63 => Instruction8080::new(pc as u16, op, format!("MOV H,E")),
        0x64 => Instruction8080::new(pc as u16, op, format!("MOV H,H")),
        0x65 => Instruction8080::new(pc as u16, op, format!("MOV H,L")),
        0x66 => Instruction8080::new(pc as u16, op, format!("MOV H,M")),
        0x67 => Instruction8080::new(pc as u16, op, format!("MOV H,A")),
        0x68 => Instruction8080::new(pc as u16, op, format!("MOV L,B")),
        0x69 => Instruction8080::new(pc as u16, op, format!("MOV L,C")),
        0x6a => Instruction8080::new(pc as u16, op, format!("MOV L,D")),
        0x6b => Instruction8080::new(pc as u16, op, format!("MOV L,E")),
        0x6c => Instruction8080::new(pc as u16, op, format!("MOV L,H")),
        0x6d => Instruction8080::new(pc as u16, op, format!("MOV L,L")),
        0x6e => Instruction8080::new(pc as u16, op, format!("MOV L,M")),
        0x6f => Instruction8080::new(pc as u16, op, format!("MOV L,A")),
        0x70 => Instruction8080::new(pc as u16, op, format!("MOV M,B")),
        0x71 => Instruction8080::new(pc as u16, op, format!("MOV M,C")),
        0x72 => Instruction8080::new(pc as u16, op, format!("MOV M,D")),
        0x73 => Instruction8080::new(pc as u16, op, format!("MOV M,E")),
        0x74 => Instruction8080::new(pc as u16, op, format!("MOV M,H")),
        0x75 => Instruction8080::new(pc as u16, op, format!("MOV M,L")),
        0x76 => Instruction8080::new(pc as u16, op, format!("HLT")),
        0x77 => Instruction8080::new(pc as u16, op, format!("MOV M,A")),
        0x78 => Instruction8080::new(pc as u16, op, format!("MOV A,B")),
        0x79 => Instruction8080::new(pc as u16, op, format!("MOV A,C")),
        0x7a => Instruction8080::new(pc as u16, op, format!("MOV A,D")),
        0x7b => Instruction8080::new(pc as u16, op, format!("MOV A,E")),
        0x7c => Instruction8080::new(pc as u16, op, format!("MOV A,H")),
        0x7d => Instruction8080::new(pc as u16, op, format!("MOV A,L")),
        0x7e => Instruction8080::new(pc as u16, op, format!("MOV A,M")),
        0x7f => Instruction8080::new(pc as u16, op, format!("MOV A,A")),
        0x80 => Instruction8080::new(pc as u16, op, format!("ADD B")),
        0x81 => Instruction8080::new(pc as u16, op, format!("ADD C")),
        0x82 => Instruction8080::new(pc as u16, op, format!("ADD D")),
        0x83 => Instruction8080::new(pc as u16, op, format!("ADD E")),
        0x84 => Instruction8080::new(pc as u16, op, format!("ADD H")),
        0x85 => Instruction8080::new(pc as u16, op, format!("ADD L")),
        0x86 => Instruction8080::new(pc as u16, op, format!("ADD M")),
        0x87 => Instruction8080::new(pc as u16, op, format!("ADD A")),
        0x88 => Instruction8080::new(pc as u16, op, format!("ADC B")),
        0x89 => Instruction8080::new(pc as u16, op, format!("ADC C")),
        0x8a => Instruction8080::new(pc as u16, op, format!("ADC D")),
        0x8b => Instruction8080::new(pc as u16, op, format!("ADC E")),
        0x8c => Instruction8080::new(pc as u16, op, format!("ADC H")),
        0x8d => Instruction8080::new(pc as u16, op, format!("ADC L")),
        0x8e => Instruction8080::new(pc as u16, op, format!("ADC M")),
        0x8f => Instruction8080::new(pc as u16, op, format!("ADC A")),
        0x90 => Instruction8080::new(pc as u16, op, format!("SUB B")),
        0x91 => Instruction8080::new(pc as u16, op, format!("SUB C")),
        0x92 => Instruction8080::new(pc as u16, op, format!("SUB D")),
        0x93 => Instruction8080::new(pc as u16, op, format!("SUB E")),
        0x94 => Instruction8080::new(pc as u16, op, format!("SUB H")),
        0x95 => Instruction8080::new(pc as u16, op, format!("SUB L")),
        0x96 => Instruction8080::new(pc as u16, op, format!("SUB M")),
        0x97 => Instruction8080::new(pc as u16, op, format!("SUB A")),
        0x98 => Instruction8080::new(pc as u16, op, format!("SBB B")),
        0x99 => Instruction8080::new(pc as u16, op, format!("SBB C")),
        0x9a => Instruction8080::new(pc as u16, op, format!("SBB D")),
        0x9b => Instruction8080::new(pc as u16, op, format!("SBB E")),
        0x9c => Instruction8080::new(pc as u16, op, format!("SBB H")),
        0x9d => Instruction8080::new(pc as u16, op, format!("SBB L")),
        0x9e => Instruction8080::new(pc as u16, op, format!("SBB M")),
        0x9f => Instruction8080::new(pc as u16, op, format!("SBB A")),
        0xa0 => Instruction8080::new(pc as u16, op, format!("ANA B")),
        0xa1 => Instruction8080::new(pc as u16, op, format!("ANA C")),
        0xa2 => Instruction8080::new(pc as u16, op, format!("ANA D")),
        0xa3 => Instruction8080::new(pc as u16, op, format!("ANA E")),
        0xa4 => Instruction8080::new(pc as u16, op, format!("ANA H")),
        0xa5 => Instruction8080::new(pc as u16, op, format!("ANA L")),
        0xa6 => Instruction8080::new(pc as u16, op, format!("ANA M")),
        0xa7 => Instruction8080::new(pc as u16, op, format!("ANA A")),
        0xa8 => Instruction8080::new(pc as u16, op, format!("XRA B")),
        0xa9 => Instruction8080::new(pc as u16, op, format!("XRA C")),
        0xaa => Instruction8080::new(pc as u16, op, format!("XRA D")),
        0xab => Instruction8080::new(pc as u16, op, format!("XRA E")),
        0xac => Instruction8080::new(pc as u16, op, format!("XRA H")),
        0xad => Instruction8080::new(pc as u16, op, format!("XRA L")),
        0xae => Instruction8080::new(pc as u16, op, format!("XRA M")),
        0xaf => Instruction8080::new(pc as u16, op, format!("XRA A")),
        //ORA
        0xb0 => Instruction8080::new(pc as u16, op, format!("ORA B")),
        0xb1 => Instruction8080::new(pc as u16, op, format!("ORA C")),
        0xb2 => Instruction8080::new(pc as u16, op, format!("ORA D")),
        0xb3 => Instruction8080::new(pc as u16, op, format!("ORA E")),
        0xb4 => Instruction8080::new(pc as u16, op, format!("ORA H")),
        0xb5 => Instruction8080::new(pc as u16, op, format!("ORA L")),
        0xb6 => Instruction8080::new(pc as u16, op, format!("ORA M")),
        0xb7 => Instruction8080::new(pc as u16, op, format!("ORA A")),
        // CMP
        0xb8 => Instruction8080::new(pc as u16, op, format!("CMP B")),
        0xb9 => Instruction8080::new(pc as u16, op, format!("CMP C")),
        0xba => Instruction8080::new(pc as u16, op, format!("CMP D")),
        0xbb => Instruction8080::new(pc as u16, op, format!("CMP E")),
        0xbc => Instruction8080::new(pc as u16, op, format!("CMP H")),
        0xbd => Instruction8080::new(pc as u16, op, format!("CMP L")),
        0xbe => Instruction8080::new(pc as u16, op, format!("CMP M")),
        0xbf => Instruction8080::new(pc as u16, op, format!("CMP A")),
        0xc0 => Instruction8080::new(pc as u16, op, format!("RNZ")),
        0xc1 => Instruction8080::new(pc as u16, op, format!("POP B")),
        0xc2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JNZ {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xc3 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JMP {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xc4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CNZ {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xc5 => Instruction8080::new(pc as u16, op, format!("PUSH B")),
        0xc6 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("ADI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xc7 => Instruction8080::new(pc as u16, op, format!("RST 0 1  CALL $0")),
        0xc8 => Instruction8080::new(pc as u16, op, format!("RZ")),
        0xc9 => Instruction8080::new(pc as u16, op, format!("RET")),
        0xca => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JZ {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xcc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CZ {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xcd => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CALL {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xce => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("ACI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xcf => Instruction8080::new(pc as u16, op, format!("RST 1 1  CALL $8")),
        0xd0 => Instruction8080::new(pc as u16, op, format!("RNC")),
        0xd1 => Instruction8080::new(pc as u16, op, format!("POP D")),
        0xd2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JNC {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xd3 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("OUT {:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xd4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CNC {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xd5 => Instruction8080::new(pc as u16, op, format!("PUSH D")),
        0xd6 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("SUI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xd7 => Instruction8080::new(pc as u16, op, format!("RST 2 1  CALL $10")),
        0xd8 => Instruction8080::new(pc as u16, op, format!("RC")),
        0xda => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JC {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xdb => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("IN {:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xdc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CC {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xde => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("SBI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xdf => Instruction8080::new(pc as u16, op, format!("RST 3 1  CALL $18")),
        0xe0 => Instruction8080::new(pc as u16, op, format!("RPO")),
        0xe1 => Instruction8080::new(pc as u16, op, format!("POP H")),
        0xe2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JPO {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xe3 => Instruction8080::new(pc as u16, op, format!("XTHL")),
        0xe4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CPO {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xe5 => Instruction8080::new(pc as u16, op, format!("PUSH H")),
        0xe6 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("ANI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xe7 => Instruction8080::new(pc as u16, op, format!("RST 4 1  CALL $20")),
        0xe8 => Instruction8080::new(pc as u16, op, format!("RPE")),
        0xe9 => Instruction8080::new(pc as u16, op, format!("PCHL")),
        0xea => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JPE {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xeb => Instruction8080::new(pc as u16, op, format!("XCHG")),
        0xec => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CPE {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xee => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("XRI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xef => Instruction8080::new(pc as u16, op, format!("RST 5 1  CALL $28")),
        0xf0 => Instruction8080::new(pc as u16, op, format!("RP")),
        0xf1 => Instruction8080::new(pc as u16, op, format!("POP PSW")),
        0xf2 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JP {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xf3 => Instruction8080::new(pc as u16, op, format!("DI")),
        0xf4 => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("CP {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xf5 => Instruction8080::new(pc as u16, op, format!("PUSH PSW")),
        0xf6 => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("ORI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xf7 => Instruction8080::new(pc as u16, op, format!("RST 6 1  CALL $30")),
        0xf8 => Instruction8080::new(pc as u16, op, format!("RM")),
        0xf9 => Instruction8080::new(pc as u16, op, format!("SPHL")),
        0xfa => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr = Instruction8080::new(pc as u16, op, format!("JM {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xfb => Instruction8080::new(pc as u16, op, format!("EI")),
        0xfc => {
            let addr = get_addr_from_bytes(pc, &buffer);
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("CM adr 3  if M, CALL {:#04x}", addr));
            instr.num_bytes = 3;
            instr
        }
        0xfe => {
            let mut instr =
                Instruction8080::new(pc as u16, op, format!("CPI A,{:#04x}", buffer[pc + 1]));
            instr.num_bytes = 2;
            instr
        }
        0xff => Instruction8080::new(pc as u16, op, format!("RST 7 1  CALL $38")),
        _ => Instruction8080::new(pc as u16, op, format!("Invalid Op code")),
    }
}
