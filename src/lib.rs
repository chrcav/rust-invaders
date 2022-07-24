extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Texture;
use sdl2::surface::Surface;
use std::path::Path;
use std::thread;
use std::time::Duration;
use std::time::Instant;

mod cpu;

#[derive(Default)]
pub struct MachineInvaders {
    pause: u8,
    shift_offset: u8,
    shift0: u8,
    shift1: u8,
    read1: u8,
    read2: u8,
    state8080: cpu::State8080,
}

impl MachineInvaders {
    pub fn new() -> Self {
        Self {
            pause: 0,
            shift_offset: 0,
            shift0: 0,
            shift1: 0,
            read1: 0,
            read2: 0,
            state8080: cpu::State8080::new(),
        }
    }
}

pub fn create_initial_invaders_machine(
    filename: &str,
    start_pc: u16,
) -> std::io::Result<MachineInvaders> {
    let mut machine = MachineInvaders::new();
    machine.state8080 = cpu::create_initial_emustate(filename, start_pc, 0x2000..0x3fff)
        .expect("couldn't initialize 8080 cpu state");
    Ok(machine)
}

pub fn emu8080(mut machine: MachineInvaders) -> std::io::Result<()> {
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
                    let end = cpu::debug::index_instruction_containing_addr(
                        &machine.state8080.assembly[..],
                        0x23ff,
                    );
                    cpu::debug::dump_assembly(&machine.state8080.assembly[..end]);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::H),
                    ..
                } => {
                    let mut data = pixeldata_from_memory(&machine.state8080.memory, 0x2400, 0x3fff);
                    let surface = Surface::from_data(
                        &mut data[..],
                        224,
                        256,
                        224 * 3,
                        PixelFormatEnum::RGB24,
                    )
                    .unwrap();
                    surface
                        .save_bmp(Path::new("./invaders.bmp"))
                        .expect("unable to write out bmp");
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
                Event::KeyUp {
                    keycode: Some(Keycode::Return),
                    ..
                } => machine.read1 &= 0xfb,
                Event::KeyUp {
                    keycode: Some(Keycode::Space),
                    ..
                } => machine.read1 &= 0xef,
                Event::KeyUp {
                    keycode: Some(Keycode::A),
                    ..
                } => machine.read1 &= 0xdf,
                Event::KeyUp {
                    keycode: Some(Keycode::D),
                    ..
                } => machine.read1 &= 0xbf,
                _ => {}
            }
        }

        if machine.pause == 0x1 {
            continue 'emu;
        }

        let now = Instant::now();
        if now.duration_since(last_interrupt) > Duration::new(0, 16666667) {
            if cpu::are_interrupts_enabled(&machine.state8080) == 0x1 {
                machine.state8080 = cpu::generate_interrupt(machine.state8080, 2);
                canvas.clear();
                // The rest of the game loop goes here...
                let texture_creator = canvas.texture_creator();

                let mut data = pixeldata_from_memory(&machine.state8080.memory, 0x2400, 0x3fff);
                let surface =
                    Surface::from_data(&mut data[..], 224, 256, 224 * 3, PixelFormatEnum::RGB24)
                        .unwrap();
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
                machine.state8080 = cpu::emu8080_opcode(machine.state8080, op);
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
        0x01 => machine.read1,
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
