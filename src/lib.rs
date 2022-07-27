extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Canvas;
use sdl2::render::Texture;
use sdl2::surface::Surface;
use sdl2::video::Window;
use std::path::Path;
use std::time::Duration;
use std::time::Instant;

mod cpu;

pub struct MachineInvaders {
    pause: u8,
    done: u8,
    shift_offset: u8,
    shift0: u8,
    shift1: u8,
    coin_up: u8,
    p1_start: u8,
    p1_shoot: u8,
    p1_left: u8,
    p1_right: u8,
    p2_start: u8,
    p2_shoot: u8,
    p2_left: u8,
    p2_right: u8,
    state8080: cpu::State8080,
    sdl_context: sdl2::Sdl,
}

impl MachineInvaders {
    pub fn new() -> Self {
        Self {
            pause: 0,
            done: 0,
            shift_offset: 0,
            shift0: 0,
            shift1: 0,
            coin_up: 0,
            p1_start: 0,
            p1_shoot: 0,
            p1_left: 0,
            p1_right: 0,
            p2_start: 0,
            p2_shoot: 0,
            p2_left: 0,
            p2_right: 0,
            state8080: cpu::State8080::new(),
            sdl_context: sdl2::init().unwrap(),
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
    let video_subsystem = machine.sdl_context.video().unwrap();

    let window = video_subsystem
        .window("rust-sdl2 demo", 896, 1024)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut last_interrupt = 0x2;
    let mut last_interrupt_time = Instant::now();
    'emu: loop {
        machine = update_button_state(machine);

        if machine.pause == 0x1 {
            continue 'emu;
        }

        let port1 = create_input_port_byte(&machine, 0x1);
        machine.state8080 = cpu::write_input(machine.state8080, 0x1, port1);
        let port2 = create_input_port_byte(&machine, 0x2);
        machine.state8080 = cpu::write_input(machine.state8080, 0x2, port2);

        let now = Instant::now();
        if now.duration_since(last_interrupt_time) > Duration::new(0, 8333333) {
            if last_interrupt == 0x1 {
                machine.state8080 = cpu::generate_interrupt(machine.state8080, 2);
                last_interrupt = 0x2;

                canvas = draw_screen(canvas, &machine.state8080.memory);
            } else if last_interrupt == 0x2 {
                machine.state8080 = cpu::generate_interrupt(machine.state8080, 1);
                last_interrupt = 0x1;
            }
            last_interrupt_time = Instant::now();
        }
        machine.state8080 = cpu::emu8080(machine.state8080);

        if cpu::new_output_byte(&machine.state8080, 0x2) {
            machine = machine_output(machine, 0x2);
        }
        if cpu::new_output_byte(&machine.state8080, 0x4) {
            machine = machine_output(machine, 0x4);
        }

        if machine.done == 0x1 {
            break 'emu;
        }
    }
    Ok(())
}

fn draw_screen(mut canvas: Canvas<Window>, memory: &Vec<u8>) -> Canvas<Window> {
    canvas.clear();
    // The rest of the game loop goes here...
    let texture_creator = canvas.texture_creator();

    let mut data = pixeldata_from_memory(memory, 0x2400, 0x3fff);
    let surface =
        Surface::from_data(&mut data[..], 224, 256, 224 * 3, PixelFormatEnum::RGB24).unwrap();
    let texture = Texture::from_surface(&surface, &texture_creator).unwrap();
    canvas
        .copy(&texture, None, None)
        .expect("couldn't render texture");
    canvas.present();
    canvas
}

fn update_button_state(mut machine: MachineInvaders) -> MachineInvaders {
    let mut event_pump = machine.sdl_context.event_pump().unwrap();
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => machine.done = 0x1,
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
                let surface =
                    Surface::from_data(&mut data[..], 224, 256, 224 * 3, PixelFormatEnum::RGB24)
                        .unwrap();
                surface
                    .save_bmp(Path::new("./invaders.bmp"))
                    .expect("unable to write out bmp");
                println!("created screenshot ./invaders.bmp");
            }
            Event::KeyDown {
                keycode: Some(Keycode::P),
                ..
            } => machine.pause = (machine.pause ^ 0x1) & 0x1,

            Event::KeyDown {
                keycode: Some(Keycode::C),
                ..
            } => machine.coin_up = 0x1,
            Event::KeyUp {
                keycode: Some(Keycode::C),
                ..
            } => machine.coin_up = 0x0,

            Event::KeyDown {
                keycode: Some(Keycode::Num1),
                ..
            } => machine.p1_start = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::Space),
                ..
            } => machine.p1_shoot = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => machine.p1_left = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => machine.p1_right = 0x1,
            Event::KeyUp {
                keycode: Some(Keycode::Num1),
                ..
            } => machine.p1_start = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::Space),
                ..
            } => machine.p1_shoot = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::A),
                ..
            } => machine.p1_left = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::D),
                ..
            } => machine.p1_right = 0x0,

            Event::KeyDown {
                keycode: Some(Keycode::Num2),
                ..
            } => machine.p2_start = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::X),
                ..
            } => machine.p2_shoot = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::Left),
                ..
            } => machine.p2_left = 0x1,
            Event::KeyDown {
                keycode: Some(Keycode::Right),
                ..
            } => machine.p2_right = 0x1,

            Event::KeyUp {
                keycode: Some(Keycode::Num2),
                ..
            } => machine.p2_start = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::X),
                ..
            } => machine.p2_shoot = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::Left),
                ..
            } => machine.p2_left = 0x0,
            Event::KeyUp {
                keycode: Some(Keycode::Right),
                ..
            } => machine.p2_right = 0x0,
            _ => {}
        }
    }
    machine
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

fn create_input_port_byte(machine: &MachineInvaders, port: u8) -> u8 {
    let mut byte: u8 = 0;
    match port {
        0x01 => {
            byte |= machine.coin_up;
            byte |= machine.p2_start << 1;
            byte |= machine.p1_start << 2;
            byte |= 0x1 << 3;
            byte |= machine.p1_shoot << 4;
            byte |= machine.p1_left << 5;
            byte |= machine.p1_right << 6;
        }
        0x02 => {
            byte |= machine.p2_shoot << 4;
            byte |= machine.p2_left << 5;
            byte |= machine.p2_right << 6;
        }
        _ => {}
    }
    byte
}

fn machine_output(mut machine: MachineInvaders, port: u8) -> MachineInvaders {
    let value = cpu::read_output(&machine.state8080, port);
    match port {
        0x02 => {
            machine.shift_offset = value & 0x7;
            let val = ((machine.shift1 as u16) << 8) | (machine.shift0 as u16);
            machine.state8080 = cpu::write_input(
                machine.state8080,
                0x3,
                ((val >> (8 - machine.shift_offset)) & 0xff) as u8,
            );
        }
        0x04 => {
            machine.shift0 = machine.shift1;
            machine.shift1 = value;
            let val = ((machine.shift1 as u16) << 8) | (machine.shift0 as u16);
            machine.state8080 = cpu::write_input(
                machine.state8080,
                0x3,
                ((val >> (8 - machine.shift_offset)) & 0xff) as u8,
            );
        }
        0x03 | 0x05 | 0x06 => {}
        _ => panic!("invalid port {}", port),
    }
    machine
}
