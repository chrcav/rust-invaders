use emu8080::*;

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    let filename = args.nth(1).unwrap();
    let start_pc: u16 = args.nth(0).unwrap().parse().unwrap();
    let state = create_initial_emustate(&filename, start_pc).unwrap();

    emu8080::emu8080(state)
}
