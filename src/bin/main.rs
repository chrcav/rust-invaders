use rustinvaders::*;

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    let mut filename_opt = args.nth(1);
    let filename = filename_opt.get_or_insert(String::from("invaders.bin"));
    let mut start_pc_opt = args.nth(0);
    let start_pc: u16 = start_pc_opt
        .get_or_insert(String::from("0"))
        .parse()
        .unwrap();
    let state = create_initial_invaders_machine(&filename, start_pc).unwrap();

    rustinvaders::emu_invaders(state)
}
