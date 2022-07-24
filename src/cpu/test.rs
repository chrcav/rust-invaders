use super::*;

#[test]
fn test_cpudiag() {
    let mut state = create_initial_emustate("cpudiag.bin", 256, 0x0000..0xffff).unwrap();

    state.memory[0] = 0xc9; //RET
    state.memory[5] = 0xc9; //RET
    state.memory[368] = 0x7;
    //Skip DAA test
    state.memory[0x59c] = 0xc3; //JMP
    state.memory[0x59d] = 0xc2;
    state.memory[0x59e] = 0x05;

    let mut print_buf = Vec::new();
    loop {
        let op = state.memory[state.pc as usize];
        println!("start state:\n{:02x} {}{}", op, state, state.cc);
        state.pc += 1;
        match op {
            _ => {
                state = emu8080_opcode(state, op);
            }
        };
        //println!("end state:\n{:02x} {}{}", op, state, state.cc);
        if state.pc as usize >= state.program_len {
            break;
        }
        if state.pc == 0 {
            break;
        }
        if state.pc == 5 {
            if state.c == 2 {
                print_buf.push(state.a);
            } else if state.c == 9 {
                let mut addr = addr_from_reg_pair(state.d, state.e) + 3;
                let mut c = state.memory[addr as usize] as char;
                while c != '$' {
                    print_buf.push(c as u8);
                    addr += 1;
                    c = state.memory[addr as usize] as char;
                }
            }
        }
    }
    assert_eq!(String::from_utf8(print_buf).unwrap(), " CPU IS OPERATIONAL");
}

#[test]
fn test_opcode_daa() {
    let mut state = State8080::new();

    state.a = 0x9b;
    state = emu8080_opcode(state, 0x27);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x1);
    assert_eq!(state.cc.cy, 0x1);
    assert_eq!(state.cc.ac, 0x1);
}

#[test]
fn test_opcode_rlc() {
    let mut state = State8080::new();

    state.a = 0x80;
    state = emu8080_opcode(state, 0x07);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x01);
    assert_eq!(state.cc.cy, 0x1);

    state.a = 0xf2;
    state = emu8080_opcode(state, 0x07);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0xe5);
    assert_eq!(state.cc.cy, 0x1);
}

#[test]
fn test_opcode_rrc() {
    let mut state = State8080::new();

    state.a = 0x1;
    state = emu8080_opcode(state, 0x0f);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x80);
    assert_eq!(state.cc.cy, 0x1);

    state.a = 0xf2;
    state = emu8080_opcode(state, 0x0f);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x79);
    assert_eq!(state.cc.cy, 0x0);
}

#[test]
fn test_opcode_ral() {
    let mut state = State8080::new();

    state.a = 0x80;
    state = emu8080_opcode(state, 0x17);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x00);
    assert_eq!(state.cc.cy, 0x1);

    state.a = 0xb5;
    state.cc.cy = 0x0;
    state = emu8080_opcode(state, 0x17);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x6a);
    assert_eq!(state.cc.cy, 0x1);
}

#[test]
fn test_opcode_rar() {
    let mut state = State8080::new();

    state.a = 0x1;
    state = emu8080_opcode(state, 0x1f);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0x00);
    assert_eq!(state.cc.cy, 0x1);

    state.a = 0x6a;
    state.cc.cy = 0x1;
    state = emu8080_opcode(state, 0x1f);
    println!("{:08b}", state.a);
    assert_eq!(state.a, 0xb5);
    assert_eq!(state.cc.cy, 0x0);
}

#[test]
fn test_opcode_xthl() {
    let mut state = State8080::new();

    state.sp = 0x00;
    state.h = 0x9;
    state.l = 0x2;
    state.memory.resize_with(256, Default::default);
    state.memory[state.sp as usize] = 0x7;
    state.memory[state.sp as usize + 1] = 0x3;
    state = emu8080_opcode(state, 0xe3);
    println!("{:08b}", state.a);
    assert_eq!(state.h, 0x3);
    assert_eq!(state.l, 0x7);
    assert_eq!(state.memory[state.sp as usize], 0x2);
    assert_eq!(state.memory[state.sp as usize + 1], 0x9);
}
