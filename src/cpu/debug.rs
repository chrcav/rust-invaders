use super::*;

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

pub fn dump_assembly(intrs: &[Instruction8080]) {
    for instr in intrs {
        println!("{}", instr);
    }
}

pub fn index_instruction_containing_addr(instrs: &[Instruction8080], addr: u16) -> usize {
    let mut i = 0;
    for instr in instrs {
        if (instr.addr..instr.addr + instr.num_bytes).contains(&addr) {
            break;
        }
        i += 1;
    }
    i
}

pub fn disassemble(buffer: &Vec<u8>) -> Vec<Instruction8080> {
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
