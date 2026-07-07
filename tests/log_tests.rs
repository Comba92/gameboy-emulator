use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use tomboyemu_core::emu::GbEmulator;

#[derive(Default, Debug, PartialEq)]
struct CpuTestState {
    pc: u16,
    sp: u16,
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
}

// A: 01 F: B0 B: 00 C: 13 D: 00 E: D8 H: 01 L: 4D SP: FFFE PC: 00:0100 (00 C3 13 02)
fn parse_log_line(mut line: &str) -> CpuTestState {
    let mut state = CpuTestState::default();

    state.a = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.f = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.b = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.c = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.d = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.e = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.h = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.l = u8::from_str_radix(&line[3..5], 16).unwrap();
    line = &line[6..];
    state.sp = u16::from_str_radix(&line[4..8], 16).unwrap();
    line = &line[9..];
    state.pc = u16::from_str_radix(&line[7..11], 16).unwrap();

    state
}

fn cpu_to_mock(emu: &GbEmulator) -> CpuTestState {
    let cpu = &emu.cpu;

    CpuTestState {
        pc: cpu.pc,
        sp: cpu.sp,
        a: cpu.a,
        f: cpu.f.clone().into_bits(),
        b: cpu.bc.hi(),
        c: cpu.bc.lo(),
        d: cpu.de.hi(),
        e: cpu.de.lo(),
        h: cpu.hl.hi(),
        l: cpu.hl.lo(),
    }
}

#[test]
fn parse_test() {
    let file = File::open("./tests/logs/BootromLog.txt").unwrap();
    let mut reader = BufReader::new(file);
    let mut log = String::new();
    reader.read_line(&mut log).unwrap();

    let state = parse_log_line(log.lines().next().unwrap());
    println!("{state:x?}");
}

use pretty_assertions::assert_eq;

#[test]
fn test_whole_file() {
    // let file = File::open("./tests/logs/BootromLog.txt").unwrap();
    let file = File::open("./tests/logs/EpicLog.txt").unwrap();
    let mut reader = BufReader::new(file);
    let mut log = String::new();

    let rom = std::fs::read("D:\\code\\emulators\\nes-emulator\\roms\\Dr. Mario.gb").unwrap();
    let boot = std::fs::read("./utils/dmg_boot.bin").unwrap();

    let mut emu = GbEmulator::builder()
        // .with_bios(Some(&boot))
        .with_rom(&rom)
        .build()
        .unwrap();

    let mut line = 0;
    let mut last_op = 0;
    let mut last_op_name = Opcode::NOP;
    while let Ok(len) = reader.read_line(&mut log) {
        if len == 0 {
            break;
        }

        let log_state = parse_log_line(&log);
        let my_state = cpu_to_mock(&emu);

        assert_eq!(
            log_state, my_state,
            "Test failed at line {line}: {} \n{:x?}\n{:x?}\n{last_op_name:?} ({last_op:02x})",
            log, log_state, my_state
        );

        last_op = emu.debug_read(emu.cpu.pc);
        last_op_name = OPCODES_NAMES[&last_op];
        emu.cpu_step();
        line += 1;
        log.clear();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    NOP,
    LD,
    INC16,
    INC,
    DEC,
    RLCA,
    ADDHL,
    DEC16,
    RRCA,
    STOP,
    RLA,
    JR,
    RRA,
    JRCC,
    DAA,
    CPL,
    SCF,
    CCF,
    HALT,
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
    RETCC,
    POP,
    JPCC,
    JP,
    CALLCC,
    PUSH,
    RST,
    RET,
    PREFIX,
    CALL,
    ILLEGAL,
    RETI,
    ADDSP,
    JPHL,
    DI,
    LDSP,
    LDHL,
    EI,
}

pub const OPCODES_NAMES: std::sync::LazyLock<std::collections::HashMap<u8, Opcode>> =
    std::sync::LazyLock::new(|| {
        use Opcode::*;

        const LIST: [(u8, Opcode); 256] = [
            (0, NOP),
            (1, LD),
            (2, LD),
            (3, INC16),
            (4, INC),
            (5, DEC),
            (6, LD),
            (7, RLCA),
            (8, LD),
            (9, ADDHL),
            (10, LD),
            (11, DEC16),
            (12, INC),
            (13, DEC),
            (14, LD),
            (15, RRCA),
            (16, STOP),
            (17, LD),
            (18, LD),
            (19, INC16),
            (20, INC),
            (21, DEC),
            (22, LD),
            (23, RLA),
            (24, JR),
            (25, ADDHL),
            (26, LD),
            (27, DEC16),
            (28, INC),
            (29, DEC),
            (30, LD),
            (31, RRA),
            (32, JRCC),
            (33, LD),
            (34, LD),
            (35, INC16),
            (36, INC),
            (37, DEC),
            (38, LD),
            (39, DAA),
            (40, JRCC),
            (41, ADDHL),
            (42, LD),
            (43, DEC16),
            (44, INC),
            (45, DEC),
            (46, LD),
            (47, CPL),
            (48, JRCC),
            (49, LD),
            (50, LD),
            (51, INC16),
            (52, INC),
            (53, DEC),
            (54, LD),
            (55, SCF),
            (56, JRCC),
            (57, ADDHL),
            (58, LD),
            (59, DEC16),
            (60, INC),
            (61, DEC),
            (62, LD),
            (63, CCF),
            (64, LD),
            (65, LD),
            (66, LD),
            (67, LD),
            (68, LD),
            (69, LD),
            (70, LD),
            (71, LD),
            (72, LD),
            (73, LD),
            (74, LD),
            (75, LD),
            (76, LD),
            (77, LD),
            (78, LD),
            (79, LD),
            (80, LD),
            (81, LD),
            (82, LD),
            (83, LD),
            (84, LD),
            (85, LD),
            (86, LD),
            (87, LD),
            (88, LD),
            (89, LD),
            (90, LD),
            (91, LD),
            (92, LD),
            (93, LD),
            (94, LD),
            (95, LD),
            (96, LD),
            (97, LD),
            (98, LD),
            (99, LD),
            (100, LD),
            (101, LD),
            (102, LD),
            (103, LD),
            (104, LD),
            (105, LD),
            (106, LD),
            (107, LD),
            (108, LD),
            (109, LD),
            (110, LD),
            (111, LD),
            (112, LD),
            (113, LD),
            (114, LD),
            (115, LD),
            (116, LD),
            (117, LD),
            (118, HALT),
            (119, LD),
            (120, LD),
            (121, LD),
            (122, LD),
            (123, LD),
            (124, LD),
            (125, LD),
            (126, LD),
            (127, LD),
            (128, ADD),
            (129, ADD),
            (130, ADD),
            (131, ADD),
            (132, ADD),
            (133, ADD),
            (134, ADD),
            (135, ADD),
            (136, ADC),
            (137, ADC),
            (138, ADC),
            (139, ADC),
            (140, ADC),
            (141, ADC),
            (142, ADC),
            (143, ADC),
            (144, SUB),
            (145, SUB),
            (146, SUB),
            (147, SUB),
            (148, SUB),
            (149, SUB),
            (150, SUB),
            (151, SUB),
            (152, SBC),
            (153, SBC),
            (154, SBC),
            (155, SBC),
            (156, SBC),
            (157, SBC),
            (158, SBC),
            (159, SBC),
            (160, AND),
            (161, AND),
            (162, AND),
            (163, AND),
            (164, AND),
            (165, AND),
            (166, AND),
            (167, AND),
            (168, XOR),
            (169, XOR),
            (170, XOR),
            (171, XOR),
            (172, XOR),
            (173, XOR),
            (174, XOR),
            (175, XOR),
            (176, OR),
            (177, OR),
            (178, OR),
            (179, OR),
            (180, OR),
            (181, OR),
            (182, OR),
            (183, OR),
            (184, CP),
            (185, CP),
            (186, CP),
            (187, CP),
            (188, CP),
            (189, CP),
            (190, CP),
            (191, CP),
            (192, RETCC),
            (193, POP),
            (194, JPCC),
            (195, JP),
            (196, CALLCC),
            (197, PUSH),
            (198, ADD),
            (199, RST),
            (200, RETCC),
            (201, RET),
            (202, JPCC),
            (0xCB, PREFIX),
            (204, CALLCC),
            (205, CALL),
            (206, ADC),
            (207, RST),
            (208, RETCC),
            (209, POP),
            (210, JPCC),
            (211, ILLEGAL),
            (212, CALLCC),
            (213, PUSH),
            (214, SUB),
            (215, RST),
            (216, RETCC),
            (217, RETI),
            (218, JPCC),
            (219, ILLEGAL),
            (220, CALLCC),
            (221, ILLEGAL),
            (222, SBC),
            (223, RST),
            (224, LD),
            (225, POP),
            (226, LD),
            (227, ILLEGAL),
            (228, ILLEGAL),
            (229, PUSH),
            (230, AND),
            (231, RST),
            (232, ADDSP),
            (233, JPHL),
            (234, LD),
            (235, ILLEGAL),
            (236, ILLEGAL),
            (237, ILLEGAL),
            (238, XOR),
            (239, RST),
            (240, LD),
            (241, POP),
            (242, LD),
            (243, DI),
            (244, ILLEGAL),
            (245, PUSH),
            (246, OR),
            (247, RST),
            (248, LDSP),
            (249, LDHL),
            (250, LD),
            (251, EI),
            (252, ILLEGAL),
            (253, ILLEGAL),
            (254, CP),
            (255, RST),
        ];

        std::collections::HashMap::from(LIST)
    });
