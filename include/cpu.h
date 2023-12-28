#ifndef INCLUDE_CPU_H_
#define INCLUDE_CPU_H_

#include <iostream>
#include <cstdint>

// Opcodes
#define  OP_ADC_IMM    0x69
#define  OP_ADC_ZP     0x65
#define  OP_ADC_ZPX    0x75
#define  OP_ADC_ABS    0x6D
#define  OP_ADC_ABSX   0x7D
#define  OP_ADC_ABSY   0x79
#define  OP_ADC_IZX    0x61
#define  OP_ADC_IZY    0x71
#define  OP_AND_IMM    0x29
#define  OP_AND_ZP     0x25
#define  OP_AND_ZPX    0x35
#define  OP_AND_ABS    0x2D
#define  OP_AND_ABSX   0x3D
#define  OP_AND_ABSY   0x39
#define  OP_AND_IZX    0x21
#define  OP_AND_IZY    0x31
#define  OP_ASL_A      0x0A
#define  OP_ASL_ZP     0x06
#define  OP_ASL_ZPX    0x16
#define  OP_ASL_ABS    0x0E
#define  OP_ASL_ABSX   0x1E
#define  OP_BCC        0x90
#define  OP_BCS        0xb0
#define  OP_BEQ        0xf0
#define  OP_BIT_ZP     0x24
#define  OP_BIT_ABS    0x2c
#define  OP_BMI        0x30
#define  OP_BNE        0xd0
#define  OP_BPL        0x10
#define  OP_BRK        0x00
#define  OP_BVC        0x50
#define  OP_BVS        0x70
#define  OP_CLC        0x18
#define  OP_CLD        0xd8
#define  OP_CLI        0x58
#define  OP_CLV        0xb8
#define  OP_CMP_IMM    0xC9
#define  OP_CMP_ZP     0xC5
#define  OP_CMP_ZPX    0xD5
#define  OP_CMP_ABS    0xCD
#define  OP_CMP_ABSX   0xDD
#define  OP_CMP_ABSY   0xD9
#define  OP_CMP_IZX    0xC1
#define  OP_CMP_IZY    0xD1
#define  OP_CPX_IMM    0xE0
#define  OP_CPX_ZP     0xE4
#define  OP_CPX_ABS    0xEC
#define  OP_CPY_IMM    0xC0
#define  OP_CPY_ZP     0xC4
#define  OP_CPY_ABS    0xCC
#define  OP_DEC_ZP     0xC6
#define  OP_DEC_ZPX    0xD6
#define  OP_DEC_ABS    0xCE
#define  OP_DEC_ABSX   0xDE
#define  OP_DEX        0xCA
#define  OP_DEY        0x88
#define  OP_EOR_IMM    0x49
#define  OP_EOR_ZP     0x45
#define  OP_EOR_ZPX    0x55
#define  OP_EOR_ABS    0x4D
#define  OP_EOR_ABSX   0x5D
#define  OP_EOR_ABSY   0x59
#define  OP_EOR_IZX    0x41
#define  OP_EOR_IZY    0x51
#define  OP_INC_ZP     0xE6
#define  OP_INC_ZPX    0xF6
#define  OP_INC_ABS    0xEE
#define  OP_INC_ABSX   0xFE
#define  OP_INX        0xE8
#define  OP_INY        0xC8
#define  OP_JMP_ABS    0x4C
#define  OP_JMP_IND    0x6C
#define  OP_JSR        0x20
#define  OP_LDA_IMM    0xA9
#define  OP_LDA_ZP     0xA5
#define  OP_LDA_ZPX    0xB5
#define  OP_LDA_ABS    0xAD
#define  OP_LDA_ABSX   0xBD
#define  OP_LDA_ABSY   0xB9
#define  OP_LDA_IZX    0xA1
#define  OP_LDA_IZY    0xB1
#define  OP_LDX_IMM    0xA2
#define  OP_LDX_ZP     0xA6
#define  OP_LDX_ZPY    0xB6
#define  OP_LDX_ABS    0xAE
#define  OP_LDX_ABSY   0xBE
#define  OP_LDY_IMM    0xA0
#define  OP_LDY_ZP     0xA4
#define  OP_LDY_ZPX    0xB4
#define  OP_LDY_ABS    0xAC
#define  OP_LDY_ABSX   0xBC
#define  OP_LSR_A      0x4A
#define  OP_LSR_ZP     0x46
#define  OP_LSR_ZPX    0x56
#define  OP_LSR_ABS    0x4E
#define  OP_LSR_ABSX   0x5E
#define  OP_NOP        0xEA
// "illegal" NOP codes
#define  OP_NOP_IMP_01 0x1A
#define  OP_NOP_IMP_02 0x3A
#define  OP_NOP_IMP_03 0x5A
#define  OP_NOP_IMP_04 0x7A
#define  OP_NOP_IMP_05 0xDA
#define  OP_NOP_IMP_06 0xFA
#define  OP_NOP_IMM_01 0x80
#define  OP_NOP_IMM_02 0x82
#define  OP_NOP_IMM_03 0x89
#define  OP_NOP_IMM_04 0xC2
#define  OP_NOP_IMM_05 0xE2
#define  OP_NOP_ZP_01  0x04
#define  OP_NOP_ZP_02  0x44
#define  OP_NOP_ZP_03  0x64
#define  OP_NOP_ZPX_01 0x14
#define  OP_NOP_ZPX_02 0x34
#define  OP_NOP_ZPX_03 0x54
#define  OP_NOP_ZPX_04 0x74
#define  OP_NOP_ZPX_05 0xD4
#define  OP_NOP_ZPX_06 0xF4
#define  OP_NOP_ABS    0x0C
#define  OP_NOP_ABX_01 0x1C
#define  OP_NOP_ABX_02 0x3C
#define  OP_NOP_ABX_03 0x5C
#define  OP_NOP_ABX_04 0x7C
#define  OP_NOP_ABX_05 0xDC
#define  OP_NOP_ABX_06 0xFC
#define  OP_ORA_IMM    0x09
#define  OP_ORA_ZP     0x05
#define  OP_ORA_ZPX    0x15
#define  OP_ORA_ABS    0x0D
#define  OP_ORA_ABSX   0x1D
#define  OP_ORA_ABSY   0x19
#define  OP_ORA_IZX    0x01
#define  OP_ORA_IZY    0x11
#define  OP_PHA        0x48
#define  OP_PHP        0x08
#define  OP_PLA        0x68
#define  OP_PLP        0x28
#define  OP_ROL_A      0x2A
#define  OP_ROL_ZP     0x26
#define  OP_ROL_ZPX    0x36
#define  OP_ROL_ABS    0x2E
#define  OP_ROL_ABSX   0x3E
#define  OP_ROR_A      0x6A
#define  OP_ROR_ZP     0x66
#define  OP_ROR_ZPX    0x76
#define  OP_ROR_ABS    0x6E
#define  OP_ROR_ABSX   0x7E
#define  OP_RTI        0x40
#define  OP_RTS        0x60
#define  OP_SBC_IMM    0xE9
#define  OP_SBC_ZP     0xE5
#define  OP_SBC_ZPX    0xF5
#define  OP_SBC_ABS    0xED
#define  OP_SBC_ABSX   0xFD
#define  OP_SBC_ABSY   0xF9
#define  OP_SBC_IZX    0xE1
#define  OP_SBC_IZY    0xF1
#define  OP_SEC        0x38
#define  OP_SED        0xF8
#define  OP_SEI        0x78
#define  OP_STA_ZP     0x85
#define  OP_STA_ZPX    0x95
#define  OP_STA_ABS    0x8D
#define  OP_STA_ABSX   0x9D
#define  OP_STA_ABSY   0x99
#define  OP_STA_IZX    0x81
#define  OP_STA_IZY    0x91
#define  OP_STX_ZP     0x86
#define  OP_STX_ZPY    0x96
#define  OP_STX_ABS    0x8E
#define  OP_STY_ZP     0x84
#define  OP_STY_ZPX    0x94
#define  OP_STY_ABS    0x8C
#define  OP_TAX        0xAA
#define  OP_TAY        0xA8
#define  OP_TSX        0xBA
#define  OP_TXA        0x8A
#define  OP_TXS        0x9A
#define  OP_TYA        0x98

enum Flags {
  C = 1 << 0,
  Z = 1 << 1,
  I = 1 << 2,
  D = 1 << 3,
  B = 1 << 4,
  E = 1 << 5,
  V = 1 << 6,
  N = 1 << 7,
};

class Registers {
 public:
    // Accumulator
    uint8_t A = 0;

    // Index registers
    uint8_t X = 0;
    uint8_t Y = 0;

    // Program counter
    // TODO(liam): initial PC value?
    uint16_t PC;

    // Stack pointer
    uint8_t S = 0xfd;

    // Status register (only 6 bits used)
    uint8_t P = 0x24;
};

// 16-bit addressable; 2kb present. little endian.
// first 256kb page is 'zero page'
// second page is system stack
//
// last 6 bytes of memory $FFFA to $FFFF must be programmed with the addresses
// of the non-maskable interrupt handler ($FFFA/B), the power on reset location
// ($FFFC/D) and the BRK/interrupt request handler ($FFFE/F) respectively.
//
// 22 memory-mapped registers for various purposes from $4000-$401f
// $4020-$ffff is available to the game
class Memory {
 public:
    Memory() {}

    void write(uint16_t, int8_t);
    uint8_t read(uint16_t);

 private:
    // TODO: should be 0x3fff for vram
    uint8_t data[0xffff] {};
};

// 256x240 screen of 8x8 tiles for bg, 64 8x8 or 8x16 sprites
// Emulator authors may wish to emulate the NTSC NES/Famicom CPU at 21441960 Hz
// ((341×262−0.5)×4×60) to ensure a synchronised/stable 60 frames per second.
//
// The PPU addresses a 14-bit (16kB) address space, $0000-3FFF
// The NES has 2kB of RAM dedicated to the PPU, normally mapped to the nametable
// address space from $2000-2FFF
struct PPU {
  // mapped registers
  uint8_t controller,
          mask,
          status,
          oam_address,
          oam_data,
          scroll,
          address,
          data,
          oam_dma,
          // "internal" registers
          v,
          t,
          x,
          w;
  Memory vram;
  uint8_t oam[256];
};

// 2 pulse channel, 1 triangle channel, 1 noise channel, 1 delta modulation
// channel
struct APU {
};

struct Cartridge {
  uint8_t rom[0x4000];  // 16kB
};

// 8-bit cpu on 6502 instruction set, emulating NES NTSC chip 2A03 at 1.79Mhz
// no decimal mode
class CPU {
 public:
    CPU();
    explicit CPU(const Registers&);

//    std::ostream& operator<<(std::ostream&);
    void write_mem(uint16_t, uint8_t);
    uint8_t read_mem(uint16_t);

    void op_adc(uint8_t);
    void op_and(uint8_t);
    void op_asl(uint8_t);
    void op_bcc();
    void op_bcs();
    void op_beq();
    void op_bit(uint8_t);
    void op_bmi();
    void op_bne();
    void op_bpl();
    void op_brk();
    void op_bvc();
    void op_bvs();
    void op_clc();
    void op_cld();
    void op_cli();
    void op_clv();
    void op_cmp(uint8_t);
    void op_cpx(uint8_t);
    void op_cpy(uint8_t);
    void op_dec(uint8_t);
    void op_dex();
    void op_dey();
    void op_eor(uint8_t);
    void op_inc(uint8_t);
    void op_inx();
    void op_iny();
    void op_jmp(uint8_t);
    void op_jsr();
    void op_lda(uint8_t);
    void op_ldx(uint8_t);
    void op_ldy(uint8_t);
    void op_lsr(uint8_t);
    void op_nop(uint8_t);
    void op_ora(uint8_t);
    void op_pha();
    void op_php();
    void op_pla();
    void op_plp();
    void op_rol(uint8_t);
    void op_ror(uint8_t);
    void op_rti();
    void op_rts();
    void op_sbc(uint8_t);
    void op_sec();
    void op_sed();
    void op_sei();
    void op_sta(uint8_t);
    void op_stx(uint8_t);
    void op_sty(uint8_t);
    void op_tax();
    void op_tay();
    void op_tsx();
    void op_txa();
    void op_txs();
    void op_tya();

    void execute();

    // TODO: put these in private
    Memory memory;
    Registers registers;
    Cartridge cartridge;

 private:
    PPU ppu;
    APU apu;
};

std::ostream& operator<<(std::ostream&, const CPU&);


#endif  // INCLUDE_CPU_H_
