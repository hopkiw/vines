#ifndef INCLUDE_CPU_H_
#define INCLUDE_CPU_H_

#include <iostream>
#include <cstdint>

enum Flags {
  C = 1,
  Z = 2,
  I = 4,
  D = 8,
  B = 16,
  V = 32,
  N = 64,
};

class Registers {
 public:
    // Accumulator
    uint8_t A = 0;

    // Index registers
    uint8_t X = 0;
    uint8_t Y = 0;

    // Program counter
    // TODO: initial PC value?
    uint16_t PC;

    // Stack pointer
    uint8_t S = 0xfd;

    // Status register (only 6 bits used)
    uint8_t P = 0x34;
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
    uint8_t data[0xffff] {};
};

// 256x240 screen of 8x8 tiles for bg, 64 8x8 or 8x16 sprites
// Emulator authors may wish to emulate the NTSC NES/Famicom CPU at 21441960 Hz
// ((341×262−0.5)×4×60) to ensure a synchronised/stable 60 frames per second.
struct PPU {
};

// 2 pulse channel, 1 triangle channel, 1 noise channel, 1 delta modulation
// channel
struct APU {
};

// 8-bit cpu on 6502 instruction set, emulating NES NTSC chip 2A03 at 1.79Mhz
// no decimal mode
class CPU {
 public:
    CPU();
    explicit CPU(const Registers&);

    std::ostream& operator<<(std::ostream&);

    void parse();

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
    void op_nop();
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

    void test();
    void decode(uint8_t);

 private:
    Registers registers;
    Memory memory;
    PPU ppu;
    APU apu;

    void fetch();
    void execute();
};

#endif  // INCLUDE_CPU_H_
