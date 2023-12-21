#include <cstdint>
#include <iostream>

#include "./cpu.h"

void Memory::write(uint16_t addr, int8_t value) {
  data[addr] = value;
}

uint8_t Memory::read(uint16_t addr) {
  return data[addr];
}

CPU::CPU(const Registers& r) {
  registers.A = r.A;
  registers.X = r.X;
  registers.Y = r.Y;
}

std::ostream& operator<<(std::ostream& os, const Registers& registers) {
  os
    << "A:" << static_cast<int>(registers.A)
    << " X:" << static_cast<int>(registers.X)
    << " Y:" << static_cast<int>(registers.Y)
    << " PC:" << static_cast<int>(registers.PC)
    << " S:" << static_cast<int>(registers.S)
    << " P:" << static_cast<int>(registers.P);
  return os;
}

std::ostream& CPU::operator<<(std::ostream& os) {
  os << "CPU<" << registers << ">";
  return os;
}

void CPU::fetch() {
}

void CPU::decode(uint8_t byte) {
  uint8_t instruction = memory.read(registers.PC++);
  switch (instruction) {
    case 0x69:
    case 0x65:
    case 0x75:
    case 0x6D:
    case 0x7D:
    case 0x79:
    case 0x61:
    case 0x71:
      op_adc(instruction);
      break;

    case 0x29:
    case 0x25:
    case 0x2D:
    case 0x3D:
    case 0x35:
    case 0x39:
    case 0x21:
    case 0x31:
      op_and(instruction);
      break;

    case 0x0A:
    case 0x06:
    case 0x16:
    case 0x0E:
    case 0x1E:
      op_asl(instruction);
      break;

    case 0x90:
      op_bcc();
      break;

    case 0xb0:
      op_bcs();
      break;

    case 0xf0:
      op_beq();
      break;

    case 0x24:
    case 0x2c:
      op_bit(instruction);
      break;

    case 0x30:
      op_bmi();
      break;

    case 0xd0:
      op_bne();
      break;

    case 0x10:
      op_bpl();
      break;

    case 0x00:
      op_brk();
      break;

    case 0x50:
      op_bvc();
      break;

    case 0x70:
      op_bvs();
      break;

    case 0x18:
      op_clc();
      break;

    case 0xd8:
      op_cld();
      break;

    case 0x58:
      op_cli();
      break;

    case 0xb8:
      op_clv();
      break;

    case 0xC9:
    case 0xC5:
    case 0xD5:
    case 0xCD:
    case 0xDD:
    case 0xD9:
    case 0xC1:
    case 0xD1:
      op_cmp(instruction);
      break;

    case 0xE0:
    case 0xE4:
    case 0xEC:
      op_cpx(instruction);
      break;

    case 0xC0:
    case 0xC4:
    case 0xCC:
      op_cpy(instruction);
      break;

    case 0xC6:
    case 0xD6:
    case 0xCE:
    case 0xDE:
      op_dec(instruction);
      break;

    case 0xCA:
      op_dex();
      break;

    case 0x88:
      op_dey();
      break;

    case 0x49:
    case 0x45:
    case 0x55:
    case 0x4D:
    case 0x5D:
    case 0x59:
    case 0x41:
    case 0x51:
      op_eor(instruction);
      break;

    case 0xE6:
    case 0xF6:
    case 0xEE:
    case 0xFE:
      op_inc(instruction);
      break;

    case 0xE8:
      op_inx();
      break;

    case 0xC8:
      op_iny();
      break;

    case 0x4C:
    case 0x6C:
      op_jmp(instruction);
      break;

    case 0x20:
      op_jsr();
      break;

    case 0xA9:
    case 0xA5:
    case 0xB5:
    case 0xAD:
    case 0xBD:
    case 0xB9:
    case 0xA1:
    case 0xB1:
      op_lda(instruction);
      break;

    case 0xA2:
    case 0xA6:
    case 0xB6:
    case 0xAE:
    case 0xBE:
      op_ldx(instruction);
      break;

    case 0xA0:
    case 0xA4:
    case 0xB4:
    case 0xAC:
    case 0xBC:
      op_ldy(instruction);
      break;

    case 0x4A:
    case 0x46:
    case 0x56:
    case 0x4E:
    case 0x5E:
      op_lsr(instruction);
      break;

    case 0xEA:
      op_nop();
      break;

    case 0x09:
    case 0x05:
    case 0x15:
    case 0x0D:
    case 0x1D:
    case 0x19:
    case 0x01:
    case 0x11:
      op_ora(instruction);
      break;

    case 0x48:
      op_pha();
      break;

    case 0x08:
      op_php();
      break;

    case 0x68:
      op_plp();
      break;

    case 0x2A:
    case 0x26:
    case 0x36:
    case 0x2E:
    case 0x3E:
      op_rol(instruction);
      break;

    case 0x6A:
    case 0x66:
    case 0x76:
    case 0x6E:
    case 0x7E:
      op_ror(instruction);
      break;

    case 0x40:
      op_rti();
      break;

    case 0x60:
      op_rts();
      break;

    case 0xE9:
    case 0xE5:
    case 0xF5:
    case 0xED:
    case 0xFD:
    case 0xF9:
    case 0xE1:
    case 0xF1:
      op_sbc(instruction);
      break;

    case 0x38:
      op_sec();
      break;

    case 0xF8:
      op_sed();
      break;

    case 0x78:
      op_sei();
      break;

    case 0x85:
    case 0x95:
    case 0x8D:
    case 0x9D:
    case 0x99:
    case 0x81:
    case 0x91:
      op_sta(instruction);
      break;

    case 0x86:
    case 0x96:
    case 0x8E:
      op_stx(instruction);
      break;

    case 0x84:
    case 0x94:
    case 0x8C:
      op_sty(instruction);
      break;

    case 0xAA:
      op_tax();
      break;

    case 0xA8:
      op_tay();
      break;

    case 0xBA:
      op_tsx();
      break;

    case 0x8A:
      op_txa();
      break;

    case 0x9A:
      op_txs();
      break;

    case 0x98:
      op_tya();
      break;

    default:
      std::cout << "Illegal instruction: " << instruction << std::endl;
      break;
  }
}

void CPU::op_adc(uint8_t) {
}

void CPU::op_and(uint8_t) {
}


void CPU::op_asl(uint8_t) {
}

void CPU::op_bcc() {
}

void CPU::op_bcs() {
}

void CPU::op_beq() {
}

void CPU::op_bit(uint8_t) {
}

void CPU::op_bmi() {
}

void CPU::op_bne() {
}

void CPU::op_bpl() {
}

void CPU::op_brk() {
}

void CPU::op_bvc() {
}

void CPU::op_bvs() {
}

void CPU::op_clc() {
}

void CPU::op_cld() {
}

void CPU::op_cli() {
}

void CPU::op_clv() {
}

void CPU::op_cmp(uint8_t) {
}

void CPU::op_cpx(uint8_t) {
}

void CPU::op_cpy(uint8_t) {
}

void CPU::op_dec(uint8_t) {
}

void CPU::op_dex() {
  registers.A = registers.X;
  if (registers.A == 0)
    registers.P |= Flags::Z;
  else if (registers.A & 0x80)
    registers.P |= Flags::N;
}

void CPU::op_dey() {
  registers.A = registers.X;
  if (registers.A == 0)
    registers.P |= Flags::Z;
  else if (registers.A & 0x80)
    registers.P |= Flags::N;
}

void CPU::op_eor(uint8_t) {
}

void CPU::op_inc(uint8_t) {
}

void CPU::op_inx() {
}

void CPU::op_iny() {
}

void CPU::op_jmp(uint8_t) {
}

void CPU::op_jsr() {
}

void CPU::op_lda(uint8_t instruction) {
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case 0xA9:
      // immediate
      val = memory.read(registers.PC++);
      break;
    case 0xA5:
      // zp
      addr = memory.read(registers.PC++);
      val = memory.read(addr);
      break;
    case 0xB5:
      // zp,x
      addr = memory.read(registers.PC++) + registers.X;
      val = memory.read(addr);
      break;
    case 0xAD:
      // abs
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr);
      break;
    case 0xBD:
      // abs,x
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr + registers.X);
      break;
    case 0xB9:
      // abs,y
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr + registers.Y);
      break;
    case 0xA1:
      // izx
      zaddr = memory.read(registers.PC++) + registers.X;
      addr = memory.read(zaddr);
      addr = addr & (memory.read(zaddr + 1) << 8);
      val = memory.read(addr);
      break;
    case 0xB1:
      // izy
      zaddr = memory.read(registers.PC++) + registers.Y;
      addr = memory.read(zaddr);
      addr = addr & (memory.read(zaddr + 1) << 8);
      val = memory.read(addr);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;

  registers.A = val;
}

void CPU::op_ldx(uint8_t instruction) {
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case 0xA2:
      // immediate
      val = memory.read(registers.PC++);
      break;
    case 0xA6:
      // zp
      addr = memory.read(registers.PC++);
      val = memory.read(addr);
      break;
    case 0xB6:
      // zp,y
      addr = memory.read(registers.PC++) + registers.Y;
      val = memory.read(addr);
      break;
    case 0xAE:
      // abs
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr);
      break;
    case 0xBE:
      // abs,y
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr + registers.Y);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;

  registers.X = val;
}

void CPU::op_ldy(uint8_t instruction) {
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case 0xA0:
      // immediate
      val = memory.read(registers.PC++);
      break;
    case 0xA4:
      // zp
      addr = memory.read(registers.PC++);
      val = memory.read(addr);
      break;
    case 0xB4:
      // zp,x
      addr = memory.read(registers.PC++) + registers.X;
      val = memory.read(addr);
      break;
    case 0xAC:
      // abs
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr);
      break;
    case 0xBC:
      // abs,y
      addr = memory.read(registers.PC++);
      addr = addr & (memory.read(registers.PC++) << 8);
      val = memory.read(addr + registers.X);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;

  registers.Y = val;
}

void CPU::op_lsr(uint8_t instruction) {
}

void CPU::op_nop() {
}

void CPU::op_ora(uint8_t instruction) {
}

void CPU::op_pha() {
}

void CPU::op_php() {
}

void CPU::op_pla() {
}

void CPU::op_plp() {
}

void CPU::op_rol(uint8_t instruction) {
}

void CPU::op_ror(uint8_t instruction) {
}

void CPU::op_rti() {
}

void CPU::op_rts() {
}

void CPU::op_sbc(uint8_t instruction) {
}

void CPU::op_sec() {
}

void CPU::op_sed() {
}

void CPU::op_sei() {
}

void CPU::op_sta(uint8_t instruction) {
}

void CPU::op_stx(uint8_t instruction) {
}

void CPU::op_sty(uint8_t instruction) {
}

void CPU::op_tax() {
  if (registers.A == 0)
    registers.P |= Flags::Z;
  else if (registers.A & 0x80)
    registers.P |= Flags::N;

  registers.X = registers.A;
}

void CPU::op_tay() {
  if (registers.A == 0)
    registers.P |= Flags::Z;
  else if (registers.A & 0x80)
    registers.P |= Flags::N;

  registers.Y = registers.A;
}

void CPU::op_tsx() {
  if (registers.S == 0)
    registers.P |= Flags::Z;
  else if (registers.S & 0x80)
    registers.P |= Flags::N;

  registers.X = registers.S;
}

void CPU::op_txa() {
  if (registers.X == 0)
    registers.P |= Flags::Z;
  else if (registers.X & 0x80)
    registers.P |= Flags::N;

  registers.A = registers.X;
}

void CPU::op_txs() {
  if (registers.X == 0)
    registers.P |= Flags::Z;
  else if (registers.X & 0x80)
    registers.P |= Flags::N;

  registers.S = registers.X;
}

void CPU::op_tya() {
  if (registers.Y == 0)
    registers.P |= Flags::Z;
  else if (registers.Y & 0x80)
    registers.P |= Flags::N;

  registers.A = registers.Y;
}
