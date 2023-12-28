#include <cstdint>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <stdexcept>

#include "./cpu.h"

void Memory::write(uint16_t addr, int8_t value) {
  data[addr] = value;
}

uint8_t Memory::read(uint16_t addr) {
  return data[addr];
}

CPU::CPU(const Registers& r) : cartridge{}, ppu{} {
  registers.A = r.A;
  registers.X = r.X;
  registers.Y = r.Y;
}

std::ostream& operator<<(std::ostream& os, const Registers& registers) {
  os << std::hex << std::setfill('0')
    << "A:"  << std::setw(2) << static_cast<int>(registers.A)
    << " X:" << std::setw(2) << static_cast<int>(registers.X)
    << " Y:" << std::setw(2) << static_cast<int>(registers.Y)
    << " PC:" << std::setw(4) << static_cast<int>(registers.PC)
    << " P:" << std::setw(2) << static_cast<int>(registers.P)
    << " S:" << std::setw(2) << static_cast<int>(registers.S);
  return os;
}

std::ostream& operator<<(std::ostream& os, const CPU& cpu) {
  os << "CPU<" << cpu.registers << ">";
  return os;
}

void CPU::write_mem(uint16_t addr, uint8_t val) {
  switch (addr) {
    case 0x2000 ... 0x3fff:
      addr = addr - 0x2000;
      switch (addr % 8) {
        case 0:
          printf("PPUCTRL set %#04x", val);
          ppu.controller = val;
          break;
        case 1:
          printf("PPUMASK set %#04x", val);
          ppu.mask = val;
          break;
        case 2:
          printf("PPUSTATUS set ... not supposed to be%#04x", val);
          break;
        case 3:
          printf("OAMADDR set %#04x", val);
          ppu.oam_address = val;
          break;
        case 4:
          printf("OAMDATA set %#04x", val);
          break;
        case 5:
          printf("PPUSCROLL set %#04x", val);
          break;
        case 6:
          printf("PPUADDR set %#04x", val);
          if (ppu.w) {
            ppu.address = ppu.address | (val << 8);
            ppu.w = 0;
          } else {
            ppu.address = val;
            ppu.w = 1;
          }
          break;
        case 7:
          printf("PPUDATA set %#04x", val);
          break;
      }
      break;
    case 0x4014:
      printf("OAMDMA set to %#04x", val);
      break;
    case 0x1800 ... 0x1fff:
      addr = addr - 0x800;
    case 0x1000 ... 0x17ff:
      addr = addr - 0x800;
    case 0x0800 ... 0x0fff:
      addr = addr - 0x800;
    case 0x0000 ... 0x07ff:  // real 2kB
      memory.write(addr, val);
      break;
    default:
      memory.write(addr, val);
      break;
  }
}

uint8_t CPU::read_mem(uint16_t addr) {
  uint8_t val = 0;
  switch (addr) {
    case 0x2000 ... 0x3fff:
      addr = addr - 0x2000;
      switch (addr % 8) {
        case 0:
          printf("PPUCTRL read");;
          val = ppu.controller;
          break;
        case 1:
          printf("PPUMASK read");;
          break;
        case 2:
          printf("PPUSTATUS read");;
          val = ppu.status;
          break;
        case 3:
          printf("OAMADDR read");;
          break;
        case 4:
          printf("OAMDATA read");;
          break;
        case 5:
          printf("PPUSCROLL read");;
          break;
        case 6:
          printf("PPUADDR read");;
          break;
        case 7:
          printf("PPUDATA read");;
          break;
      }
      break;
    case 0x4000 ... 0x4013:
      break;
    case 0x4014:
      break;
    case 0x4015:
      break;
    case 0x4016:
      break;
    case 0x4017:
      break;
    case 0x1800 ... 0x1fff:
      addr = addr - 0x800;
    case 0x1000 ... 0x17ff:
      addr = addr - 0x800;
    case 0x0800 ... 0x0fff:
      addr = addr - 0x800;
    case 0x0000 ... 0x07ff:  // real 2kB
      val = memory.read(addr);
      break;
    case 0x8000 ... 0xbfff:
      addr = addr - 0x8000;
      val = cartridge.rom[addr];
      break;
    case 0xc000 ... 0xffff:
      addr = addr - 0xc000;
      val = cartridge.rom[addr];
      break;
    default:
      val = memory.read(addr);
      break;
  }

  return val;
}

void CPU::execute() {
  if (registers.PC < 0x0100)
    throw std::runtime_error("execute from zp");

  uint8_t instruction = read_mem(registers.PC++);
  std::cout << " 0x" << std::hex << (int)instruction << ": ";
  switch (instruction) {
    case OP_ADC_IMM:
    case OP_ADC_ZP:
    case OP_ADC_ZPX:
    case OP_ADC_ABS:
    case OP_ADC_ABSX:
    case OP_ADC_ABSY:
    case OP_ADC_IZX:
    case OP_ADC_IZY:
      op_adc(instruction);
      break;

    case OP_AND_IMM:
    case OP_AND_ZP:
    case OP_AND_ZPX:
    case OP_AND_ABS:
    case OP_AND_ABSX:
    case OP_AND_ABSY:
    case OP_AND_IZX:
    case OP_AND_IZY:
      op_and(instruction);
      break;

    case OP_ASL_A:
    case OP_ASL_ZP:
    case OP_ASL_ZPX:
    case OP_ASL_ABS:
    case OP_ASL_ABSX:
      op_asl(instruction);
      break;

    case OP_BCC     :
       op_bcc();
       break;

    case OP_BCS:
       op_bcs();
       break;

    case OP_BEQ:
       op_beq();
       break;

    case OP_BIT_ZP:
    case OP_BIT_ABS:
       op_bit(instruction);
       break;

    case OP_BMI:
       op_bmi();
       break;

    case OP_BNE:
       op_bne();
       break;

    case OP_BPL:
       op_bpl();
       break;

    case OP_BRK:
       op_brk();
       break;

    case OP_BVC:
       op_bvc();
       break;

    case OP_BVS:
       op_bvs();
       break;

    case OP_CLC:
       op_clc();
       break;

    case OP_CLD:
       op_cld();
       break;

    case OP_CLI:
       op_cli();
       break;

    case OP_CLV:
       op_clv();
       break;

    case OP_CMP_IMM:
    case OP_CMP_ZP:
    case OP_CMP_ZPX:
    case OP_CMP_ABS:
    case OP_CMP_ABSX:
    case OP_CMP_ABSY:
    case OP_CMP_IZX:
    case OP_CMP_IZY:
       op_cmp(instruction);
       break;

    case OP_CPX_IMM:
    case OP_CPX_ZP:
    case OP_CPX_ABS:
       op_cpx(instruction);
       break;

    case OP_CPY_IMM:
    case OP_CPY_ZP:
    case OP_CPY_ABS:
       op_cpy(instruction);
       break;

    case OP_DEC_ZP:
    case OP_DEC_ZPX:
    case OP_DEC_ABS:
    case OP_DEC_ABSX:
       op_dec(instruction);
       break;

    case OP_DEX:
       op_dex();
       break;

    case OP_DEY:
       op_dey();
       break;

    case OP_EOR_IMM:
    case OP_EOR_ZP:
    case OP_EOR_ZPX:
    case OP_EOR_ABS:
    case OP_EOR_ABSX:
    case OP_EOR_ABSY:
    case OP_EOR_IZX:
    case OP_EOR_IZY:
       op_eor(instruction);
       break;

    case OP_INC_ZP:
    case OP_INC_ZPX:
    case OP_INC_ABS:
    case OP_INC_ABSX:
       op_inc(instruction);
       break;

    case OP_INX:
       op_inx();
       break;

    case OP_INY:
       op_iny();
       break;

    case OP_JMP_ABS:
    case OP_JMP_IND:
       op_jmp(instruction);
       break;

    case OP_JSR:
       op_jsr();
       break;

    case OP_LDA_IMM:
    case OP_LDA_ZP:
    case OP_LDA_ZPX:
    case OP_LDA_ABS:
    case OP_LDA_ABSX:
    case OP_LDA_ABSY:
    case OP_LDA_IZX:
    case OP_LDA_IZY:
       op_lda(instruction);
       break;

    case OP_LDX_IMM:
    case OP_LDX_ZP:
    case OP_LDX_ZPY:
    case OP_LDX_ABS:
    case OP_LDX_ABSY:
       op_ldx(instruction);
       break;

    case OP_LDY_IMM:
    case OP_LDY_ZP:
    case OP_LDY_ZPX:
    case OP_LDY_ABS:
    case OP_LDY_ABSX:
       op_ldy(instruction);
       break;

    case OP_LSR_A:
    case OP_LSR_ZP:
    case OP_LSR_ZPX:
    case OP_LSR_ABS:
    case OP_LSR_ABSX:
       op_lsr(instruction);
       break;

    case OP_NOP:
    case OP_NOP_IMP_01:
    case OP_NOP_IMP_02:
    case OP_NOP_IMP_03:
    case OP_NOP_IMP_04:
    case OP_NOP_IMP_05:
    case OP_NOP_IMP_06:
    case OP_NOP_IMM_01:
    case OP_NOP_IMM_02:
    case OP_NOP_IMM_03:
    case OP_NOP_IMM_04:
    case OP_NOP_IMM_05:
    case OP_NOP_ZP_01:
    case OP_NOP_ZP_02:
    case OP_NOP_ZP_03:
    case OP_NOP_ZPX_01:
    case OP_NOP_ZPX_02:
    case OP_NOP_ZPX_03:
    case OP_NOP_ZPX_04:
    case OP_NOP_ZPX_05:
    case OP_NOP_ZPX_06:
    case OP_NOP_ABS:
    case OP_NOP_ABX_01:
    case OP_NOP_ABX_02:
    case OP_NOP_ABX_03:
    case OP_NOP_ABX_04:
    case OP_NOP_ABX_05:
    case OP_NOP_ABX_06:
       op_nop(instruction);
       break;

    case OP_ORA_IMM:
    case OP_ORA_ZP:
    case OP_ORA_ZPX:
    case OP_ORA_ABS:
    case OP_ORA_ABSX:
    case OP_ORA_ABSY:
    case OP_ORA_IZX:
    case OP_ORA_IZY:
       op_ora(instruction);
       break;

    case OP_PHA:
       op_pha();
       break;

    case OP_PHP:
       op_php();
       break;

    case OP_PLA:
       op_pla();
       break;

    case OP_PLP:
       op_plp();
       break;

    case OP_ROL_A:
    case OP_ROL_ZP:
    case OP_ROL_ZPX:
    case OP_ROL_ABS:
    case OP_ROL_ABSX:
       op_rol(instruction);
       break;

    case OP_ROR_A:
    case OP_ROR_ZP:
    case OP_ROR_ZPX:
    case OP_ROR_ABS:
    case OP_ROR_ABSX:
       op_ror(instruction);
       break;

    case OP_RTI:
       op_rti();
       break;

    case OP_RTS:
       op_rts();
       break;

    case OP_SBC_IMM:
    case OP_SBC_ZP:
    case OP_SBC_ZPX:
    case OP_SBC_ABS:
    case OP_SBC_ABSX:
    case OP_SBC_ABSY:
    case OP_SBC_IZX:
    case OP_SBC_IZY:
       op_sbc(instruction);
       break;

    case OP_SEC:
       op_sec();
       break;

    case OP_SED:
       op_sed();
       break;

    case OP_SEI:
       op_sei();
       break;

    case OP_STA_ZP:
    case OP_STA_ZPX:
    case OP_STA_ABS:
    case OP_STA_ABSX:
    case OP_STA_ABSY:
    case OP_STA_IZX:
    case OP_STA_IZY:
       op_sta(instruction);
       break;

    case OP_STX_ZP:
    case OP_STX_ZPY:
    case OP_STX_ABS:
       op_stx(instruction);
       break;

    case OP_STY_ZP:
    case OP_STY_ZPX:
    case OP_STY_ABS:
       op_sty(instruction);
       break;

    case OP_TAX:
       op_tax();
       break;

    case OP_TAY:
       op_tay();
       break;

    case OP_TSX:
       op_tsx();
       break;

    case OP_TXA:
       op_txa();
       break;

    case OP_TXS:
       op_txs();
       break;

    case OP_TYA:
       op_tya();
       break;

    default:
      {
      std::stringstream ss;
      ss << "Illegal instruction 0x" << std::hex << static_cast<int>(instruction)
        << " at addr 0x" << static_cast<int>(registers.PC - 1);
      throw std::runtime_error(ss.str());
      }
      break;
  }
}

void CPU::op_adc(uint8_t instruction) {
  std::cout << "op_adc ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_ADC_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_ADC_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_ADC_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_ADC_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_ADC_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.X);
      break;
    case OP_ADC_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.Y);
      break;
    case OP_ADC_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_ADC_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }
  uint16_t subtotal = registers.A + val;
  if (registers.P & Flags::C)
    subtotal++;

  if (subtotal > (subtotal & 0xff))
    registers.P |= Flags::C;
  else
    registers.P &= ~Flags::C;

  subtotal = subtotal & 0xff;

  if (subtotal == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (subtotal & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;


  if (((registers.A & 0x80) == (val & 0x80)) &&
      ((registers.A & 0x80) != (subtotal & 0x80)))
    registers.P |= Flags::V;
  else
    registers.P &= ~Flags::V;

  registers.A = subtotal;
}

void CPU::op_and(uint8_t instruction) {
  std::cout << "op_and ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_AND_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_AND_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_AND_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_AND_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_AND_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
    case OP_AND_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
    case OP_AND_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_AND_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }

  val = registers.A & val;

  if (val == 0)
    registers.P |= Flags::Z;
  if (val & 0x80)
    registers.P |= Flags::N;

  registers.A = val;
}

void CPU::op_asl(uint8_t instruction) {
  std::cout << "op_asl ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_ASL_A:
      val = registers.A;
      break;
    case OP_ASL_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_ASL_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_ASL_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_ASL_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
  }

  uint8_t result = val << 1;

  if (result == 0) {
    registers.P |= Flags::Z;
  } else {
    registers.P &= ~Flags::Z;
    if (val & (1 << 7))
      registers.P |= Flags::C;
    else
      registers.P &= ~Flags::C;
  }

  if (result & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  if (instruction == OP_ASL_A) {
    registers.A = result;
  } else {
    write_mem(addr, result);
  }
}

void CPU::op_bcc() {
  std::cout << "op_bcc ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if ((registers.P & Flags::C) == 0)
    registers.PC = registers.PC + rel;
}

void CPU::op_bcs() {
  std::cout << "op_bcs ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if (registers.P & Flags::C)
    registers.PC = registers.PC + rel;
}

void CPU::op_beq() {
  std::cout << "op_beq ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if (registers.P & Flags::Z)
    registers.PC = registers.PC + rel;
}

void CPU::op_bit(uint8_t instruction) {
  std::cout << "op_bit ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_BIT_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_BIT_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
  }

  if (val & (1 << 7))
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  if (val & (1 << 6))
    registers.P |= Flags::V;
  else
    registers.P &= ~Flags::V;

  if ((val & registers.A) == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;
}

void CPU::op_bmi() {
  std::cout << "op_bmi ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if (registers.P & Flags::N)
    registers.PC = registers.PC + rel;
}

void CPU::op_bne() {
  std::cout << "op_bne ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if ((registers.P & Flags::Z) == 0)
    registers.PC = registers.PC + rel;
}

void CPU::op_bpl() {
  std::cout << "op_bpl ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if ((registers.P & Flags::N) == 0)
    registers.PC = registers.PC + rel;
}

void CPU::op_brk() {
  std::cout << "op_brk ";
  uint16_t retaddr;
  uint8_t status;

  registers.P |= Flags::I;

  retaddr = registers.PC + 2;
  write_mem(0x100 + registers.S--, retaddr >> 8);
  write_mem(0x100 + registers.S--, retaddr & 0xff);

  status = registers.P;
  status = status | Flags::B;
  write_mem(0x100 + registers.S--, status);

  retaddr = read_mem(0xFFFE);
  retaddr = retaddr | (read_mem(0xFFFF) << 8);

  registers.PC = retaddr;
}

void CPU::op_bvc() {
  std::cout << "op_bvc ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if ((registers.P & Flags::V) == 0)
    registers.PC = registers.PC + rel;
}

void CPU::op_bvs() {
  std::cout << "op_bvs ";
  int8_t rel = read_mem(registers.PC++);
  if (rel < 0)
    std::cout << "negative rel ";
  if (registers.P & Flags::V)
    registers.PC = registers.PC + rel;
}

void CPU::op_clc() {
  std::cout << "op_clc ";
  registers.P &= ~Flags::C;
}

void CPU::op_cld() {
  std::cout << "op_cld ";
  registers.P &= ~Flags::D;
}

void CPU::op_cli() {
  std::cout << "op_cli ";
  registers.P &= ~Flags::I;
}

void CPU::op_clv() {
  std::cout << "op_clv ";
  registers.P &= ~Flags::V;
}

void CPU::op_cmp(uint8_t instruction) {
  std::cout << "op_cmp ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_CMP_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_CMP_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_CMP_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_CMP_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_CMP_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.X);
      break;
    case OP_CMP_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.Y);
      break;
    case OP_CMP_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_CMP_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }

  if (val > registers.A)
    registers.P &= ~Flags::C;
  else
    registers.P |= Flags::C;

  val = registers.A - val;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;
}

void CPU::op_cpx(uint8_t instruction) {
  std::cout << "op_cpx ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_CPX_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_CPX_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_CPX_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
  }

  if (val > registers.X)
    registers.P &= ~Flags::C;
  else
    registers.P |= Flags::C;

  val = registers.X - val;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;
}

void CPU::op_cpy(uint8_t instruction) {
  std::cout << "op_cpy ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_CPY_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_CPY_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_CPY_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
  }

  if (val > registers.Y)
    registers.P &= ~Flags::C;
  else
    registers.P |= Flags::C;

  val = registers.Y - val;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;
}

void CPU::op_dec(uint8_t instruction) {
  std::cout << "op_dec ";
  uint16_t addr;
  switch (instruction) {
    case OP_DEC_ZP:
      addr = read_mem(registers.PC++);
      break;
    case OP_DEC_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      break;
    case OP_DEC_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      break;
    case OP_DEC_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      break;
  }
  uint16_t val = read_mem(addr) - 1;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  write_mem(addr, val);
}

void CPU::op_dex() {
  std::cout << "op_dex ";
  uint8_t val = registers.X - 1;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.X = val;
}

void CPU::op_dey() {
  std::cout << "op_dey ";
  uint8_t val = registers.Y - 1;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.Y = val;
}

void CPU::op_eor(uint8_t instruction) {
  std::cout << "op_eor ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_EOR_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_EOR_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_EOR_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_EOR_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_EOR_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
    case OP_EOR_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
    case OP_EOR_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_EOR_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }

  val = registers.A ^ val;
  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}

void CPU::op_inc(uint8_t instruction) {
  std::cout << "op_inc ";
  uint16_t addr;
  switch (instruction) {
    case OP_INC_ZP:
      addr = read_mem(registers.PC++);
      break;
    case OP_INC_ZPX:
      addr = read_mem(registers.PC++);
      addr = addr + registers.X;
      addr = addr % 0x100;
      break;
    case OP_INC_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      break;
    case OP_INC_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      break;
  }
  uint16_t val = read_mem(addr) + 1;
  if (val > 0xff)
    val = 0;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  write_mem(addr, val);
}

void CPU::op_inx() {
  std::cout << "op_inx ";
  uint16_t val = registers.X + 1;
  if (val > 0xff)
    val = 0;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.X = val;
}

void CPU::op_iny() {
  std::cout << "op_iny ";
  uint16_t val = registers.Y + 1;
  if (val > 0xff)
    val = 0;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.Y = val;
}

void CPU::op_jmp(uint8_t instruction) {
  std::cout << "op_jmp ";
  uint16_t dest, indirect;
  switch (instruction) {
    case OP_JMP_ABS:
      dest = read_mem(registers.PC++);
      dest = dest | (read_mem(registers.PC++) << 8);
      break;
    case OP_JMP_IND:
      indirect = read_mem(registers.PC++);
      indirect = indirect | (read_mem(registers.PC++) << 8);
      dest = read_mem(indirect);
      // todo: make elegant
      if ((indirect & 0x00ff) == 0x00ff)
        indirect = indirect & 0xff00;
      else
        indirect = indirect + 1;
      dest = dest | (read_mem(indirect) << 8);
      break;
  }
  registers.PC = dest;
}

void CPU::op_jsr() {
  std::cout << "op_jsr ";
  uint16_t retaddr;
  retaddr = registers.PC + 1;
  write_mem(0x100 + registers.S--, retaddr >> 8);
  write_mem(0x100 + registers.S--, retaddr & 0xff);

  uint16_t dest;
  dest = read_mem(registers.PC++);
  dest = dest | (read_mem(registers.PC++) << 8);
  registers.PC = dest;
}

void CPU::op_lda(uint8_t instruction) {
  std::cout << "op_lda ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_LDA_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_LDA_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_LDA_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_LDA_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_LDA_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      // printf("\n\nfound %02x at %04x\n", val, addr);
      break;
    case OP_LDA_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.Y);
      break;
    case OP_LDA_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_LDA_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}

void CPU::op_ldx(uint8_t instruction) {
  std::cout << "op_ldx ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_LDX_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_LDX_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_LDX_ZPY:
      addr = read_mem(registers.PC++) + registers.Y;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_LDX_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_LDX_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.Y);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.X = val;
}

void CPU::op_ldy(uint8_t instruction) {
  std::cout << "op_ldy ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_LDY_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_LDY_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_LDY_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_LDY_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_LDY_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.X);
      break;
  }

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;
  
  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.Y = val;
}

void CPU::op_lsr(uint8_t instruction) {
  std::cout << "op_lsr ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_LSR_A:
      val = registers.A;
      break;
    case OP_LSR_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_LSR_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_LSR_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_LSR_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
  }

  if (val & 0x1)
    registers.P |= Flags::C;
  else
    registers.P &= ~Flags::C;

  val = val >> 1;

  if (val == 0) {
    registers.P |= Flags::Z;
  } else {
    registers.P &= ~Flags::Z;
  }

  registers.P &= ~Flags::N;

  if (instruction == OP_LSR_A)
    registers.A = val;
  else
    write_mem(addr, val);
}

void CPU::op_nop(uint8_t instruction) {
  std::cout << "op_nop ";
  switch (instruction) {
    case OP_NOP_IMP_01:
    case OP_NOP_IMP_02:
    case OP_NOP_IMP_03:
    case OP_NOP_IMP_04:
    case OP_NOP_IMP_05:
    case OP_NOP_IMP_06:
      // no additional bytes
      break;
    case OP_NOP_ABS:
    case OP_NOP_ABX_01:
    case OP_NOP_ABX_02:
    case OP_NOP_ABX_03:
    case OP_NOP_ABX_04:
    case OP_NOP_ABX_05:
    case OP_NOP_ABX_06:
      // two bytes for above
      registers.PC++;  // no break!
    case OP_NOP_IMM_01:
    case OP_NOP_IMM_02:
    case OP_NOP_IMM_03:
    case OP_NOP_IMM_04:
    case OP_NOP_IMM_05:
    case OP_NOP_ZP_01:
    case OP_NOP_ZP_02:
    case OP_NOP_ZP_03:
    case OP_NOP_ZPX_01:
    case OP_NOP_ZPX_02:
    case OP_NOP_ZPX_03:
    case OP_NOP_ZPX_04:
    case OP_NOP_ZPX_05:
    case OP_NOP_ZPX_06:
      // one byte for above
      registers.PC++;
      break;
  }
}

void CPU::op_ora(uint8_t instruction) {
  std::cout << "op_ora ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_ORA_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_ORA_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_ORA_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_ORA_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_ORA_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.X);
      break;
    case OP_ORA_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr + registers.Y);
      break;
    case OP_ORA_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_ORA_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }

  val = val | registers.A;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}

void CPU::op_pha() {
  std::cout << "op_pha ";
  write_mem(0x100 + registers.S--, registers.A);
}

void CPU::op_php() {
  std::cout << "op_php ";
  uint8_t val = registers.P | Flags::E | Flags::B;
  write_mem(0x100 + registers.S--, val);
}

void CPU::op_pla() {
  std::cout << "op_pla ";
  uint8_t val = read_mem(0x100 + ++registers.S);

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}

void CPU::op_plp() {
  std::cout << "op_plp ";
  uint8_t val = read_mem(0x100 + ++registers.S);
  registers.P = (val & ~Flags::B) | Flags::E;  // ignore flags
}

void CPU::op_rol(uint8_t instruction) {
  std::cout << "op_rol ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_ROL_A:
      val = registers.A;
      break;
    case OP_ROL_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_ROL_ZPX:
      addr = read_mem(registers.PC++);
      addr = addr + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_ROL_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_ROL_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
  }

  val = val << 1;
  if (registers.P & Flags::C)
    val = val | 1;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  if (instruction == OP_ROL_A)
    registers.A = val;
  else
    write_mem(addr, val);
}

void CPU::op_ror(uint8_t instruction) {
  std::cout << "op_ror ";
  uint8_t val;
  uint16_t addr;
  switch (instruction) {
    case OP_ROR_A:
      val = registers.A;
      break;
    case OP_ROR_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_ROR_ZPX:
      addr = read_mem(registers.PC++);
      addr = addr + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_ROR_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_ROR_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
  }

  uint8_t result = val >> 1;
  if (registers.P & Flags::C)
    result = result | (1 << 7);

  if (val & 0x1)
    registers.P |= Flags::C;
  else
    registers.P &= ~Flags::C;

  if (result == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (result & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  if (instruction == OP_ROR_A)
    registers.A = result;
  else
    write_mem(addr, result);
}

void CPU::op_rti() {
  std::cout << "op_rti ";
  uint8_t sr;
  uint16_t pc;

  sr = read_mem(0x100 + ++registers.S);
  registers.P = sr;
  registers.P |= Flags::E;

  pc = read_mem(0x100 + ++registers.S);
  pc = pc | (read_mem(0x100 + ++registers.S) << 8);
  registers.PC = pc;
}

void CPU::op_rts() {
  std::cout << "op_rts ";
  uint16_t pc;

  pc = read_mem(0x100 + ++registers.S);
  pc = pc | (read_mem(0x100 + ++registers.S) << 8);
  registers.PC = pc + 1;
}

void CPU::op_sbc(uint8_t instruction) {
  std::cout << "op_sbc ";
  uint8_t val;
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_SBC_IMM:
      val = read_mem(registers.PC++);
      break;
    case OP_SBC_ZP:
      addr = read_mem(registers.PC++);
      val = read_mem(addr);
      break;
    case OP_SBC_ZPX:
      addr = read_mem(registers.PC++);
      addr = addr + registers.X;
      addr = addr % 0x100;
      val = read_mem(addr);
      break;
    case OP_SBC_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      val = read_mem(addr);
      break;
    case OP_SBC_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      val = read_mem(addr);
      break;
    case OP_SBC_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
    case OP_SBC_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      val = read_mem(addr);
      break;
    case OP_SBC_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      val = read_mem(addr);
      break;
  }
  val = ~val;
  uint16_t subtotal = registers.A + val;
  if (registers.P & Flags::C)
    subtotal++;

  if (subtotal > (subtotal & 0xff))
    registers.P |= Flags::C;
  else
    registers.P &= ~Flags::C;

  subtotal = subtotal & 0xff;

  if (subtotal == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (subtotal & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  if (((registers.A & 0x80) == (val & 0x80)) &&
      ((registers.A & 0x80) != (subtotal & 0x80)))
    registers.P |= Flags::V;
  else
    registers.P &= ~Flags::V;

  registers.A = subtotal;
}

void CPU::op_sec() {
  std::cout << "op_sec ";
  registers.P |= Flags::C;
}

void CPU::op_sed() {
  std::cout << "op_sed ";
  registers.P |= Flags::D;
}

void CPU::op_sei() {
  std::cout << "op_sei ";
  registers.P |= Flags::I;
}

void CPU::op_sta(uint8_t instruction) {
  std::cout << "op_sta ";
  uint16_t addr, zaddr;
  switch (instruction) {
    case OP_STA_ZP:
      addr = read_mem(registers.PC++);
      break;
    case OP_STA_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      break;
    case OP_STA_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      // printf("\n\nwrite %02x to %04x\n", registers.A, addr);
      break;
    case OP_STA_ABSX:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.X;
      break;
    case OP_STA_ABSY:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      addr = addr + registers.Y;
      break;
    case OP_STA_IZX:
      zaddr = read_mem(registers.PC++) + registers.X;
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      break;
    case OP_STA_IZY:
      zaddr = read_mem(registers.PC++);
      zaddr = zaddr % 0x100;
      addr = read_mem(zaddr);
      zaddr = (zaddr + 1) % 0x100;
      addr = addr | (read_mem(zaddr) << 8);
      addr = addr + registers.Y;
      break;
  }
  write_mem(addr, registers.A);
}

void CPU::op_stx(uint8_t instruction) {
  std::cout << "op_stx ";
  uint16_t addr;
  switch (instruction) {
    case OP_STX_ZP:
      addr = read_mem(registers.PC++);
      break;
    case OP_STX_ZPY:
      addr = read_mem(registers.PC++) + registers.Y;
      addr = addr % 0x100;
      break;
    case OP_STX_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      break;
  }
  write_mem(addr, registers.X);
}

void CPU::op_sty(uint8_t instruction) {
  std::cout << "op_sty ";
  uint16_t addr;
  switch (instruction) {
    case OP_STY_ZP:
      addr = read_mem(registers.PC++);
      break;
    case OP_STY_ZPX:
      addr = read_mem(registers.PC++) + registers.X;
      addr = addr % 0x100;
      break;
    case OP_STY_ABS:
      addr = read_mem(registers.PC++);
      addr = addr | (read_mem(registers.PC++) << 8);
      break;
  }
  write_mem(addr, registers.Y);
}

void CPU::op_tax() {
  std::cout << "op_tax ";
  uint8_t val = registers.A;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.X = val;
}

void CPU::op_tay() {
  std::cout << "op_tay ";
  uint8_t val = registers.A;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.Y = val;
}

void CPU::op_tsx() {
  std::cout << "op_tsx ";
  uint8_t val = registers.S;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.X = val;
}

void CPU::op_txa() {
  std::cout << "op_txa ";
  uint8_t val = registers.X;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}

void CPU::op_txs() {
  std::cout << "op_txs ";
  registers.S = registers.X;
}

void CPU::op_tya() {
  std::cout << "op_tya ";
  uint8_t val = registers.Y;

  if (val == 0)
    registers.P |= Flags::Z;
  else
    registers.P &= ~Flags::Z;

  if (val & 0x80)
    registers.P |= Flags::N;
  else
    registers.P &= ~Flags::N;

  registers.A = val;
}
