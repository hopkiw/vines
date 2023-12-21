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
    case 0x00:
      op_brk();
      break;
    case 0x01:
      // izx
      op_ora();
      break;
    case 0x02:
      // illegal
      break;
    case 0x03:
      // illegal
      break;
    case 0x04:
      // illegal
      // NOP
      break;
    case 0x05:
      // zp
      op_ora();
      break;
    case 0x06:
      // zp
      op_asl();
      break;
    case 0x07:
      // illegal
      break;
    case 0x08:
      op_php();
      break;
    case 0x09:
      // immediate
      op_ora();
      break;
    case 0x0a:
      // asl implicit a
      op_asl(registers.A);
      break;
    case 0x0b:
      // illegal
      break;
    case 0x0c:
      // illegal
      break;
    case 0x0d:
      // abs
      op_ora();
      break;
    case 0x0e:
      // abs
      op_asl();
      break;
    case 0x0f:
      // illegal
      break;
    case 0x10:
      // rel
      op_bpl();
      break;
    case 0x11:
      // izy
      op_ora();
      break;
    case 0x12:
      // illegal
      break;
    case 0x13:
      // illegal
      break;
    case 0x14:
      // illegal
      break;
    case 0x15:
      // zp,x
      break;
    case 0x16:
      // zp,x
      op_asl();
      break;
    case 0x17:
      // illegal
      break;
    case 0x18:
      op_clc();
      break;
    case 0x19:
      // aby
      op_ora();
      break;
    case 0x1a:
      // illegal
      break;
    case 0x1b:
      // illegal
      break;
    case 0x1c:
      // illegal
      break;
    case 0x1d:
      // abx
      op_ora();
      break;
    case 0x1e:
      // abx
      op_asl();
      break;
    case 0x1f:
      // illegal
      break;
    case 0x20:
      // abs
      op_jsr();
      break;
    case 0x21:
      // izx
      op_and();
      break;
    case 0x22:
      // illegal
      break;
    case 0x23:
      // illegal
      break;
    case 0x24:
      // zp
      op_bit();
      break;
    case 0x25:
      // zp
      break;
    case 0x26:
      // zp
      op_rol();
      break;
    case 0x27:
      // illegal
      break;
    case 0x28:
      op_plp();
      break;
    case 0x29:
      // immediate
      op_and();
      break;
    case 0x2a:
      // implicit a
      op_rol();
      break;
    case 0x2b:
      // illegal
      break;
    case 0x2c:
      // abs
      op_dex();
      break;
    case 0x2d:
      // abs
      op_and();
      break;
    case 0x2e:
      // abs
      op_rol();
      break;
    case 0x2f:
      // illegal
      break;
    case 0x30:
      // rel
      op_bmi();
      break;
    case 0x31:
      // izy
      op_and();
      break;
    case 0x32:
      // illegal
      break;
    case 0x33:
      // illegal
      break;
    case 0x34:
      // illegal
      break;
    case 0x35:
      // zp,x
      break;
    case 0x36:
      // zp,x
      op_rol();
      break;
    case 0x37:
      // illegal
      break;
    case 0x38:
      op_sec();
      break;
    case 0x39:
      // aby
      op_and();
      break;
    case 0x3a:
      // illegal
      break;
    case 0x3b:
      // illegal
      break;
    case 0x3c:
      // illegal
      break;
    case 0x3d:
      // abx
      op_and();
      break;
    case 0x3e:
      // abx
      op_rol();
      break;
    case 0x3f:
      // illegal
      break;
    case 0x40:
      op_rti();
      break;
    case 0x41:
      // izx
      op_eor();
      break;
    case 0x42:
      // illegal
      break;
    case 0x43:
      // illegal
      break;
    case 0x44:
      // illegal
      break;
    case 0x45:
      // zp
      break;
    case 0x46:
      // zp
      op_lsr();
      break;
    case 0x47:
      // illegal
      break;
    case 0x48:
      op_pha();
      break;
    case 0x49:
      // immediate
      op_eor();
      break;
    case 0x4a:
      // implicit a
      op_lsr();
      break;
    case 0x4b:
      // illegal
      break;
    case 0x4c:
      // abs
      op_jmp();
      break;
    case 0x4d:
      // abs
      op_eor();
      break;
    case 0x4e:
      // abs
      op_lsr();
      break;
    case 0x4f:
      // illegal
      break;
    case 0x50:
      // rel
      op_bvc();
      break;
    case 0x51:
      // izy
      op_eor();
      break;
    case 0x52:
      // illegal
      break;
    case 0x53:
      // illegal
      break;
    case 0x54:
      // illegal
      break;
    case 0x55:
      // zp,x
      break;
    case 0x56:
      // zp,x
      op_lsr();
      break;
    case 0x57:
      // illegal
      break;
    case 0x58:
      op_cli();
      break;
    case 0x59:
      // aby
      op_eor();
      break;
    case 0x5a:
      // illegal
      break;
    case 0x5b:
      // illegal
      break;
    case 0x5c:
      // illegal
      break;
    case 0x5d:
      // abx
      op_eor();
      break;
    case 0x5e:
      // abx
      op_lsr();
      break;
    case 0x5f:
      // illegal
      break;
    case 0x60:
      op_rts();
      break;
    case 0x61:
      // izx
      op_adc();
      break;
    case 0x62:
      // illegal
      break;
    case 0x63:
      // illegal
      break;
    case 0x64:
      // illegal
      break;
    case 0x65:
      // zp
      break;
    case 0x66:
      // zp
      op_ror();
      break;
    case 0x67:
      // illegal
      break;
    case 0x68:
      op_pla();
      break;
    case 0x69:
      // immediate
      op_adc();
      break;
    case 0x6a:
      // implicit a
      op_ror();
      break;
    case 0x6b:
      // illegal
      break;
    case 0x6c:
      op_jmp();
      break;
    case 0x6d:
      // abs
      op_adc();
      break;
    case 0x6e:
      // abs
      op_ror();
      break;
    case 0x6f:
      // illegal
      break;
    case 0x70:
      // rel
      op_bvs();
      break;
    case 0x71:
      // izy
      op_adc();
      break;
    case 0x72:
      // illegal
      break;
    case 0x73:
      // illegal
      break;
    case 0x74:
      // illegal
      break;
    case 0x75:
      // zp,x
      break;
    case 0x76:
      // zp,x
      op_ror();
      break;
    case 0x77:
      // illegal
      break;
    case 0x78:
      op_sei();
      break;
    case 0x79:
      // aby
      op_adc();
      break;
    case 0x7a:
      // illegal
      break;
    case 0x7b:
      // illegal
      break;
    case 0x7c:
      // illegal
      break;
    case 0x7d:
      // abx
      op_adc();
      break;
    case 0x7e:
      // abx
      op_ror();
      break;
    case 0x7f:
      // illegal
      break;
    case 0x80:
      // illegal
      break;
    case 0x81:
      // izx
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.X;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        op_sta(addr);
      }
      break;
    case 0x82:
      // illegal
      break;
    case 0x83:
      // illegal
      break;
    case 0x84:
      // zpg
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sty(addr);
      }
      break;
    case 0x85:
      // zp
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sta(addr);
      }
      break;
    case 0x86:
      // zp
      {
        uint16_t addr = memory.read(registers.PC++);
        op_stx(addr);
      }
      break;
    case 0x87:
      // illegal
      break;
    case 0x88:
      op_dey();
      break;
    case 0x89:
      // illegal
      break;
    case 0x8a:
      op_txa();
      break;
    case 0x8b:
      // illegal
      break;
    case 0x8c:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        op_sty(addr);
      }
      break;
    case 0x8d:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        op_sta(addr);
      }
      break;
    case 0x8e:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        op_stx(addr);
      }
      break;
    case 0x8f:
      // illegal
      break;
    case 0x90:
      // rel
      op_bcc();
      break;
    case 0x91:
      // izy
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.Y;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        op_sta(addr);
      }
      break;
    case 0x92:
      // illegal
      break;
    case 0x93:
      // illegal
      break;
    case 0x94:
      // zp,x
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sty(addr + registers.X);
      }
      break;
    case 0x95:
      // zp,x
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sta(addr + registers.X);
      }
      break;
    case 0x96:
      // zp,y
      {
        uint16_t addr = memory.read(registers.PC++);
        op_stx(addr + registers.Y);
      }
      break;
    case 0x97:
      // illegal
      break;
    case 0x98:
      op_tya();
      break;
    case 0x99:
      // aby
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.Y;
        op_sta(addr);
      }
      break;
    case 0x9a:
      op_txs();
      break;
    case 0x9b:
      // illegal
      break;
    case 0x9c:
      // illegal
      break;
    case 0x9d:
      // abx
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.X;
        op_sta(addr);
      }
      break;
    case 0x9e:
      // illegal
      break;
    case 0x9f:
      // illegal
      break;
    case 0xa0:
      // immediate
      {
        uint8_t immediate = memory.read(registers.PC++);
        op_ldx(immediate);
      }
      break;
    case 0xa1:
      // izx
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.X;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xa2:
      // illegal
      {
        uint8_t immediate = memory.read(registers.PC++);
        op_ldx(immediate);
      }
      break;
    case 0xa3:
      // illegal
      break;
    case 0xa4:
      // zp
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_ldy(val);
      }
      break;
    case 0xa5:
      // zp
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xa6:
      // zp
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_ldx(val);
      }
      break;
    case 0xa7:
      // illegal
      break;
    case 0xa8:
      op_tay();
      break;
    case 0xa9:
      // immediate
      {
        uint8_t immediate  = memory.read(registers.PC++);
        op_lda(immediate);
      }
      break;
    case 0xaa:
      op_tax();
      break;
    case 0xab:
      // illegal
      break;
    case 0xac:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        uint8_t val = memory.read(addr);
        op_ldy(val);
      }
      break;
    case 0xad:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xae:
      // abs
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        uint8_t val = memory.read(addr);
        op_ldx(val);
      }
      break;
    case 0xaf:
      // illegal
      break;
    case 0xb0:
      // rel
      op_bcs();
      break;
    case 0xb1:
      // izy
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.Y;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xb2:
      // illegal
      break;
    case 0xb3:
      // illegal
      break;
    case 0xb4:
      // zp,x
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr + registers.X);
        op_ldy(val);
      }
      break;
    case 0xb5:
      // zp,x
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr + registers.X);
        op_lda(val);
      }
      break;
    case 0xb6:
      // zp,y
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr + registers.Y);
        op_ldx(val);
      }
      break;
    case 0xb7:
      // illegal
      break;
    case 0xb8:
      op_clv();
      break;
    case 0xb9:
      // aby
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.Y;
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xba:
      op_tsx();
      break;
    case 0xbb:
      // illegal
      break;
    case 0xbc:
      // abx
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.X;
        uint8_t val = memory.read(addr);
        op_ldy(val);
      }
      break;
    case 0xbd:
      // abx
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.X;
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xbe:
      // aby
      {
        uint16_t addr = memory.read(registers.PC++);
        addr = addr & (memory.read(registers.PC++) << 8);
        addr = addr + registers.Y;
        uint8_t val = memory.read(addr);
        op_ldx(val);
      }
      break;
      break;
    case 0xbf:
      // illegal
      break;
    case 0xc0:
      // immediate
      op_cpy();
      break;
    case 0xc1:
      // izx
      op_cmp();
      break;
    case 0xc2:
      // illegal
      break;
    case 0xc3:
      // illegal
      break;
    case 0xc4:
      // zp
      op_cpy();
      break;
    case 0xc5:
      // zp
      op_cmp();
      break;
    case 0xc6:
      // zp
      op_dec();
      break;
    case 0xc7:
      // illegal
      break;
    case 0xc8:
      op_iny();
      break;
    case 0xc9:
      // immediate
      op_cmp();
      break;
    case 0xca:
      op_dex();
      break;
    case 0xcb:
      // illegal
      break;
    case 0xcc:
      // abs
      op_cpy();
      break;
    case 0xcd:
      // abs
      op_cmp();
      break;
    case 0xce:
      // abs
      op_dec();
      break;
    case 0xcf:
      // illegal
      break;
    case 0xd0:
      // rel
      op_bne();
      break;
    case 0xd1:
      // izy
      op_cmp();
      break;
    case 0xd2:
      // illegal
      break;
    case 0xd3:
      // illegal
      break;
    case 0xd4:
      // illegal
      break;
    case 0xd5:
      // zp,x
      op_cmp();
      break;
    case 0xd6:
      // zp,x
      op_dec();
      break;
    case 0xd7:
      // illegal
      break;
    case 0xd8:
      op_cld();
      break;
    case 0xd9:
      // aby
      op_cmp();
      break;
    case 0xda:
      // illegal
      break;
    case 0xdb:
      // illegal
      break;
    case 0xdc:
      // illegal
      break;
    case 0xdd:
      // abx
      op_cmp();
      break;
    case 0xde:
      // abx
      op_dec();
      break;
    case 0xdf:
      // illegal
      break;
    case 0xe0:
      // immediate
      op_cpx();
      break;
    case 0xe1:
      // izx
      op_sbc();
      break;
    case 0xe2:
      // illegal
      break;
    case 0xe3:
      // illegal
      break;
    case 0xe4:
      // zp
      op_cpx();
      break;
    case 0xe5:
      // zp
      op_sbc();
      break;
    case 0xe6:
      // zp
      op_inc();
      break;
    case 0xe7:
      // illegal
      break;
    case 0xe8:
      op_inx();
      break;
    case 0xe9:
      // immediate
      op_sbc();
      break;
    case 0xea:
      // official nop?
      break;
    case 0xeb:
      // illegal
      break;
    case 0xec:
      // abs
      op_cpx();
      break;
    case 0xed:
      // abs
      op_sbc();
      break;
    case 0xee:
      // abs
      op_inc();
      break;
    case 0xef:
      // illegal
      break;
    case 0xf0:
      // rel
      op_beq();
      break;
    case 0xf1:
      // izy
      op_sbc();
      break;
    case 0xf2:
      // illegal
      break;
    case 0xf3:
      // illegal
      break;
    case 0xf4:
      // illegal
      break;
    case 0xf5:
      // zp,x
      op_sbc();
      break;
    case 0xf6:
      // zp,x
      op_inc();
      break;
    case 0xf7:
      // illegal
      break;
    case 0xf8:
      op_sed();
      break;
    case 0xf9:
      // aby
      op_sbc();
      break;
    case 0xfa:
      // illegal
      break;
    case 0xfb:
      // illegal
      break;
    case 0xfc:
      // illegal
      break;
    case 0xfd:
      // abx
      op_sbc();
      break;
    case 0xfe:
      // abx
      op_inc();
      break;
    case 0xff:
      // illegal
      break;
    default:
      break;
  }
}

void CPU::execute() {
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

void CPU::op_lda(uint8_t val) {
  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;
  registers.A = val;
}
void CPU::op_ldx(uint8_t val) {
  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;
  registers.X = val;
}
void CPU::op_ldy(uint8_t val) {
  if (val == 0)
    registers.P |= Flags::Z;
  else if (val & 0x80)
    registers.P |= Flags::N;
  registers.Y = val;
}

void CPU::op_sta(uint16_t addr) {
  memory.write(addr, registers.A);
}
void CPU::op_stx(uint16_t addr) {
  memory.write(addr, registers.X);
}
void CPU::op_sty(uint16_t addr) {
  memory.write(addr, registers.Y);
}
