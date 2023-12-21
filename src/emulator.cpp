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

void CPU::test() {
  std::cout
    << static_cast<int>(registers.A)
    << ","
    << static_cast<int>(registers.X)
    << std::endl;
}

void CPU::fetch() {
}

void CPU::decode(uint8_t byte) {
  /*
  int b = (byte & 0x1c) >> 2;
  std::cout << "b of " << static_cast<int>(byte) << " is " << b << std::endl;
  */
  uint8_t instruction = memory.read(registers.PC++);
  switch (instruction) {
    case 0x00:
      break;
    case 0x01:
      break;
    case 0x02:
      break;
    case 0x03:
      break;
    case 0x04:
      break;
    case 0x05:
      break;
    case 0x06:
      break;
    case 0x07:
      break;
    case 0x08:
      break;
    case 0x09:
      break;
    case 0x0a:
      break;
    case 0x0b:
      break;
    case 0x0c:
      break;
    case 0x0d:
      break;
    case 0x0e:
      break;
    case 0x0f:
      break;
    case 0x10:
      break;
    case 0x11:
      break;
    case 0x12:
      break;
    case 0x13:
      break;
    case 0x14:
      break;
    case 0x15:
      break;
    case 0x16:
      break;
    case 0x17:
      break;
    case 0x18:
      break;
    case 0x19:
      break;
    case 0x1a:
      break;
    case 0x1b:
      break;
    case 0x1c:
      break;
    case 0x1d:
      break;
    case 0x1e:
      break;
    case 0x1f:
      break;
    case 0x20:
      break;
    case 0x21:
      break;
    case 0x22:
      break;
    case 0x23:
      break;
    case 0x24:
      break;
    case 0x25:
      break;
    case 0x26:
      break;
    case 0x27:
      break;
    case 0x28:
      break;
    case 0x29:
      break;
    case 0x2a:
      break;
    case 0x2b:
      break;
    case 0x2c:
      break;
    case 0x2d:
      break;
    case 0x2e:
      break;
    case 0x2f:
      break;
    case 0x30:
      break;
    case 0x31:
      break;
    case 0x32:
      break;
    case 0x33:
      break;
    case 0x34:
      break;
    case 0x35:
      break;
    case 0x36:
      break;
    case 0x37:
      break;
    case 0x38:
      break;
    case 0x39:
      break;
    case 0x3a:
      break;
    case 0x3b:
      break;
    case 0x3c:
      break;
    case 0x3d:
      break;
    case 0x3e:
      break;
    case 0x3f:
      break;
    case 0x40:
      break;
    case 0x41:
      break;
    case 0x42:
      break;
    case 0x43:
      break;
    case 0x44:
      break;
    case 0x45:
      break;
    case 0x46:
      break;
    case 0x47:
      break;
    case 0x48:
      break;
    case 0x49:
      break;
    case 0x4a:
      break;
    case 0x4b:
      break;
    case 0x4c:
      break;
    case 0x4d:
      break;
    case 0x4e:
      break;
    case 0x4f:
      break;
    case 0x50:
      break;
    case 0x51:
      break;
    case 0x52:
      break;
    case 0x53:
      break;
    case 0x54:
      break;
    case 0x55:
      break;
    case 0x56:
      break;
    case 0x57:
      break;
    case 0x58:
      break;
    case 0x59:
      break;
    case 0x5a:
      break;
    case 0x5b:
      break;
    case 0x5c:
      break;
    case 0x5d:
      break;
    case 0x5e:
      break;
    case 0x5f:
      break;
    case 0x60:
      break;
    case 0x61:
      break;
    case 0x62:
      break;
    case 0x63:
      break;
    case 0x64:
      break;
    case 0x65:
      break;
    case 0x66:
      break;
    case 0x67:
      break;
    case 0x68:
      break;
    case 0x69:
      break;
    case 0x6a:
      break;
    case 0x6b:
      break;
    case 0x6c:
      break;
    case 0x6d:
      break;
    case 0x6e:
      break;
    case 0x6f:
      break;
    case 0x70:
      break;
    case 0x71:
      break;
    case 0x72:
      break;
    case 0x73:
      break;
    case 0x74:
      break;
    case 0x75:
      break;
    case 0x76:
      break;
    case 0x77:
      break;
    case 0x78:
      break;
    case 0x79:
      break;
    case 0x7a:
      break;
    case 0x7b:
      break;
    case 0x7c:
      break;
    case 0x7d:
      break;
    case 0x7e:
      break;
    case 0x7f:
      break;
    case 0x80:
      break;
    case 0x81:
      // indirect zp,x
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.X;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        op_sta(addr);
      }
      break;
    case 0x82:
      break;
    case 0x83:
      break;
    case 0x84:
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sty(addr);
      }
      break;
    case 0x85:
      {
        uint16_t addr = memory.read(registers.PC++);
        op_sta(addr);
      }
      break;
    case 0x86:
      {
        uint16_t addr = memory.read(registers.PC++);
        op_stx(addr);
      }
      break;
    case 0x87:
      break;
    case 0x88:
      op_dey();
      break;
    case 0x89:
      break;
    case 0x8a:
      op_txa();
      break;
    case 0x8b:
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
      break;
    case 0x90:
      break;
    case 0x91:
      // indirect zp,y
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.Y;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        op_sta(addr);
      }
      break;
    case 0x92:
      break;
    case 0x93:
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
      break;
    case 0x9c:
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
      break;
    case 0x9f:
      break;
    case 0xa0:
      {
        uint8_t immediate = memory.read(registers.PC++);
        op_ldx(immediate);
      }
      break;
    case 0xa1:
      // indirect zp,x
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.X;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xa2:
      {
        uint8_t immediate = memory.read(registers.PC++);
        op_ldx(immediate);
      }
      break;
    case 0xa3:
      break;
    case 0xa4:
      // zero-page ldy
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_ldy(val);
      }
      break;
    case 0xa5:
      // zero-page lda
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xa6:
      // zero-page ldx
      {
        uint16_t addr = memory.read(registers.PC++);
        uint8_t val = memory.read(addr);
        op_ldx(val);
      }
      break;
    case 0xa7:
      break;
    case 0xa8:
      op_tay();
      break;
    case 0xa9:
      {
        uint8_t immediate  = memory.read(registers.PC++);
        op_lda(immediate);
      }
      break;
    case 0xaa:
      op_tax();
      break;
    case 0xab:
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
      break;
    case 0xb0:
      break;
    case 0xb1:
      {
        uint16_t zaddr = memory.read(registers.PC++) + registers.Y;
        uint16_t addr = memory.read(zaddr);       // low byte
        addr = addr & (memory.read(zaddr) << 8);  // high byte
        uint8_t val = memory.read(addr);
        op_lda(val);
      }
      break;
    case 0xb2:
      break;
    case 0xb3:
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
      break;
    case 0xb8:
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
      break;
    case 0xc0:
      break;
    case 0xc1:
      break;
    case 0xc2:
      break;
    case 0xc3:
      break;
    case 0xc4:
      break;
    case 0xc5:
      break;
    case 0xc6:
      break;
    case 0xc7:
      break;
    case 0xc8:
      break;
    case 0xc9:
      break;
    case 0xca:
      op_dex();
      break;
    case 0xcb:
      break;
    case 0xcc:
      break;
    case 0xcd:
      break;
    case 0xce:
      break;
    case 0xcf:
      break;
    case 0xd0:
      break;
    case 0xd1:
      break;
    case 0xd2:
      break;
    case 0xd3:
      break;
    case 0xd4:
      break;
    case 0xd5:
      break;
    case 0xd6:
      break;
    case 0xd7:
      break;
    case 0xd8:
      break;
    case 0xd9:
      break;
    case 0xda:
      break;
    case 0xdb:
      break;
    case 0xdc:
      break;
    case 0xdd:
      break;
    case 0xde:
      break;
    case 0xdf:
      break;
    case 0xe0:
      break;
    case 0xe1:
      break;
    case 0xe2:
      break;
    case 0xe3:
      break;
    case 0xe4:
      break;
    case 0xe5:
      break;
    case 0xe6:
      break;
    case 0xe7:
      break;
    case 0xe8:
      break;
    case 0xe9:
      break;
    case 0xea:
      break;
    case 0xeb:
      break;
    case 0xec:
      break;
    case 0xed:
      break;
    case 0xee:
      break;
    case 0xef:
      break;
    case 0xf0:
      break;
    case 0xf1:
      break;
    case 0xf2:
      break;
    case 0xf3:
      break;
    case 0xf4:
      break;
    case 0xf5:
      break;
    case 0xf6:
      break;
    case 0xf7:
      break;
    case 0xf8:
      break;
    case 0xf9:
      break;
    case 0xfa:
      break;
    case 0xfb:
      break;
    case 0xfc:
      break;
    case 0xfd:
      break;
    case 0xfe:
      break;
    case 0xff:
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
