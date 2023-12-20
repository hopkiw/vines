#include <cstdint>
#include <iostream>

#include "./cpu.h"

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
