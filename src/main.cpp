#include <stdexcept>
#include <string.h>

#include <ios>
#include <iomanip>
#include <iostream>
#include <fstream>

#include "./cpu.h"

int main() {
  /*
  CPU cpu(Registers{22, 32, 0});
  cpu.memory.write(0x4020, 0xA9);  // lda, imm
  cpu.memory.write(0x4021, 0x0C);
  cpu.memory.write(0x4022, 0x8D);  // sta, abs
  cpu.memory.write(0x4023, 0xff);
  cpu.memory.write(0x4024, 0x3f);
  cpu.registers.PC = 0x4020;
  */

  std::ifstream fh;
  fh.open("nestest.nes", std::fstream::binary);
  if (fh.fail()) {
    std::cout << "Error: nestest.nes: " << strerror(errno) << std::endl;
    return 1;
  }
  fh.seekg(0x10);

  CPU cpu(Registers{});
  fh.read(reinterpret_cast<char*>(&cpu.cartridge.rom), 0x4000);

  cpu.registers.PC = 0xc000;
  int ret = 0, count = 0;
  while (1) {
    std::cout << cpu;

    try {
      cpu.execute();
    }
    catch (std::runtime_error& e) {
      std::cout << "Error: " << e.what() << std::endl;
      ret = 1;
      break;
    }
    // std::cout << "CPU state: " << cpu << "\n" << std::endl;
    count++;
    std::cout << std::endl;
  }

  std::cout << "Ran " << std::dec << count << " instructions" << std::endl;
  std::cout << "Memory 0x02" << (int)cpu.memory.read(02) << std::endl;
  std::cout << "Memory 0x03" << (int)cpu.memory.read(03) << std::endl;
  return ret;
}
