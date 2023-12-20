#include <iostream>

#include "./cpu.h"

int main() {
  CPU cpu(Registers{22, 32, 0});
  std::cout << "Hello, world: " << &cpu << std::endl;
  cpu.test();
  cpu.op_tax();
  cpu.test();
  return 0;
}
