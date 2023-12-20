all: main

main.o: src/main.cpp include/cpu.h
	g++ -g -c -Iinclude -o main.o src/main.cpp

emulator.o: src/emulator.cpp include/cpu.h
	g++ -g -c -Iinclude -o emulator.o src/emulator.cpp

opcodes.o: src/opcodes.cpp include/cpu.h
	g++ -g -c -Iinclude -o opcodes.o src/opcodes.cpp

main: main.o emulator.o opcodes.o
	g++ -g -o main main.o emulator.o opcodes.o

.PHONY: clean
clean:
	rm -f *.o main
