.PHONY: all
all: hello

.PHONY: hello
hello: hello/main.asm
	nasm -f elf64 hello/main.asm && ld -e _start -o hello/hello hello/main.o

