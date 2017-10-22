C_FLAGS=-x c -std=c99 -Wall -Wextra -pedantic
C_FLAGS_AFTER=-lm

.PHONY: all
all: minesweeper hello showoff

.PHONY: minesweeper
minesweeper: minesweeper/main.c
	gcc $(C_FLAGS) minesweeper/main.c -o minesweeper/minesweeper $(C_FLAGS_AFTER)

.PHONY: hello
hello: hello/main.c
	gcc $(C_FLAGS) hello/main.c -o hello/hello $(C_FLAGS_AFTER)

.PHONY: showoff
showoff: showoff/main.c
	gcc $(C_FLAGS) showoff/main.c -o showoff/showoff $(C_FLAGS_AFTER)

