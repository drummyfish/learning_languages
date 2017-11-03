C_FLAGS=-x c++ -std=c++11 -Wall -Wextra -pedantic
C_FLAGS_AFTER=-lm

.PHONY: all
all: hello showoff minesweeper

.PHONY: hello
hello: hello/main.cpp
	c++ $(C_FLAGS) hello/main.cpp -o hello/hello $(C_FLAGS_AFTER)

.PHONY: minesweeper
hello: minesweeper/main.cpp
	c++ $(C_FLAGS) minesweeper/minesweeper.cpp -o minesweeper/minesweeper $(C_FLAGS_AFTER)

.PHONY: showoff
showoff: showoff/main.cpp
	c++ $(C_FLAGS) showoff/main.cpp -o showoff/showoff $(C_FLAGS_AFTER)

