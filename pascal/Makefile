.PHONY: all
all: hello showoff

.PHONY: hello
hello: hello/main.pas
	fpc $(C_FLAGS) hello/main.pas -ohello

.PHONY: showoff
showoff: showoff/main.pas
	fpc $(C_FLAGS) showoff/main.pas -oshowoff

