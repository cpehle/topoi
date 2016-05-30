
all: Repl.native

PKGS = react,cmdliner,ounit

Repl.native: src/*.re
	rebuild -pkgs $(PKGS) -I src/ Repl.native
