
PKGS = cmdliner,ounit

all: Repl.native
Repl.native: src/*.re
	rebuild -pkgs $(PKGS) -I src/ Repl.native
