CC=ghc
CFLAGS=-Wall -O2

tetris: tetris.hs display.hs
	ghc $(CFLAGS) -o tetris tetris.hs display.hs
	
comment:
	haddock tetris.hs display.hs

clear:
	rm -f tetris.exe tetris tetris.hi tetris.o display.hi display.o rotate.hi rotate.o types.hi types.o