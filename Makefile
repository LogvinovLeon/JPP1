all: Main

Main: Main.hs
	ghc -Wall --make Main -O2

clean:
	rm -f *.o *.hi ArrayTests Main
