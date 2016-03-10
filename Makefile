all: Main

Main: Main.hs
	ghc -Wall --make Main

clean:
	rm -f *.o *.hi ArrayTests Main
