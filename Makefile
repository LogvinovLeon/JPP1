all: ArrayTests

ArrayTests: ArrayTests.hs MyArray.hs
	ghc -Wall -Werror --make ArrayTests

clean:
	rm -f *.o *.hi
