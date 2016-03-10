all: ArrayTests

MyArray: MyArray.hs
	ghc -Wall --make MyArray

ArrayTests: ArrayTests.hs
	ghc -Wall --make ArrayTests

clean:
	rm -f *.o *.hi ArrayTests
