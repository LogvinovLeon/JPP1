all: MyArray

MyArray: MyArray.hs
	ghc -Wall --make MyArray

clean:
	rm -f *.o *.hi
