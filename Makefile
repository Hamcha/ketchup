all: base example

base:
	cabal configure
	cabal build

example:
	ghc example.hs

install:
	cabal install

clean:
	rm -rf ./example
	rm -rf *.o */*.o *.hi */*.hi
	cabal clean
