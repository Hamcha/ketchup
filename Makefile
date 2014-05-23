all: example

example:
	ghc example.hs

clean:
	rm -rf ./example
	rm -rf *.o */*.o *.hi */*.hi