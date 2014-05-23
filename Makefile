all: example

example:
	ghc example.hs

clean:
	rm *.o */*.o *.hi */*.hi