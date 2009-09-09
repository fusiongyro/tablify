all:
	ghc --make tablify

clean:
	rm *.hi *.o tablify

.PHONY: clean