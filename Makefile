all: tablify

tablify: CSV.hs HTML.hs TBL.hs Tablify.hs Unicode.hs Utilities.hs
	ghc --make tablify

clean:
	rm *.hi *.o tablify

.PHONY: clean