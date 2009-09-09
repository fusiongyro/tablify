all: tablify.bz2

tablify.bz2: tablify
	bzip2 -fk tablify

tablify: CSV.hs HTML.hs TBL.hs Tablify.hs Unicode.hs Utilities.hs
	ghc --make tablify

reallyclean: clean
	rm -f tablify tablify.bz2

clean:
	rm -f *.hi *.o

.PHONY: clean reallyclean