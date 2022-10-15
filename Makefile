.PHONY : build aboba nameless

build:
	stack build --docker


# Generate compiler frontend (parsers and lexers)
generate: src/Lang/ErrM.hs src/Lang/Lex.hs src/Lang/Par.hs src/Lang/Print.hs
	rm src/Lang/Lex.x
	rm src/Lang/Par.y
	rm src/Lang/Par.info
	-rm src/Lang/*.bak
	mv src/Lang/Doc.txt languages/Lang.doc.txt

src/Lang/ErrM.hs src/Lang/Lex.x src/Lang/Print.hs src/Lang/Par.y : languages/Lang.cf
	bnfc --haskell -d -o src languages/Lang.cf
	rm src/Lang/Skel.hs
	rm src/Lang/Test.hs


%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<
