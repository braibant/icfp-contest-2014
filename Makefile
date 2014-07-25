all: tiles.ml
	ocamlbuild simulator.byte -I Ghc

tiles.ml: tiles.xpm make-tiles.sh
	./make-tiles.sh
