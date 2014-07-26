all: tiles.ml
	ocamlbuild -use-ocamlfind simulator.byte
	ocamlbuild -use-ocamlfind main.byte

tiles.ml: tiles.xpm make-tiles.sh
	./make-tiles.sh
