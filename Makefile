all:
	ocamlbuild -use-ocamlfind simulator.byte
	ocamlbuild -use-ocamlfind main.byte

display_test: tiles.ml
	ocamlbuild -tag debug display_test.byte

tiles.ml: tiles.xpm make-tiles.sh
	./make-tiles.sh

.PHONY: clean
clean:
	rm -f *.cm[ioxa] *.cmxa
	rm -f *.annot
	rm -rf _build
	rm -f simulator.byte display_test.byte
