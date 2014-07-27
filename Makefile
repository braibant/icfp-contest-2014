
OCAMLBUILD=ocamlbuild -no-hygiene

.PHONY: force

all: force tiles.ml
	$(OCAMLBUILD) -use-ocamlfind simulator.byte
	$(OCAMLBUILD) -use-ocamlfind main.byte
	$(OCAMLBUILD) -use-ocamlfind ghc_trace.byte

display_test: force tiles.ml
	$(OCAMLBUILD) -tag debug display_test.byte

ghc_test: force
	$(OCAMLBUILD) -tag debug ghc_test.byte

tiles.ml: tiles.xpm make-tiles.sh
	./make-tiles.sh

.PHONY: clean
clean:
	rm -f *.cm[ioxa] *.cmxa
	rm -f *.annot
	rm -rf _build
	rm -f simulator.byte display_test.byte
