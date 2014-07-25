PACKAGES=
# I don't understand warning 18
OPTIONS=-tag annot -no-sanitize -tag debug -use-ocamlfind -cflags -w,+a-4-9-18-41-44-40-45-42 -cflags -warn-error,+5+10+8+12+20+11 -tag strict_sequence -cflag -bin-annot -j 8
#OPTIONS += -cflags -warn-error,+a
DIRECTORIES=miniml .
OCAMLBUILD=@ocamlbuild \
		 $(addprefix -package ,$(PACKAGES)) \
		 $(OPTIONS)	\
		 $(addprefix -I ,$(DIRECTORIES)) \

.PHONY: tests tests.native tests_debug force

all: miniml.native .merlin

miniml.native: force
	$(OCAMLBUILD) miniml/miniml.native

.merlin: Makefile
	@rm -f .merlin.tmp
	@for PKG in $(PACKAGES); do echo PKG $$PKG >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo S $$SRC >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo B _build/$$SRC >> .merlin.tmp; done
	@mv .merlin.tmp .merlin
