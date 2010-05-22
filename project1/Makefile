OBJS=lexer.cmo parser.cmo k.cmo domain.cmo analyzer.cmo main.cmo
NATIVE_OBJS=lexer.cmx parser.cmx domain.cmx k.cmx analyzer.cmx main.cmx

all: $(OBJS)
	ocamlc -o main $(OBJS)

native: $(NATIVE_OBJS)
	ocamlopt -o main.native $(NATIVE_OBJS)

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml main main.native *.cmo *.o .depend *.annot

parser.cmo: k.cmo parser.cmi

parser.cmx: k.cmx parser.cmi

lexer.cmo: parser.cmo

lexer.cmx: parser.cmx

.SUFFIXES: .ml .mli .cmo .cmx .cmi .mly .mll

.mli.cmi:
	ocamlc -dtypes -c $<

.ml.cmx:
	ocamlopt -dtypes -c $<

.ml.cmo:
	ocamlc -c -g $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

.mly.mli:
	ocamlyacc $<

.depend:
	ocamldep *.mli *.ml > .depend

.PHONY: clean

include .depend
