all: value.cmx memory.cmx lexer.cmx parser.cmx k.cmx main.cmx
	ocamlopt -o main.native value.cmx memory.cmx lexer.cmx parser.cmx k.cmx main.cmx

value.cmx: value.ml
	ocamlopt -c value.ml

k.cmx: memory.cmx k.ml
	ocamlopt -c k.ml

main.cmx: k.cmx main.ml
	ocamlopt -c main.ml

memory.cmx: value.cmx memory.ml
	ocamlopt -c memory.ml

parser.cmx: parser.ml parser.cmi
	ocamlopt -c parser.ml

parser.cmi: parser.mli
	ocamlopt -c parser.mli

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.ml: parser.mly k.cmx
	ocamlyacc parser.mly

lexer.cmx: lexer.ml
	ocamlopt -c lexer.ml

lexer.ml: lexer.mll parser.cmx
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml main.native *.o

debug: value.cmo memory.cmo lexer.cmo parser.cmo k.cmo main.cmo
	ocamlc -g -o main.debug value.cmo memory.cmo lexer.cmo parser.cmo k.cmo main.cmo

value.cmo: value.ml
	ocamlc -g -c value.ml

memory.cmo: value.cmo memory.ml
	ocamlc -g -c value.cmo memory.ml

lexer.cmo: lexer.ml
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml
	ocamlc -g -c parser.ml

k.cmo: value.cmo memory.cmo k.ml
	ocamlc -g -c value.cmo memory.cmo k.ml

main.cmo: k.cmo main.ml
	ocamlc -g -c k.cmo main.ml