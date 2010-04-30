all: example.cmo functors.cmo
	ocamlc -o main functors.cmo example.cmo

example.cmo: functors.cmo example.ml
	ocamlc -c functors.cmo example.ml

functors.cmo: functors.ml
	ocamlc -c functors.ml

