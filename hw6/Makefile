all: example.cmo functors.cmo
	ocamlc -o main functors.cmo example.cmo

example.cmo: functors.cmo example.ml
	ocamlc -c functors.cmo example.ml

functors.cmo: functors.ml
	ocamlc -c functors.ml

test: test_suite
	./test_suite

test_powerset_domain.cmo: functors.cmo test_powerset_domain.ml
	ocamlc -c -I .. unix.cma ../oUnit.cmo functors.cmo test_powerset_domain.ml

test_fun_domain.cmo: functors.cmo example.cmo test_fun_domain.ml
	ocamlc -c -I .. unix.cma ../oUnit.cmo functors.cmo example.cmo test_fun_domain.ml

test_zintvl.cmo: functors.cmo test_zintvl.ml
	ocamlc -c -I .. unix.cma ../oUnit.cmo functors.cmo test_zintvl.ml

test_suite: functors.cmo example.cmo test_powerset_domain.cmo test_fun_domain.cmo test_zintvl.cmo test_suite.ml
	ocamlc -o test_suite -I .. unix.cma ../oUnit.cmo functors.cmo example.cmo test_powerset_domain.cmo test_fun_domain.cmo test_zintvl.cmo test_suite.ml

clean:
	rm -f *.cmo *.cmi test_suite main
