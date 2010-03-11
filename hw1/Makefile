TESTS = test_ex1.ml test_ex2.ml test_ex3.ml test_ex4.ml test_ex5.ml test_ex6.ml test_ex7.ml
SRCS = ex1.ml ex2.ml ex3.ml ex4.ml ex5.ml ex6.ml ex7.ml

test: test_suite
	./test_suite

test_suite: $(SRCS) $(TESTS) test_suite.ml
	ocamlc -o test_suite -I .. unix.cma ../oUnit.cmo $(SRCS) $(TESTS) test_suite.ml

clean:
	rm -f ex?.cm* test_ex?.cm* test_suite.cm* test_suite