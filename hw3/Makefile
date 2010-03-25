TESTS = test_ex3.ml
SRCS = ex3.ml

test: test_suite
	./test_suite

test_suite: $(SRCS) $(TESTS) test_suite.ml
	ocamlc -o test_suite -I .. unix.cma ../oUnit.cmo $(SRCS) $(TESTS) test_suite.ml

clean:
	rm -f ex?.cm* test_ex?.cm* test_suite.cm* test_suite
