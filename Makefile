
run-continuations-tests: continuations
	./introduction-to-continuations-tests

continuations:
	csc -c introduction-to-continuations.scm
	csc -c introduction-to-continuations-tests.scm
	csc introduction-to-continuations-tests.scm introduction-to-continuations.o 

