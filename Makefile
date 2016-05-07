
run-continuations-tests: continuations
	./introduction-to-continuations-tests

continuations:
	csc -s introduction-to-continuations.scm
	csc -s introduction-to-continuations-tests.scm
	csc introduction-to-continuations-tests.scm introduction-to-continuations.so 

alone:
	csc -s introduction-to-continuations.scm
