
run-continuations-tests: continuations
	csi utils.scm continuations.scm < introduction-to-continuations-tests.scm

continuations: # both object code and dynamic library
	csc -dynamic continuations.scm
	csc -c continuations.scm

seasoned-schemer-tests: continuations
	csc -c seasoned-schemer-tests.scm
	csc seasoned-schemer-tests.o -o seasoned-schemer-tests
	./seasoned-schemer-tests
