
run-continuations-tests: continuations
	csi utils.scm continuations.scm < introduction-to-continuations-tests.scm

continuations: # both object code and dynamic library
	csc -dynamic continuations.scm
	#csc -c continuations.scm

seasoned-schemer-tests: #continuations
	csc seasoned-schemer-tests.scm
	./seasoned-schemer-tests
