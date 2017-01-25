
run-seasoned-schemer-tests: continuations
	csc -dynamic seasoned-schemer.scm -j seasoned-schemer
	csc -dynamic seasoned-schemer.import.scm
	csc seasoned-schemer-tests.scm -o seasoned-schemer-tests
	./seasoned-schemer-tests

run-intro-continuations-tests:
	csi utils.scm continuations.scm < introduction-to-continuations-tests.scm

continuations: # both object code and dynamic library
	# the following commands comes from here: http://wiki.call-cc.org/man/4/Modules#examples-of-using-modules
	csc -dynamic continuations.scm -j continuations # `continuations.so` will contain *procedural abstractions* as a library 
	csc -dynamic continuations.import.scm # `continuations.import.so` will contain *syntactic abstractions* as an import library, independently.
	# in this manner, eval `(use continuations)` will load both procedural and syntactic abstractions

run-continuations-tests: continuations
	csc continuations-tests.scm
	./continuations-tests

run-collatz-tests:
	# this rule collapses both definitions and tests
	csc collatz.scm -o collatz-tests
	./collatz-tests


run-all-tests: run-collatz-tests run-continuations-tests run-seasoned-schemer-tests
