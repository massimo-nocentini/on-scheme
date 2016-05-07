
run-continuations-tests: continuations
	csi introduction-to-continuations.scm < introduction-to-continuations-tests.scm

continuations:
	csc -dynamic introduction-to-continuations.scm
