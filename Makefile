
run-continuations-tests: continuations
	csi utils.scm continuations.scm < introduction-to-continuations-tests.scm

continuations:
	csc -dynamic -unit continuations continuations.scm
	#csc -c continuations.scm
