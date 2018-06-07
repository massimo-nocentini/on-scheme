
promise-test:
	csc promise-test.scm
	mv promise-test executables/
	./executables/promise-test

collatz-test:
	csc collatz-test.scm
	mv collatz-test executables/
	./executables/collatz-test

continuations-test:
	csc continuations-test.scm
	mv continuations-test executables/
	./executables/continuations-test

learning-test:
	csc learning-test.scm
	mv learning-test executables/
	./executables/learning-test

ackermann-test:
	csc ackermann-test.scm
	mv ackermann-test executables/
	./executables/ackermann-test

seasoned-schemer-test:
	csc seasoned-schemer-test.scm
	mv seasoned-schemer-test executables/
	./executables/seasoned-schemer-test

unionfind-test:
	csc unionfind-test.scm
	mv unionfind-test executables/
	./executables/unionfind-test

streams-test:
	csc -X numbers-syntax streams-test.scm # according to http://wiki.call-cc.org/eggref/4/numbers#compiled-code
	mv streams-test executables/
	./executables/streams-test

run-tests: promise-test collatz-test continuations-test learning-test ackermann-test seasoned-schemer-test unionfind-test streams-test
