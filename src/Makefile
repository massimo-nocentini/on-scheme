

commons:
	csc -dynamic commons.scm -J
	csc -dynamic commons.import.scm -J

promise:
	csc -dynamic promise.scm -J
	csc -dynamic promise.import.scm -J

collatz:
	csc -dynamic collatz.scm -J
	csc -dynamic collatz.import.scm -J
	#mv collatz.so collatz.import.so ../tests/
	#cd ../tests/ && make collatz-test

continuations:
	csc -dynamic continuations.scm -J
	csc -dynamic continuations.import.scm -J
	#mv continuations.so continuations.import.so ../tests/
	#cd ../tests/ && make continuations-test

all: clean commons promise continuations collatz 
	mv *.so ../tests/
	cd ../tests/ && make test-all

clean:
	rm -rf *.so *.import.scm
