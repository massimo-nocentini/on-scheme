

commons:
	csc -dynamic commons.scm -J
	csc -dynamic commons.import.scm -J

promise:
	csc -dynamic promise.scm -J
	csc -dynamic promise.import.scm -J

collatz:
	csc -dynamic collatz.scm -J
	csc -dynamic collatz.import.scm -J

continuations:
	csc -dynamic continuations.scm -J
	csc -dynamic continuations.import.scm -J

seasoned-schemer:
	csc -dynamic seasoned-schemer.scm -J
	csc -dynamic seasoned-schemer.import.scm -J

unionfind:
	csc -dynamic unionfind.scm -J
	csc -dynamic unionfind.import.scm -J

all: clean commons promise continuations collatz seasoned-schemer unionfind
	mv *.so ../tests/
	cd ../tests/ && make test-all

clean:
	rm -rf *.so *.import.scm
