

current: commons streams unionfind microkanren deploy-shared-libs
	cd ../tests/ && make microkanren-test

current1: commons streams series deploy-shared-libs
	cd ../tests/ && make streams-test

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

streams:
	csc -dynamic streams.scm -J
	csc -dynamic streams.import.scm -J

series:
	csc -dynamic series.scm -J
	csc -dynamic series.import.scm -J

microkanren:
	csc -dynamic microkanren.scm -J
	csc -dynamic microkanren.import.scm -J

all: clean commons promise continuations collatz seasoned-schemer unionfind streams series microkanren
	# pass

deploy-shared-libs:
	mv *.so ../tests/

clean:
	rm -rf *.so *.import.scm

tests: all deploy-shared-libs
	cd ../tests/ && make run-tests

