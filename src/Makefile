

current4: commons continuations SECD-machine deploy-shared-libs
	cd ../tests/ && make SECD-machine-test

current2: commons streams unionfind microkanren reasoned-schemer deploy-shared-libs
	cd ../tests/ && make microkanren-test


current5: commons SC-machine deploy-shared-libs
	cd ../tests/ && make SC-machine-test

current: commons continuations deploy-shared-libs
	cd ../tests/ && make continuations-test

current3: commons streams series dice-of-doom deploy-shared-libs
	cd ../tests/ && make dice-of-doom-test


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

reasoned-schemer:
	csc -dynamic reasoned-schemer.scm -J
	csc -dynamic reasoned-schemer.import.scm -J

dice-of-doom:
	csc -dynamic dice-of-doom.scm -J
	csc -dynamic dice-of-doom.import.scm -J

SC-machine:
	csc -dynamic SC-machine.scm -J
	csc -dynamic SC-machine.import.scm -J

SECD-machine:
	csc -dynamic SECD-machine.scm -J
	csc -dynamic SECD-machine.import.scm -J

#all: clean commons promise continuations collatz seasoned-schemer unionfind streams series microkanren reasoned-schemer dice-of-doom SC-machine SECD-machine
all: clean commons promise continuations collatz seasoned-schemer unionfind streams series microkanren reasoned-schemer SC-machine SECD-machine
	# pass

deploy-shared-libs:
	mv *.so ../tests/

clean:
	rm -rf *.so *.import.scm

tests: all deploy-shared-libs
	cd ../tests/ && make run-tests

