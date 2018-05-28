
commons:
	csc -dynamic commons.scm -J
	csc -dynamic commons.import.scm -J

promise:
	csc -dynamic promise.scm -J
	csc -dynamic promise.import.scm -J

all: clean commons promise
	mv *.so ../tests/

clean:
	rm -rf *.so *.import.scm
