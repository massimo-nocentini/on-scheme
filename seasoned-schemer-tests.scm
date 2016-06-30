
;(declare (uses continuations))
(load "continuations.so")
(require-extension test)

;; Simple test
(test 4 (+ 2 2))

;; group
(test-group "A group"
  (test "A test with description" 5 (+ 2 3))
    (test-assert "This should always be true" (string? "foo")))

;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
