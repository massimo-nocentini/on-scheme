
(include "continuations.scm") ; inclusion is necessary to have macros at compile-time

(import continuations) ; where our "continuations stuff" lie

(require-extension test)


    (test-group "LETCC syntactic abstraction"

     (test 1 (letcc hop 1))
     (test 1 (letcc hop (hop 1)))
     (test 1 (letcc hop (add1 (hop 1))))
     (test 3 (+ 2 (letcc hop (add1 (hop 1))))))


    (test-group "TRY syntactic abstraction"

     (let ((id-dont-skip-applicative (lambda (x skip) x))
           (id-do-skip-applicative (lambda (x skip) (skip 'discard))) 
           (id-dont-skip-cps (lambda (x) (lambda (skip) x)))
           (id-do-skip-cps (lambda (x) (lambda (skip) (skip 'discard)))))

      (test 1 (try-applicative 
               ((skip (id-dont-skip-applicative 1 skip))
                (else 2))))

      (test 2 (try-applicative 
               ((skip (id-do-skip-applicative 1 skip))
                (else 2))))


      (test 1 (try-cps 
               ((id-dont-skip-cps 1) => identity)
               (else 2)))

      (test 2 (try-cps 
               ((id-do-skip-cps 1) => identity)
               (else 2)))

    (test 11 (letcc cont (+ 10 (try-cps 
                                ((id-dont-skip-cps 1) => identity)
                                (else cont in (lambda (k) 2) => identity)))))

    (test 12 (letcc cont (+ 10 (try-cps 
                                ((id-do-skip-cps 1) => identity)
                                (else cont in (lambda (k) 2) => identity)))))

    (test 2 (letcc cont (+ 10 (try-cps 
                               ((id-do-skip-cps 1) => identity)
                               (else cont in (lambda (k) (k 2)) => identity)))))

    )) ; end of "TRY" tests group



    ;; IMPORTANT! The following ensures nightly automated tests can
    ;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
