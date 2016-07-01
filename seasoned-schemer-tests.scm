

(use continuations test) 

    (test-group "LETCC syntactic abstraction"
     ; simple forms with `letcc`
     (test 1 (letcc hop 1))
     (test 1 (letcc hop (hop 1)))
     (test 1 (letcc hop (add1 (hop 1))))
     (test 3 (+ 2 (letcc hop (add1 (hop 1))))))

    (test-group "TRY syntactic abstraction"

     (let ((identity-applicative&return (lambda (x skip) x))
           (identity-applicative&skip (lambda (x skip) (skip 'discard))) 
           (identity-cps&return (lambda (x) (lambda (skip) x)))
           (identity-cps&skip (lambda (x) (lambda (skip) (skip 'discard)))))

      (test 1 (try-applicative 
               ((skip (identity-applicative&return 1 skip))
                (else (eternity)))))

      (test 2 (try-applicative 
               ((skip (identity-applicative&skip 1 skip))
                (else 2))))

      (test 1 (try-cps 
               ((identity-cps&return 1) => identity)
               (else (eternity))))

      (test 2 (try-cps 
               ((identity-cps&skip 1) => eternity)
               (else 2)))

    (test 11 (letcc cont (+ 10 (try-cps 
                                ((identity-cps&return 1) => identity)
                                (else cont in (lambda (k) 2) => eternity)))))

    (test 12 (letcc cont (+ 10 (try-cps 
                                ((identity-cps&skip 1) => eternity)
                                (else cont in (lambda (k) 2) => identity)))))

    (test 2 (letcc cont (+ 10 (try-cps 
                               ((identity-cps&skip 1) => eternity)
                               (else cont in (lambda (k) (k 2)) => eternity)))))

    )) ; end of "TRY" tests group



    ;; IMPORTANT! The following ensures nightly automated tests can
    ;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)



