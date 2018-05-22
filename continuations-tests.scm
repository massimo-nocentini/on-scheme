

(use continuations test) 

    (test-group "LETCC syntactic abstraction"
     ; simple forms with `letcc`
     (test 1 (letcc hop 1))
     (test 1 (letcc hop (hop 1)))
     (test 1 (letcc hop (add1 (hop 1))))
     (test 10 (+ 9 (letcc hop (add1 (hop 1))))))

    (test-group "TRY syntactic abstraction"

     (let ((identity-applicative&return (lambda (x skip) x))
           (identity-applicative&skip (lambda (x skip) (skip 'discard))))

      (test 1 (try
               (skip (identity-applicative&return 1 skip))
               (else (eternity))))

      (test 2 (try
               (skip (identity-applicative&skip 1 skip))
               (else 2)))

      (test '(1) 
       (try 
        (skip1 (list 1)) 
        (skip2 'useless) 
        (else 'even-more-useless)))

      (test '(2) 
       (try 
        (skip1 (list 1 (skip1 'discard)))
        (skip2 (list 2))
        (else 'useless)))

      (test '(3) 
       (try 
        (skip1 (list 1 (skip1 'discard)))
        (skip2 (list 2 (skip2 'discard)))
        (else (list 3))))


    )) ; end of "TRY" tests group



    ;; IMPORTANT! The following ensures nightly automated tests can
    ;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)



