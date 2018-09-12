

(import chicken scheme)

(use test)
(use commons continuations)


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

      (test "I got here\nThis string was passed to the continuation.\n"
       (with-output-to-string
        (τ
         (display
          (letcc cont
           (display "I got here\n")
           (cont "This string was passed to the continuation.\n")
           (display "...but not here"))))))

(test-error (let ((start #f))
             (unless start
              (set/cc! start))
             (display "Going to invoke `(start)`\n")
             (start)))

    (test 3 (cond/cc
             (number? add1)
             (else (lambda (cont) (+ 2 (cont 3))))))

    (test 5
     (let ((start #f))
      (letcc (cont λ)
       (unless start (set! start λ))
       (cond
        ((continuation? cont) (λ 3))
        ((number? cont) (add1 cont))
        (else (error "e")))
       (start) (λ 7)) 5))

    #;(let ((v 4)
          (cc (current-continuation)))
     (unless (continuation? cc)
      (test v cc))
     (cond
      ((continuation? cc) (test v (continuation-return cc v)))
      (else (test v cc))))

; The following prints (4 3 5)
    (test '((3 4 5) (4 3 5))
     (amb (lambda (ε ? ✗ ✓)
           (let ((a (ε (list 1 2 3 4 5 6 7)))
                 (b (ε (list 1 2 3 4 5 6 7)))
                 (c (ε (list 1 2 3 4 5 6 7))))

            ; We're looking for dimensions of a legal right
            ; triangle using the Pythagorean theorem:
            (? (= (* c c) (+ (* a a) (* b b))))

            ;(display (list a b c))
            ;(newline)

            ; And, we want the second side to be the shorter one:
            ;(? (< b a))

            ; Print out the answer:
            ;(display (list a b c))
            ;(newline)
            (✓ (list a b c))

            ; retry with any other solution
            (✗)))))

    (test (list (list #t) (list #f))
     (sat-solve (x)
      (or #t x)))

    (test '((#t #t) (#t #f))
     (sat-solve (x y)
      (or x y)))

    (test (list (list #f #f #t))
     (sat-solve (a b c)
      (and (implies a (not b)) (not a) c)))

    (test (list (list #f #t #t #t) (list #f #t #t #t))
     (sat-solve (x1 x3 x4 x5)
      (and 
       (or x1 (not x5) x4) 
       (or (not x1) x5 x3 x4))))



      ;; IMPORTANT! The following ensures nightly automated tests can
    ;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)



