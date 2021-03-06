

 (import scheme (chicken base) (chicken port))
 (import datatype srfi-69 test)
 (import commons promise) ; for my own `use`s

 (test-group "subscripting symbols and variables introduction via `fresh`"

  (test 'g₁₂₃ (symbol∼subscripts 'g123))
  (test "g₁" (with-output-to-string (lambda () (fresh₁ display)))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`
  (test "g₂" (with-output-to-string (lambda () (fresh₁ display)))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`

  (test "(g₃ g₄ g₅)" (with-output-to-string (lambda () (fresh (v w z) (display (list v w z)))))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`

  (let ((one (V (gensym)))
        (two (V 'hello))
        (three (V 'hello)))
   (test #f (equal? one two))
   (test #f (equal? one three))
   (test #t (equal? two three)))

 )

 (test-group "`delay` and `make-promise` syntactic and functional abstractions, respectively"

  (let ((count (let ((counter 0))
                (lambda ()
                 (set! counter (add1 counter))
                 counter))))

   (define delayed_count (delay (count)))
   (test 1 (force delayed_count))
   (test 1 (force delayed_count))
   (test 1 (force delayed_count))
   (test 1 (force delayed_count))
   (test 1 (force delayed_count))

   (define promised_count (make-promise (count)))
   (test 2 (force promised_count))
   (test 2 (force promised_count))

   (define promised_ccount (make-promise (count)))
   (test 2 (force promised_count))
   (test 3 (force promised_ccount))

   (test 4 (count)))

    )

(test-exit)
