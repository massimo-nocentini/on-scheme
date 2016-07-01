
(use seasoned-schemer continuations test)

    (test-group "multi-insert*&co"

     (let ((sexp '((((orange fish)) apple anchovy) apple (orange)))
           (sexp-expected '((((orange tuna fish)) apple tuna anchovy) apple (orange tuna)))
           (identity-abridged (lambda args args)))

      (test 
       "should be tail-recursive by Chicken Scheme" 
       `(,sexp-expected 1 2) 
       (multi-insert*&co 'tuna 'anchovy 'orange sexp identity-abridged))

      (test 
       "use `letcc` to return adruptly in the collector if no TCO is performed"
       `(,sexp-expected 1 2) 
       (letcc hop (multi-insert*&co 'tuna 'anchovy 'orange sexp (lambda args (hop args)))))
     )
    )

    (test-group "two-in-a-row?"
     (test "natural unwind of recursion stack, empty list" #f (two-in-a-row? '()))
     (test "natural unwind of recursion stack, no two equal peers in a row" #f (two-in-a-row? '(j f r e k s i)))
     (test "natural unwind of recursion stack, two equal peers in a row" #t (two-in-a-row? '(j f r e k s s)))
     (test "discard recursion stack hopping with letcc, empty list" #f (two-in-a-row?&hop '()))
     (test "discard recursion stack hopping with letcc, no two equal peers in a row" #f (two-in-a-row?&hop '(j f r e k s i)))
     (test "discard recursion stack hopping with letcc, two equal peers in a row" #t (two-in-a-row?&hop '(j f r e k s s)))
    )


(test-exit)
