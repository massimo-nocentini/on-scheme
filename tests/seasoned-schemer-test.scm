
 (import scheme (chicken base))
 (import test)
 (import seasoned-schemer continuations)

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

    (test-group "intersect+all"

     (test-group "intersect" ; first tests `intersect` which is a little dependency
      (test "intersect a set with an empty set" '() (intersect '(1 2 3) '()))
      (test "intersect two non-empty sets" '(2 3) (intersect '(1 2 3) '(2 3 4)))) 

     (test "non empty intersection" '(3) (intersect+all 
                                          '((3 mango and) (3 kiwis and) (3 hamburgers))))

     (test "empty intersection, empty set present" '() (intersect+all '((3 steaks and) 
                                                                        (no food and) 
                                                                        ()
                                                                        (3 diet hamburgers))))

     (test "empty intersection, empty set present in last position" 
      '() (intersect+all '((3 steaks and) (no food and) (3 diet hamburgers) ())))

     (test "empty intersection" '() (intersect+all '((3 steaks and) 
                                                     (no food and) 
                                                     (three baked potatoes)
                                                     (3 diet hamburgers))))
    )


    (test-group "comb-upto-last"

      (test "rember-upto-last, three atom occurrences" 
       '(e f) (comb-upto-last 'a -1 '(a b a d a e f)))
      (test "rember-upto-last, no atom occurrences" 
       '(a b c d a e f) (comb-upto-last 'r -1 '(a b c d a e f)))
      (test "rember-upto-last, empty prefix when atom is in `car` position" 
       '() (comb-upto-last 'a 1 '(a b c d a e f a g h)))
      (test "rember-upto-last, sublist between the start and first atom occurences" 
       '(b c d) (comb-upto-last 'a 1 '(b c d a e f a g h)))
      (test "rember-upto-last, sublist between first and second atom occurrences" 
       '(e f) (comb-upto-last 'a 2 '(b c d a e f a g h)))
      (test "rember-upto-last, suffix as done by `rember-upto-last`" 
       '(g h) (comb-upto-last 'a 3 '(b c d a e f a g h)))
      (test "rember-upto-last, suffix as done by `rember-upto-last`, again" 
       '(g h) (comb-upto-last 'a 50 '(b c d a e f a g h)))


    )

    (test-group "leftmost"

     (let ((leftmost leftmost/escape))
      (test "leftmost, there is at least one atom" 
       'a (leftmost '(((a)) b (c))))

      (test "leftmost, no atom in the very first car sexp" 
       'b (leftmost '((() ((() (())))) b (c))))

      (let ((sexp (quote ((() ((() (())))) (((() ()))) ()))))
       (test "leftmost, no atom at all" 
        sexp (leftmost sexp)))
     )
    )

    (test-group "rember1*"

    (let ((R rember1*/try))

      (test "rember1*, atom present" 
       '((delicious) (food))(R 'more '((delicious) more (food))))

      (test "rember1*, atom not present" 
       '((orange) more (fruits)) (R 'pizza '((orange) more (fruits))))

     )
    )

(test-exit)
