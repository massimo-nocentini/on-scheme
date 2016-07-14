
;(load "macros.so")
(import macros)

    (define-syntax len+
     (syntax-rules ()
      ((len+ () n) (add1 n))
      ((len+ (<sexp> . ss) n) (len+ ss (add1 n)))))

    (define-syntax car-syn
     (syntax-rules ()
      ((_ lat) (car lat))))

    (define x (lambda args (len+ '(1 2 3) 0)))

    (define y (lambda args (car-syn '(1 2 3))))

    (define z (lambda args 
               (if (cdr '(1 2 3))
                '(2)
                (display "this message never take place, I suspect"))))
    
    (t pippo)

    ;(length-syn '(1 2 3))

    (display (x 'uhkairs))
    (display (y 'uhkairs))
    (display (z 'uhkairs))
    (display pippo)


