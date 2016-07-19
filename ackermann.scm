
; implementation of a macro for the original Ackermann function φ.

(use matchable)

    (define-syntax phi
     (syntax-rules ()
      ((_ m n 0) (+ m n))
      ((_ m 0 1) 0)
      ((_ m 0 2) 1)
      ((_ m 0 p) m)
      ((_ m n p) (phi m (phi m (sub1 n) p) (sub1 p)))))
    
    (define-syntax A
     (syntax-rules ()
      ((_ () ns) (a . ns))
      ((_ (m . ms) ()) (A ms (a)))
      ((_ (m . ms) (n . ns)) (A ms (A (m . ms) ns)))))

    ;(phi 1 1 1)
    
    ;(A (a a) (a a a))

    #;(let ((sexp '(A (a a) (a a a))))
     (printf "~A is the ackermann value of ~A"
      (let T ((sexp sexp))
       (let ((expanded (expand sexp)))
        (printf "~A is the expansion of ~A~N" expanded sexp)
        (cond 
         ((equal? sexp expanded) sexp)
         (else (match (cdr expanded) 
                ((A-macro (m . ms) '()) (T `(,A-macro ,ms (a))))
                ((A-macro ms ns)
                 (let* ((inner (cons 'A ns))
                        (inner-expanded (expand inner)))
                  ;(printf "~A is the inner expansion of ~A~N" inner-expanded inner)
                  (cons (car expanded) (T inner)))))))))
      sexp))

(newline)
(newline)

    ;(let ((sexp '(A 1 2)))
            (define ackermann-expander
             (lambda (sexp tabs sender)
              (printf "~A ~A → " sender sexp)
              (match sexp
               (('A 0 n)
                (let ((arith `(add1 ,n)))
                 (printf "~A ⇒ " `(eval ',arith))
                 (eval arith)))
               (('A m 0)
                (let ((forward `(A ,(sub1 m) 1)))
                 (printf "■ ~A~N~A" forward tabs)
                 (ackermann-expander forward tabs "■"))) 
               (('A m n)
                (let* ((inner `(A ,m ,(sub1 n)))
                      (expanded-inner (ackermann-expander inner (string-append " " tabs) "○"))
                      (whole-unexpanded `(A ,(sub1 m) ,inner))
                      (whole `(A ,(sub1 m) ,expanded-inner)))
                 (printf "● ~A ≡ ~A~N~A" whole-unexpanded whole tabs)
                 (ackermann-expander whole (string-append "" tabs) "●"))))))

 


