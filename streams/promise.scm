
(module learning-promises *

 (import chicken scheme)

 (use datatype data-structures srfi-69 test ports)

 (define curry₁
  (lambda (f)
   (lambda (g)
    (lambda args
     (apply f (cons g args))))))

 (define fmap    (curry₁ map))
 (define fapply  (curry₁ apply))

 (define subscripts
  (let ((H (make-hash-table)))
   (hash-table-set! H #\0 "₀")
   (hash-table-set! H #\1 "₁")
   (hash-table-set! H #\2 "₂")
   (hash-table-set! H #\3 "₃")
   (hash-table-set! H #\4 "₄")
   (hash-table-set! H #\5 "₅")
   (hash-table-set! H #\6 "₆")
   (hash-table-set! H #\7 "₇")
   (hash-table-set! H #\8 "₈")
   (hash-table-set! H #\9 "₉")
   H))

    (define symbol∼
     (lambda (H)
      (let* ((M (lambda (n) (hash-table-ref/default H n (string n))))
             (-> (compose string->list symbol->string))
             (<- (compose string->symbol (fapply string-append)))
             (<-> (compose <- (fmap M) ->)))
       <->)))

    (define symbol∼subscripts (symbol∼ subscripts))

    (define-datatype variable (V (s symbol?)))

    (define-record-printer variable
     (lambda (v out)
      (cases variable v
       (V (s) ((compose display symbol∼subscripts) s)))))

    (define fresh₁ ; functional abstraction for introducing _distinct_ `variable` objects
     (lambda (recv)
      (recv (V (gensym)))))

    (define-syntax fresh ; syntactic sugar on top of `fresh₁` to allow arbitrary arity
     (syntax-rules ()
      ((fresh (v) body ...) (fresh₁ (lambda (v) body ...)))
      ((fresh (v w ...) body ...) (fresh₁ (lambda (v) (fresh (w ...) body ...))))))

 
    (test-group "subscripting symbols and variables introduction via `fresh`"

     (test 'g₁₂₃ (symbol∼subscripts 'g123))
     (test "g₀" (with-output-to-string (lambda () (display (fresh₁ identity))))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`
     (test "g₁" (with-output-to-string (lambda () (display (fresh₁ identity))))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`

     (test "(g₂ g₃ g₄)" (with-output-to-string (lambda () (fresh (v w z) (display (list v w z)))))) ; a very short hand for `(fresh1 (lambda (v) (display v)))`

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

    )
