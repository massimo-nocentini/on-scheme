

(module commons *

 (import chicken scheme)

 (use srfi-1 srfi-69) ; `use` only for `srfi`s

 (use numbers data-structures ports test)

 (define-syntax λ ; "little-lambda", or "lambda the ultimate", the usual functional abstraction
  (syntax-rules ()
   ((λ args body ...)
    (lambda args body ...))))

 (define ○ compose)

 (define curry₁
  (lambda (f)
   (lambda (g)
    (lambda args
     (apply f (cons g args))))))

 (define fmap    (curry₁ map))
 (define fapply  (curry₁ apply))
 (define ffilter (curry₁ filter))

    (define $  ; Haskell-like suspended application
     (lambda args
      (lambda functions
       (let ((apps (map (lambda (f) (apply f args)) functions)))
        (apply values apps)))))

 (define identity* (lambda args args))

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

     (define eternity
      (lambda args
       (apply eternity args)))

    (define-syntax dest+match/car+cdr
     (syntax-rules (else)
      ((dbind/car+cdr sexp ((a d) pair-sexp) ... (else null-sexp))
       (match sexp
        (() null-sexp)
        ((a . d) pair-sexp) ...))))

    (define call+stdout
     (lambda (thunk recv)
      (let* ((str-port (open-output-string))
             (result (with-output-to-port str-port thunk)))
       (recv result (get-output-string str-port)))))

    (define member?
     (lambda args
      (cond
       ((apply member args) #t)
       (else #f))))

    (define map/with-index
     (lambda (f s)
      (lambda (lst)
       (letrec ((M (lambda (l n)
                    (cond
                     ((null? l) '())
                     (else (cons
                            ((f n) (car l))
                            (M (cdr l) (add1 n))))))))
        (M lst s)))))

    (define map/call-with-values
     (lambda (producer consumer)
      (fmap (lambda (a)
             (call-with-values (λ () (producer a)) consumer)))))

    (define map/values
     (lambda (producer)
      (map/call-with-values producer identity*)))

    (define collect-values
     (lambda (thunk)
      (call-with-values thunk identity*)))

    (define accumulator
     (lambda (f s)
      (let ((acc s))
       (lambda (x)
        (set! acc (f x acc))
        acc))))


    (define ⁻¹
      (lambda (x) (/ 1 x)))

    (define display-on-port
     (lambda (port)
      (lambda (v)
       (display v port))))

    (define-syntax test-fail
     (syntax-rules ()
      ((test-fail sexp) (test-assert (not sexp)))))

    (define number->symbol (○ string->symbol number->string))

    (define within?
     (lambda (m M)
      (lambda (n)
       (<= m n M))))

)
