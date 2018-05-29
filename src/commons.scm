

(module commons *

 (import chicken scheme)
 
 (use srfi-1 srfi-69) ; `use` only for `srfi`s

 (use data-structures ports)

 (define curry₁
  (lambda (f)
   (lambda (g)
    (lambda args
     (apply f (cons g args))))))

 (define fmap    (curry₁ map))
 (define fapply  (curry₁ apply))
 (define ffilter (curry₁ filter))

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

)
