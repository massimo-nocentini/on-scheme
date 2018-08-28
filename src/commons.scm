

(module commons *

 (import chicken scheme)

 (use srfi-1 srfi-69) ; `use` only for `srfi`s

 (use numbers data-structures ports test lolevel)

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
 (define fsort
  (lambda (key ⊂)
   (lambda (s)
    (sort s (lambda (p q) (⊂ (key p) (key q)))))))
 (define flist-ref (curry₁ list-ref))
 (define fvector-ref (curry₁ vector-ref))
 (define equals-to? (curry₁ equal?))

    (define $  ; Haskell-like suspended application
     (lambda args
      (lambda functions
       (let ((apps (map (lambda (f) (apply f args)) functions)))
        (apply values apps)))))

    (define identity*
     (lambda args
      (cond
       ((one? (length args)) (car args))
       (else args))))

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

    (define to-string
     (lambda (v)
      (with-output-to-string (lambda () (display v)))))

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

    (define map/tree
     (lambda (f)
      (letrec ((T (lambda (sexp)
                   (cond
                    ((null? sexp) '())
                    ((pair? (car sexp)) (cons (T (car sexp)) (T (cdr sexp))))
                    (else (cons (f (car sexp)) (T (cdr sexp))))))))
       T)))

    (define collect-values
     (lambda (thunk)
      (call-with-values thunk identity*)))

    (define accumulator
     (lambda (f s)
      (let ((acc s))
       (lambda (x)
        (set! acc (f x acc))
        acc))))

    (define sub2 (○ sub1 sub1))

    (define ≠
     (lambda (v w)
      (not (equal? v w))))

    (define one?
     (lambda (n)
      (equal? n 1)))

    (define ⁻¹
      (lambda (x) (/ 1 x)))

    (define ²
      (lambda (x) (* x x)))

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

    (define group
     (lambda (key post)
      (lambda (l)
       (let* ((H (make-hash-table))
              (U (lambda (l₀)
                  (let ((k (key l₀)))
                   (cond
                    ((hash-table-exists? H k) (hash-table-set! H k (cons l₀ (hash-table-ref H k))))
                    (else                     (hash-table-set! H k (list l₀))))))))
        (for-each U l)
        (map post (hash-table->alist H))))))

    (define tuple/pred?
     (lambda (pred?)
      (letrec ((P (lambda tuples
                   (cond
                    ((apply pred? (map car tuples))
                     (let ((cdrs (map cdr tuples)))
                      (cond
                       ((every null? cdrs) #t)
                       ((every (○ not null?) cdrs) (apply P cdrs))
                       (else #f))))
                    (else #f)))))
      P)))

    (define K
     (lambda keep
      (lambda discard
       (apply values keep))))

    (define remove-duplicates
     (lambda (lst)
      (letrec ((R (lambda (lst set)
                   (cond
                    ((null? lst) set)
                    (else (let-values (((a d) (car+cdr lst)))
                           (cond
                            ((member a d) (R d set))
                            (else (R d (cons a set))))))))))
       (R lst '()))))

    (define memoize
     (lambda (f #!key (H (make-hash-table test: equal?)))
      (let ((↑ (hash-table-ref/store H)))
        (mutate-procedure! f (lambda (f)
                              (lambda args
                               (↑ args f)))))))

    (define-syntax define-tabled
     (syntax-rules (lambda)
      ((define-tabled name (lambda (args ...) body ...))
       (define name
        (let ((H (make-hash-table test: equal?)))
         (letrec ((name (lambda (args ...)
                        (let ((k `(,args ...)))
                         (let-values (((found v) (hash-table-ref/maybe H k)))
                          (unless found
                           (set! v (begin body ...))
                           (hash-table-set! H k v))
                          v)))))
         name))))))

    (define-syntax letrec-tabled
     (syntax-rules (lambda)
      ((letrec-tabled ((name (lambda (args ...) λ-body ...)) ...) body ...)
       (let () ; to limit the scope of tabled definitions; 
               ; on the contrary, `begin` doesn't limit their scope.
        (define-tabled name (lambda (args ...) λ-body ...)) ... 
        (begin body ...)))))

    (define hash-table-ref/store
     (lambda (H)
      (lambda (key missing)
       (cond
        ((hash-table-exists? H key) (hash-table-ref H key))
        (else (let ((v (apply missing key)))
               ((K v) (hash-table-set! H key v))))))))

    (define hash-table-ref/maybe
     (lambda (H key)
      (cond
       ((hash-table-exists? H key) (values #t (hash-table-ref H key)))
       (else (values #f (##core#undefined))))))

)
