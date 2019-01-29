
(module unionfind *

    (import scheme (chicken base))
    (import srfi-1 srfi-69)
    (import commons)

    (define-record unionfind π rank)

    (define-record-printer unionfind
     (lambda (U out)
      (for-each ; no need to produce output
       (lambda (e)
        (let-values (((a d) (car+cdr e)))
         (display `(,a -> ,d) out)
         (newline out)))
       (unionfind->alist U))))

    (define unionfind-walk
     (lambda (U recv res)
      (hash-table-walk (unionfind-π U) recv)
      (res)))

    (define unionfind-walk/without-loops
     (lambda (U recv res)
      (unionfind-walk U
       (lambda (k v)
        (unless (equal? k v) (recv k v)))
       res)))

    (define unionfind-empty
     (lambda ()
      (make-unionfind (make-hash-table) (make-hash-table))))

    (define unionfind-keys (○ hash-table-keys unionfind-π))

    (define unionfind-size (○ hash-table-size unionfind-π))

    (define unionfind-edges
     (lambda (U)
      (let ((n 0))
       (unionfind-walk/without-loops U
        (lambda (k v) (set! n (add1 n)))
        (lambda () n)))))

    (define unionfind-copy
     (lambda (U)
      (make-unionfind
       (hash-table-copy (unionfind-π U))
       (hash-table-copy (unionfind-rank U)))))

    (define unionfind-↑
     (lambda (U)
      (letrec ((π (unionfind-π U))
               (↑ (lambda (x)
                   (cond
                    ((hash-table-exists? π x) (let ((y (hash-table-ref π x)))
                                               (if (equal? x y) x (↑ y))))
                    (else x)))))
       ↑)))

    (define unionfind-↑!
     (lambda (U)
      (letrec ((π (unionfind-π U))
               (↑ (lambda (x)
                   (cond
                    ((hash-table-exists? π x) (let ((y (hash-table-ref π x)))
                                               (unless (equal? x y) (hash-table-set! π x (↑ y)))
                                               (hash-table-ref π x)))
                    (else x)))))
       ↑)))

    (define unionfind-★
     (lambda (U)
      (lambda (y)
       (hash-table-ref (unionfind-rank U) y))))

    (define unionfind-≡
     (lambda (U)
      (let* ((π       (unionfind-π U))
             (rank    (unionfind-rank U))
             (in?     (lambda (x) (hash-table-exists? π x)))
             (insert! (lambda (x)
                       (hash-table-set! π x x)
                       (hash-table-set! rank x 0)))
             (↑       (unionfind-↑ U))
             (★       (unionfind-★ U))
             (≡ (lambda (x y) ; the return is undefined
                 (unless (in? x) (insert! x))
                 (unless (in? y) (insert! y))
                 (let ((root-x (↑ x))
                       (root-y (↑ y)))
                  (unless (equal? root-x root-y)
                   (cond
                    ((> (★ root-x) (★ root-y)) (hash-table-set! π root-y root-x))
                    (else (begin
                           (hash-table-set! π root-x root-y)
                           (when (equal? (★ root-x) (★ root-y))
                            (hash-table-update! rank root-y add1))))))))))
       ≡)))

    (define unionfind-new
     (lambda (recv)
      (let ((U (unionfind-empty))
            (R (curry₁ recv)))
       (unionfind-accessors U (R U)))))
       ;(unionfind-accessors U (lambda (↑ ↑! ≡ →)
               ;                        (recv U ↑ ↑! ≡ →))))))

    (define unionfind-accessors
     (lambda (U recv)
      (let ((↑     (unionfind-↑ U))
            (↑!    (unionfind-↑! U))
            (≡     (unionfind-≡ U))
            (→     (lambda () (unionfind->alist U))))
       (recv ↑ ↑! ≡ →))))

    (define unionfind->alist
     (lambda (U)
      (let* ((→ (lambda(x) (hash-table-ref (unionfind-π U) x)))
             (C (lambda (k) (cons k (→ k)))))
       (map C (unionfind-keys U)))))

    #;(define unionfind->alist
     (lambda (U)
      (let* ((→ (lambda(x) (hash-table-ref (unionfind-π U) x)))
             (C (lambda (k) (cons k (→ k)))))
       ((compose (ffilter (lambda (p) (not (equal? (car p) (cdr p))))) (fmap C)) (unionfind-keys U)))))

    (define unionfind->→+★
     (lambda (U)
      (let* ((★ (unionfind-★ U))
             (C (lambda (k)
                 (let-values (((a d) (car+cdr k)))
                  (cons (cons a (★ a)) (list (cons d (★ d))))))))
       (map C (unionfind->alist U)))))

)
