
(module unionfind *

    (import chicken scheme)

    (use srfi-1 srfi-69 data-structures)

    (define-record unionfind π rank)

    (define-record-printer unionfind
     (lambda (U out)
      (for-each ; no need to produce output
       (lambda (e)
        (let-values (((a d) (car+cdr e)))
         (display `(,a -> ,d) out)
         (newline out)))
       (unionfind->alist U))))

    (define unionfind-init
     (lambda ()
      (make-unionfind (make-hash-table) (make-hash-table))))

    (define unionfind-keys (compose hash-table-keys unionfind-π))

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

    (define with-unionfind
     (lambda (recv)
      (let* ((U     (unionfind-init))
             (↑     (unionfind-↑ U))
             (↑!    (unionfind-↑! U))
             (≡     (unionfind-≡ U))
             (→     (lambda () (unionfind->alist U))))
       (recv U ↑ ↑! ≡ →))))

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
