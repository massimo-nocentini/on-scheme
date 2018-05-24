
(module unionfind *

    (import chicken scheme)

    (use srfi-69)

    (define-record unionfind π rank)

    (define unionfind-init
     (lambda ()
      (make-unionfind (make-hash-table) (make-hash-table))))

    (define unionfind-ref
     (lambda (U)
      (letrec ((π (unionfind-π U))
               (R (lambda (x) 
                    (cond
                     ((hash-table-exists? π x) (let ((y (hash-table-ref π x))) (if (equal? x y) x (R y))))
                     (else x)))))
       R)))

    (define unionfind-≡
     (lambda (U)
      (letrec ((π       (unionfind-π U))
               (rank    (unionfind-rank U))
               (ε       (unionfind-ref U))
               (in?     (lambda (x) (hash-table-exists? π x)))
               (insert! (lambda (x) 
                         (hash-table-set! π x x) 
                         (hash-table-set! rank x 0)))
               (≡ (lambda (x y)
                   (unless (in? x) (insert! x))
                   (unless (in? y) (insert! y))
                   (let ((root-x (ε x))
                         (root-y (ε y)))
                    (unless (equal? root-x root-y) 
                     (cond
                      ((> (hash-table-ref rank root-x) (hash-table-ref rank root-y)) 
                       (hash-table-update! π root-y (lambda (old) root-x)))
                      (else (begin
                             (hash-table-update! π root-x (lambda (old) root-y))
                             (when (equal? (hash-table-ref rank root-x) (hash-table-ref rank root-y))
                              (hash-table-update! rank root-y add1))))))))))
       ≡)))

    (define with-unionfind 
     (lambda (recv)
      (let* ((U (unionfind-init))
             (↑ (unionfind-ref U))
             (≡ (unionfind-≡ U)))
       (recv ↑ ≡))))
) 

