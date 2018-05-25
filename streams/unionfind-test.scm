
(import chicken scheme)
(use test unionfind ports)

    (define member? 
     (lambda args
      (cond 
       ((apply member args) #t)
       (else #f))))

(let ((H (make-hash-table)))
 (hash-table-set! H 'hello 'world)
 (test #t (hash-table-exists? H 'hello)))

(let ((H (make-hash-table)))
 (hash-table-set! H 'hello 'world)
 (hash-table-set! H 'hello 'new-world)
 (test 'new-world (hash-table-ref H 'hello)))

    (test 'fail (if #f 'succeed 'fail))
    (test 'succeed (if '() 'succeed 'fail))
    (test 'succeed (if `(,#f ,#f) 'succeed 'fail))
    (test 'else (cond
                 ((and #t #f) => (lambda (y) #t))
                 (else 'else)))

    (with-unionfind
     (lambda (U ↑ ↑! ≡ →)
      (test 'absent (↑ 'absent))
      (test '(a b) (↑ '(a b))) ; this example doesn't work in Python because lists are *not* hashable objects
      (test 0 (length (→)))
      (≡ 3 4)
      (test 4 (↑ 3))
      (test 4 (↑ 4))
      (let ((edges (→)))
       (test '(3 . 4) (assoc 3 edges))
       (test '(4 . 4) (assoc 4 edges)))
      ;(test "(4 -> 4)\n(3 -> 4)\n" (with-output-to-string (lambda () (display U))))
      ))

    (with-unionfind
     (lambda (U ↑ ↑! ≡ →)
      (test 0 (length (→)))
      (≡ 'A 'D)
      (≡ 'B 'E)
      (≡ 'C 'F)
      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((A . 0) (D . 1)) edges)
                 (member? '((B . 0) (E . 1)) edges)
                 (member? '((C . 0) (F . 1)) edges)
                 (member? '((D . 1) (D . 1)) edges)
                 (member? '((E . 1) (E . 1)) edges)
                 (member? '((F . 1) (F . 1)) edges))))
      (≡ 'C 'G)
      (≡ 'E 'A)
      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((A . 0) (D . 2)) edges)
                 (member? '((E . 1) (D . 2)) edges)
                 (member? '((B . 0) (E . 1)) edges)
                 (member? '((C . 0) (F . 1)) edges)
                 (member? '((G . 0) (F . 1)) edges)
                 (member? '((D . 2) (D . 2)) edges)
                 (member? '((F . 1) (F . 1)) edges))))
      (≡ 'B 'G)
      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((A . 0) (D . 2)) edges)
                 (member? '((E . 1) (D . 2)) edges)
                 (member? '((B . 0) (E . 1)) edges)
                 (member? '((C . 0) (F . 1)) edges)
                 (member? '((G . 0) (F . 1)) edges)
                 (member? '((D . 2) (D . 2)) edges)
                 (member? '((F . 1) (D . 2)) edges))))
     ))

    (with-unionfind
     (lambda (U ↑ ↑! ≡ →)
      (≡ 'B 'A)
      (≡ 'D 'C)
      (≡ 'C 'A)
      (≡ 'I 'F)
      (≡ 'J 'F)
      (≡ 'K 'G)
      (≡ 'H 'E)
      (≡ 'F 'E)
      (≡ 'G 'E)
      (≡ 'E 'A)
      
      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((B . 0) (A . 3)) edges)
                 (member? '((C . 1) (A . 3)) edges)
                 (member? '((D . 0) (C . 1)) edges)
                 (member? '((E . 2) (A . 3)) edges)
                 (member? '((F . 1) (E . 2)) edges)
                 (member? '((G . 1) (E . 2)) edges)
                 (member? '((I . 0) (F . 1)) edges)
                 (member? '((J . 0) (F . 1)) edges)
                 (member? '((K . 0) (G . 1)) edges)
                 (member? '((H . 0) (E . 2)) edges))))

      (test 'A (↑! 'I))

      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((B . 0) (A . 3)) edges)
                 (member? '((C . 1) (A . 3)) edges)
                 (member? '((D . 0) (C . 1)) edges)
                 (member? '((E . 2) (A . 3)) edges)
                 (member? '((F . 1) (A . 3)) edges)
                 (member? '((G . 1) (E . 2)) edges)
                 (member? '((I . 0) (A . 3)) edges)
                 (member? '((J . 0) (F . 1)) edges)
                 (member? '((K . 0) (G . 1)) edges)
                 (member? '((H . 0) (E . 2)) edges))))

      (test 'A (↑! 'K))

      (let ((edges (unionfind->→+★ U)))
       (test #t (and 
                 (member? '((B . 0) (A . 3)) edges)
                 (member? '((C . 1) (A . 3)) edges)
                 (member? '((D . 0) (C . 1)) edges)
                 (member? '((E . 2) (A . 3)) edges)
                 (member? '((F . 1) (A . 3)) edges)
                 (member? '((G . 1) (A . 3)) edges)
                 (member? '((I . 0) (A . 3)) edges)
                 (member? '((J . 0) (F . 1)) edges)
                 (member? '((K . 0) (A . 3)) edges)
                 (member? '((H . 0) (E . 2)) edges))))

     ))
