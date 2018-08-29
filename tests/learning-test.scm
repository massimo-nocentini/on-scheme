
(import chicken scheme)

(use srfi-1 srfi-13)
(use test matchable numbers)

(use commons)

    (test-group "DELAY-FORCE"

     (test-assert (promise? (delay (+ 3 4))))
     (test-assert (promise? (delay-force (+ 3 4))))

     (letrec ((stream-filter/tailcall (lambda (p? s)
                                       (delay-force
                                        (let ((s-mature (force s)))
                                         (if (null? s-mature)
                                          (delay '())
                                          (let-values (((h t) (car+cdr s-mature)))
                                           (if (p? h)
                                            (delay (cons h (stream-filter/tailcall p? t)))
                                            (stream-filter/tailcall p? t))))))))
              (stream-filter/stackfull (lambda (p? s) ; very inefficient version that uses unbounded memory because of (delay (force ...))
                                        (delay
                                         (force
                                          (let ((s-mature (force s)))
                                           (if (null? s-mature)
                                            (delay '())
                                            (let-values (((h t) (car+cdr s-mature)))
                                             (if (p? h)
                                              (delay (cons h (stream-filter/stackfull p? t)))
                                              (stream-filter/stackfull p? t)))))))))
              (from  (lambda (n)
                      (delay-force (cons n (from (+ n 1))))))
     (large-number 10000))
     (test large-number (car (force (stream-filter/tailcall
                                     (lambda (n) (= n large-number))
                                     (from 0))))))

    (test-assert (procedure? (force (λ () 3))))
    (test '3 (force (make-promise 3)))

    )

    (test-group "BOOLEANS"

     (test 'fail (if #f 'succeed 'fail))
     (test 'succeed (if '() 'succeed 'fail))
     (test 'succeed (if `(,#f ,#f) 'succeed 'fail))
     (test 'else (cond
                  ((and #t #f) => (lambda (y) #t))
                  (else 'else)))

    )

    (test-group "HASH TABLE"

     (let ((H (make-hash-table)))
      (hash-table-set! H 'hello 'world)
      (test #t (hash-table-exists? H 'hello)))

     (let ((H (make-hash-table)))
      (hash-table-set! H 'hello 'world)
      (hash-table-set! H 'hello 'new-world)
      (test 'new-world (hash-table-ref H 'hello)))

    )

    (test-group "MATCHABLE"

     (test "multiple matches"
      10 (match '(1 2 2 2 2 2 3)
          ((1 x ... 3) (apply + x))))

     (test "multiple matches, literal"
      2 (match '(1 1 1 1 1 2 3)
          ((1 ... x 3) x)))

     (test "match inside a string"
      8 (match '(1 2 3)
          ((1 x 3) (* x x x))))

     (test "declarative"
      8 (let ((x 3))
          (match '(1 3 2 3)
           ((1 x y 3) (* y y y)))))

     (test "declarative SHOULD RISE AN ERROR"
      8 (let ((x 4))
          (match '(1 3 2 3)
           ((1 x y 3) (* y y y)))))

     (test "matching with a quote in pattern"
      'a-match (match '(1 x 3)
          ((1 'x 3) 'a-match)))

     (test "matching a quoted symbol"
      'x (match '(1 x 3)
          ((1 y 3) y)))
    )

    (test-group "OUTPUT PORTS"

     (let* ((str-port (open-output-string))
            (result (with-output-to-port str-port
                     (lambda ()
                      (display "hello world")
                      "succeed"))))
      (test "hello world" (get-output-string str-port))
      (test "succeed" result))

     (call+stdout
      (lambda ()
       (display "hello world")  ; to be redirected into a collecting string
       '(hello world))          ; to be used as return value
      (lambda (r s)
       (test-assert (and (equal? '(hello world) r) (equal? "hello world" s)))))


     (test "succeed" (with-output-to-string (lambda ()
                                             (display 'succeed)
                                             #t))) ; `with-output-to-string` discards the return value

     (test "hello-world" (call-with-output-string (lambda (port)
                                                   (display 'hello-world port)
                                                   #t))) ; `call-with-output-string` discards the return value

    )

    (test-group "MAPPING"

     (test '(hello world) (call-with-values (λ () (values 'hello 'world)) identity*))
     (test 'succeed (values 'succeed))
     (test 'fail (values 'fail 'succeed))
     (test '(1 2 3) (map (lambda (i) (values (add1 i) i)) '(0 1 2))) ; this produces a warning: "expected a single result in argument #1 of procedure call `(cons (g2272 (##sys#slot g2278 0)) (quote ()))', but received 2 results"
     (test '(1 2 3)
      ((map/call-with-values
        (lambda (i) (values (add1 i) i))
        (lambda (more less) more))
       '(0 1 2)))
     (test '((1 1) (3 3))
      ((map/values (lambda (p) (values (add1 (car p)) (cadr p))))
       '((0 1) (2 3))))

     (test #t ((tuple/pred? <) '(1 2) '(2 3) '(3 4)))
     (test #f ((tuple/pred? <) '(1 5) '(2 3) '(3 4)))
     (test #f ((tuple/pred? <) '(1 5) '(2) '(3 4)))
     (test '(a b c d e) (remove-duplicates (reverse '(a b c d e))))
     (test '(e c d a b) (remove-duplicates '(a b a a c d c e e)))
    )

    (test-group "TABLING"

     (define F
      (lambda (i)
       (cond
        ((< i 2) i)
        (else (+ (F (- i 1)) (F (- i 2)))))))

     (test 0 (F 0))
     (test 1 (F 1))
     (test '(0 1 1 2 3 5 8 13 21) (map F (iota 9)))
     (time (test 1346269 (F 31))) ; 0.574s CPU time, 0.007s GC time (major), 17/6 mutations (total/tracked), 14/5606 GCs (major/minor), maximum live heap: 377.59 KiB

     (define-tabled F₀
      (lambda (i)
       (cond
        ((zero? i) 0)
        ((one? i) 1)
        (else (+ (F₀ (- i 1)) (F₀ (- i 2)))))))

     (test 0 (F₀ 0))
     (test 1 (F₀ 1))
     (test '(0 1 1 2 3 5 8 13 21) (map F₀ (iota 9)))
     (time (test 1346269 (F₀ 31))) ; 0s CPU time, 40/29 mutations (total/tracked), maximum live heap: 381.21 KiB

    (define fibonacci
     (letrec ((F (lambda (i)
                  (cond
                   ((< i 2) i)
                   (else (+ (F (- i 1)) (F (- i 2))))))))
      (memoize F)))

    (time (test 354224848179261915075 (fibonacci 100))) ; 0.001s CPU time, 117/106 mutations (total/tracked), 0/1 GCs (major/minor), maximum live heap: 391.37 KiB

    (define F₁
     (memoize (lambda (i)
               (cond
                ((< i 2) i)
                (else (+ (F₁ (- i 1)) (F₁ (- i 2))))))))

    (time (test 1346269 (F₁ 31))) ; 0.574s CPU time, 0.011s GC time (major), 17/6 mutations (total/tracked), 15/5605 GCs (major/minor), maximum live heap: 392.62 KiB

    (define-tabled pascal
     (lambda (n k)
      (cond
       ((and (zero? n) (zero? k)) 1)
       ((zero? n) 0)
       ((zero? k) (pascal (sub1 n) 0))
       (else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))))))

    (test 1 (pascal 0 0))
    (test 2 (pascal 2 1))
    (test 1 (pascal 2 2))
    (test 3 (pascal 3 2))
    (test 100891344545564193334812497256 (pascal 100 50))

    (define-tabled catalan
     (lambda (n k)
      (cond
       ((and (zero? n) (zero? k)) 1)
       ((zero? n) 0)
       ((zero? k) (apply + (map (lambda (j) (catalan (sub1 n) j)) (iota n))))
       (else (apply + (map (lambda (j) (catalan (sub1 n) j)) (iota n (sub1 k))))))))

    (define Riordan-array
     (lambda (recurrence)
      (lambda (m)
       (map (lambda (n)
             (append-map (lambda (k) (list (recurrence n k)))
              (iota (add1 n))))
        (iota m)))))

    (test '((1)
            (1 1)
            (1 2 1)
            (1 3 3 1)
            (1 4 6 4 1)
            (1 5 10 10 5 1)
            (1 6 15 20 15 6 1)
            (1 7 21 35 35 21 7 1)
            (1 8 28 56 70 56 28 8 1)
            (1 9 36 84 126 126 84 36 9 1)
            (1 10 45 120 210 252 210 120 45 10 1)
            (1 11 55 165 330 462 462 330 165 55 11 1)
            (1 12 66 220 495 792 924 792 495 220 66 12 1)
            (1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1)
            (1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1)
            (1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1)
            (1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1)
            (1 17 136 680 2380 6188 12376 19448 24310 24310 19448 12376 6188 2380 680 136 17 1) (1 18 153 816 3060 8568 18564 31824 43758 48620 43758 31824 18564 8568 3060 816 153 18 1) (1 19 171 969 3876 11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876 969 171 19 1))
     ((Riordan-array pascal) 20))

    (test '((1)
            (1 1)
            (2 2 1)
            (5 5 3 1)
            (14 14 9 4 1)
            (42 42 28 14 5 1)
            (132 132 90 48 20 6 1)
            (429 429 297 165 75 27 7 1)
            (1430 1430 1001 572 275 110 35 8 1)
            (4862 4862 3432 2002 1001 429 154 44 9 1)
            (16796 16796 11934 7072 3640 1638 637 208 54 10 1)
            (58786 58786 41990 25194 13260 6188 2548 910 273 65 11 1)
            (208012 208012 149226 90440 48450 23256 9996 3808 1260 350 77 12 1)
            (742900 742900 534888 326876 177650 87210 38760 15504 5508 1700 440 90 13 1)
            (2674440 2674440 1931540 1188640 653752 326876 149226 62016 23256 7752 2244 544 104 14 1)
            (9694845 9694845 7020405 4345965 2414425 1225785 572033 245157 95931 33915 10659 2907 663 119 15 1)
            (35357670 35357670 25662825 15967980 8947575 4601610 2187185 961400 389367 144210 48279 14364 3705 798 135 16 1)
            (129644790 129644790 94287120 58929450 33266625 17298645 8351070 3749460 1562275 600875 211508 67298 19019 4655 950 152 17 1)
            (477638700 477638700 347993910 218349120 124062000 65132550 31865925 14567280 6216210 2466750 904475 303600 92092 24794 5775 1120 170 18 1)
            (1767263190 1767263190 1289624490 811985790 463991880 245642760 121580760 56448210 24582285 10015005 3798795 1332045 427570 123970 31878 7084 1309 189 19 1))
     ((Riordan-array catalan) 20))

    (define-tabled ackermann
     (lambda (m n)
      (cond
       ((zero? m) (add1 n))
       ((zero? n) (ackermann (sub1 m) 1))
       (else (ackermann (sub1 m) (ackermann m (sub1 n)))))))

    (test 7 (ackermann 2 2))
    (test 125 (ackermann 3 4))

    )

(test-exit)
