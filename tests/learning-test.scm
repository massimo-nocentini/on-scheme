

(import chicken scheme)

(use test matchable)

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
    )

(test-exit)
