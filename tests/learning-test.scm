

(import chicken scheme)

(use test matchable) 

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
      (test "succeed" result)
      (test "succeed" (with-output-to-string (lambda () (display 'succeed)))))

    )

(test-exit)
