
(declare (uses introduction-continuations))

(require-extension test)

(test-group "about contexts"

 (let ((sexp '(+ 3 (* 4 (+ 5 6))))
       (sub-sexp '(+ 5 6))
       (context (lambda (▢ ) (+ 3 (* 4 ▢ ))))) 
  (test "context of '(+ 5 6) in '(+ 3 (* 4 (+ 5 6)))"
   47 (context (eval sub-sexp))))

 (let ((sexp '(* (+ (* 3 4) 5) 2))
       (sub-sexp '(* 3 4))
       (context (lambda (▢ ) (* (+ ▢ 5) 2)))) 
  (test "context of '(* 3 4) in '(* (+ (* 3 4) 5) 2)"
   34 (context (eval sub-sexp))))

 (let ((sexp '(define tester
               (lambda (n)
                (if (zero? n)
                 (display (+ 3 (* 4 (+ 5 6))))
                 (display (* (+ (* 3 4) 5) 2)))
                n)))
       (target-sexp '(+ 10 (tester 1)))
       (subject-sexp '(* 3 4))
       (context (lambda (▢ )
                 (+ 10 (begin
                        (display (* (+ ▢ 5) 2)) 
                        1)))))
  (test-assert "context of '(* 3 4) in '(+ 10 (tester 1))" #t))
 )

 (test-group "about escape procedures"

  (define escape-* (escaper *))

  ; the waiting + is abandoned
  (test 10 (+ (escape-* 5 2) 3))

  (test 10 (+ ((escaper *) 5 2) 3))

  (test 8 (+ ((escaper (lambda (x)
                        (- (* x 3) 7))) 
              5) 
           4))

  (test 8 (+ ((escaper (lambda (x)
                        ((escaper -) (* x 3) 7))) 
              5) 
           4))

  (test 15 ((lambda (x) 
             (* x 3)) 
            5))

(test 15 (+ ((escaper (lambda (x)
                       (- 
                        ((escaper *) x 3) 7))) 
             5) 
          4))

    (test 15 (+ ((escaper (lambda (x)
                           ((escaper -) 
                            ((escaper *) x 3) 7))) 
                 5) 
              4))

(test 15 (/ (+ ((escaper (lambda (x)
                          (- (* x 3) 7))) 
                5) 
             4) 
          2))

    ; 16.4.1
(test -1 ((escaper add1) ((escaper sub1) 0)))

    ; 16.4.2
(let ((escape-cons (escaper cons)))
 (test '(3) (escape-cons 1 (escape-cons 2 (escape-cons 3 '())))))

    ; 16.5
(let ((reset (lambda ()
              ((escaper (lambda () '(2)))))))
 (test '(2) (cons 1 (reset))))

    (let ((a-receiver (lambda (continuation) 6))
          (another-receiver (lambda (continuation) (continuation 6)))
          (yet-another-receiver (lambda (continuation) (+ 2 (continuation 6))))
          (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
     (test 27 (+ 3 (* 4 (a-receiver (escaper context)))))
     (test 27 (+ 3 (* 4 (another-receiver (escaper context)))))
     (test 27 (+ 3 (* 4 (yet-another-receiver (escaper context)))))
    )

    ; 16.9.1
    (let ((sexp '(- 3 (* 5 ((lambda (continuation) (continuation 5))
                            (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
          (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
          (receiver (lambda (continuation) (continuation 5))))
     (test -22 (- 3 (* 5 (receiver (escaper context)))))
     (test -22 (- 3 (* 5 (call/cc receiver)))))

    ; 16.9.2
    (let ((sexp '(- 3 (* 5 ((lambda (continuation) 5)
                            (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
          (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
          (receiver (lambda (continuation) 5)))
     (test -22 (- 3 (* 5 (receiver (escaper context)))))
     (test -22 (- 3 (* 5 (call/cc receiver)))))

    ; 16.9.3
    (let ((sexp '(- 3 (* 5 ((lambda (continuation) (+ 1000 (continuation 5)))
                            (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
          (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
          (receiver (lambda (continuation) (+ 1000 (continuation 5)))))
     (test -22 (- 3 (* 5 (receiver (escaper context)))))
     (test -22 (- 3 (* 5 (call/cc receiver)))))

    ; preparing for experiments
    (let ((result "any value")
          (result/cc "any value")
          ; receivers
          (receiver-1 (lambda (proc)
                       (proc (list 1))))
          (receiver-2 (lambda (proc)
                       (proc (list (proc (list 2))))))
          (receiver-3 (lambda (proc)
                       (proc (list (proc (list 3 proc))))))
          ; callers
          (display/return (lambda (x)
                           (display x)
                           x))
          (answer-maker (lambda (x)
                         (cons 'answer-is (display/return x))))
          (call (lambda (receiver)
                 (receiver display/return))))

     ; first experiment
     (let ((displayed-string (call-with-output-string 
                              (lambda (port)
                               (set! result (answer-maker (call receiver-1))))))
           (displayed/cc-string (call-with-output-string 
                                 (lambda (port)
                                  (set! result/cc (answer-maker (call/cc receiver-1)))))))
      (test "(1)(1)" displayed-string)
      (test result '(answer-is 1))
      (test "(1)" displayed/cc-string)
      (test result/cc '(answer-is 1)))

     ; second experiment
    (let ((displayed-string (call-with-output-string 
                             (lambda (port)
                              (set! result (answer-maker (call receiver-2))))))
          (displayed/cc-string (call-with-output-string 
                                (lambda (port)
                                 (set! result/cc (answer-maker (call/cc receiver-2)))))))
     (test "(2)((2))((2))" displayed-string)
     (test result '(answer-is (2)))
     (test "(2)" displayed/cc-string)
     (test result/cc '(answer-is 2)))

    ; third experiment
    (let ((displayed-string (call-with-output-string 
                             (lambda (port)
                              (set! result (answer-maker (call receiver-3))))))
          (displayed/cc-string (call-with-output-string 
                                (lambda (port)
                                 (set! result/cc (answer-maker (call/cc receiver-3))))))
          (d/r-procstr "#<procedure (display/return x)>")
          (call/cc-proc-str "#<procedure (call-with-current-continuation proc1897)>"))
     (test displayed-string 
      (string-append "(3 " d/r-procstr ")((3 " d/r-procstr "))((3 " d/r-procstr "))"))
     (test result `(answer-is (3 ,display/return)))
     (test "(1000)" (call-with-output-string 
                     (lambda (proc) 
                      ((cadr (cdr result)) (list 1000)))))
     (test result `(answer-is (3 ,display/return)))
     (test displayed/cc-string (string-append "(3 " call/cc-proc-str ")"))
     (test result/cc `(answer-is 3 ,call/cc))
     (test "1000" (call-with-output-string 
                   (lambda (proc)  
                    ((caddr result/cc) (list 1000)))))
     (test result/cc `(answer-is 1000))))
    )
;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
