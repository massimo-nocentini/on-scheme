
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


  ; the waiting + is abandoned
  (test 10 (+ (escape-* 5 2) 3))

  (define escape-* (escaper *))

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
    

)

;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
