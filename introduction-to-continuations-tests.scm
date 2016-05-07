

(load "introduction-to-continuations.so")

(display "\n--- making `escaper` function ---\n")
(make-escaper)


(display "\n--- about contexts ---\n")

(let ((sexp '(+ 3 (* 4 (+ 5 6))))
      (sub-sexp '(+ 5 6))
      (context '(lambda (▢ ) (+ 3 (* 4 ▢ ))))) 
 (display "context of '(+ 5 6) in '(+ 3 (* 4 (+ 5 6))): ")
 (display context))


(display "\n--- about escape procedures ---\n")

;(define escape-* (escaper *))

;(tester 10 (+ ((escape-*) 5 2) 3))

(tester 10 (+ ((escaper *) 5 2) 3))

(tester 8 (+ ((escaper (lambda (x)
                        (- (* x 3) 7))) 
              5) 
           4))

(tester 8 (+ ((escaper (lambda (x)
                        ((escaper -) (* x 3) 7))) 
              5) 
           4))

(tester 15 ((lambda (x) 
             (* x 3)) 
            5))

(tester 15 (+ ((escaper (lambda (x)
                         (- ((escaper *) x 3) 7))) 
               5) 
            4))

    (tester 15 (+ ((escaper (lambda (x)
                             ((escaper -) 
                              ((escaper *) x 3) 7))) 
                   5) 
                4))

(tester 8 (/ (+ ((escaper (lambda (x)
                           (- (* x 3) 7))) 
                 5) 
              4) 
           2))

    ; 16.4.1
(tester -1 ((escaper add1) ((escaper sub1) 0)))

    ; 16.4.2
    (tester '(3)
     (let ((escape-cons (escaper cons)))
      (escape-cons 1 (escape-cons 2 (escape-cons 3 '())))))

    ; 16.5
    (tester '(2)
     (let ((reset (lambda ()
                   ((escaper (lambda () '(2)))))))
      (cons 1 (reset))))

    (tester 27 
     (let ((receiver (lambda (continuation) 6))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))

    (tester 27
     (let ((receiver (lambda (continuation) (continuation 6)))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))

    (tester 27
     (let ((receiver (lambda (continuation) (+ 2 (continuation 6))))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))


    ; 16.9.1
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (continuation 5))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) (continuation 5))))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (continuation 5))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) (continuation 5))))
      (- 3 (* 5 (call/cc receiver)))))


    ; 16.9.2
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) 5)
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) 5)))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) 5)
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) 5)))
      (- 3 (* 5 (call/cc receiver)))))

    ; 16.9.3
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (+ 1000 (continuation 5)))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) (+ 1000 (continuation 5)))))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (+ 1000 (continuation 5)))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) (+ 1000 (continuation 5)))))
      (- 3 (* 5 (call/cc receiver)))))

    ; preparing for experiments
    (define result "any value")
    (define result/cc "any value")

    ; receivers
    (define receiver-1 
     (lambda (proc)
      (proc (list 1))))

    (define receiver-2 
     (lambda (proc)
      (proc (list (proc (list 2))))))

    (define receiver-3 
     (lambda (proc)
      (proc (list (proc (list 3 proc))))))

    ; callers
    (define display/return 
     (lambda (x)
      (display x)
      x))

    (define answer-maker 
     (lambda (x)
      (cons 'answer-is (display/return x))))

    (define call 
     (lambda (receiver)
      (receiver display/return)))

    ; first experiment
    (tester "(1)(1)" (set! result (answer-maker (call receiver-1))))
    (tester '(answer-is 1) result)

    (tester "(1)" (set! result/cc (answer-maker (call/cc receiver-1))))
    (tester '(answer-is 1) result/cc)

    ; second experiment
    (tester "(2)((2))((2))" (set! result (answer-maker (call receiver-2))))
    (tester '(answer-is (2)) result)

    (tester "(2)" (set! result/cc (answer-maker (call/cc receiver-2))))
    (tester '(answer-is 2) result/cc)

    ; third experiment
    (tester 
     "(3 #<procedure (display/return x)>)((3 #<procedure (display/return x)>))((3 #<procedure (display/return x)>))" 
     (set! result (answer-maker (call receiver-3))))
    (tester `(answer-is (3 ,display/return)) result)
    (tester "(1000)" '(1000) ((cadr (cadr result)) (list 1000)))
    (tester `(answer-is (3 ,display/return)) result)

    (tester 
     "(3 #<procedure (continuation . results1901)>)" 
     (set! result/cc (answer-maker (call/cc receiver-3))))
    (tester `(answer-is 3 ,(caddr result/cc)) result/cc)
    (tester "(1000)" ((caddr result/cc) (list 1000)))
    (tester `(answer-is 1000) result/cc)







