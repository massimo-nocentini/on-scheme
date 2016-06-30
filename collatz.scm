
(include "continuations.scm") ; inclusion is necessary to have macros at compile-time

(import continuations) ; where our "continuations stuff" lie

(require-extension test)

;; the following definitions resembles this article of prof. Kozen:
;; http://www.cs.cornell.edu/courses/cs3110/2011sp/recitations/rec26-cps/cps.htm

    (define collatz
     (lambda (x)
      (cond 
       ((equal? x 1)   (list 1))
       ((even? x)      (cons x (collatz (/ x 2))))
       (else           (cons x (collatz (+ 1 (* 3 x)))))))) 

    (define collatz&co
     (lambda (x)
      (letrec ((C (lambda (x col) ; `col` stands for `collector`
                   (cond 
                    ((equal? x 1)   (col (list 1)))
                    ((even? x)      (C (/ x 2) (lambda (lst) (col (cons x lst)))))
                    (else           (C (+ 1 (* 3 x)) (lambda (lst) (col (cons x lst)))))))))
       (C x identity))))

    (define collatz&co-abridged
     (lambda (x)
      (letcc hop
       (letrec ((C (lambda (x col) ; `col` stands for `collector`
                    (cond
                     ((equal? x 1)   (hop (col (list 1))))
                     ((even? x)      (C (/ x 2) (lambda (lst) (col (cons x lst)))))
                     (else           (C (+ 1 (* 3 x)) (lambda (lst) (col (cons x lst)))))))))
        (C x identity)))))

    (define collatz&co-abridged2
     (lambda (x)
      (letrec ((C (lambda (x col) ; `col` stands for `collector`
                   (cond
                    ((equal? x 1)   (col (list 1)))
                    ((even? x)      (C (/ x 2) (lambda (lst) (col (cons x lst)))))
                    (else           (C (+ 1 (* 3 x)) (lambda (lst) (col (cons x lst)))))))))
       (letcc hop (C x hop)))))

    (define collatz&cc
     (lambda (x)
      (letrec ((C (lambda (x)
                   (lambda (cont) ; `cont` stands for `continuation`
                    (cond
                     ((equal? x 1)   (cont (list 1)))
                     ((even? x)      (cons x (call/cc (C (/ x 2)))))
                     (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
       (call/cc (C x)))))

    (define collatz&cc-abridged
     (lambda (x)
      (call/cc (lambda (hop)
                (letrec ((C (lambda (x)
                             (lambda (cont) ; `cont` stands for `continuation`
                              (cond
                               ((equal? x 1)   (hop (cont (list 1))))
                               ((even? x)      (cons x (call/cc (C (/ x 2)))))
                               (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
                 (call/cc (C x)))))))


    (test-group "Collatz 3x+1 problem"

     (let ((expected '(51 154 77 232 116 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)))
      (test expected (collatz 51))
      (test expected (collatz&co 51))
      (test expected (collatz&co-abridged 51))
      (test expected (collatz&co-abridged2 51))
      (test expected (collatz&cc 51))
      (test expected (collatz&cc-abridged 51)))

    ) ; end of tests group



