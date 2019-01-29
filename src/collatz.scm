
 ;; the following definitions resembles this article of prof. Kozen:
 ;; http://www.cs.cornell.edu/courses/cs3110/2011sp/recitations/rec26-cps/cps.htm

(module collatz *

 (import scheme (chicken base))
 (import commons continuations)

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
       (letrec ((C (lambda (x col)
                    (cond
                     ((equal? x 1)   (hop (col (list 1))))
                     ((even? x)      (C (/ x 2) (lambda (lst) (col (cons x lst)))))
                     (else           (C (+ 1 (* 3 x)) (lambda (lst) (col (cons x lst)))))))))
        (C x identity)))))

    (define collatz&co-abridged2
     (lambda (x)
      (letrec ((C (lambda (x col)
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
                             (lambda (cont)
                              (cond
                               ((equal? x 1)   (hop (cont (list 1))))
                               ((even? x)      (cons x (call/cc (C (/ x 2)))))
                               (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
                 (call/cc (C x)))))))

    )




