
;; before attempting Kozen stuff, we start small with "The Little Schemeer".

    (define multi-insert*&co
     (lambda (new old_l old_r sexp coll)
      (let M ((sexp sexp) 
              (coll coll))
       (cond
        ((null? sexp) 
         (coll '() 0 0))
        ((atom? (car sexp))
         (cond
          ((equal? (car sexp) old_l) 
           (M (cdr sexp) (lambda (new_sexp L R) 
                          (coll (cons new (cons old_l new_sexp)) (add1 L) R))))
          ((equal? (car sexp) old_r) 
           (M (cdr sexp) (lambda (new_sexp L R) 
                          (coll (cons old_r (cons new new_sexp)) L (add1 R)))))
          (else
           (M (cdr sexp) (lambda (new_sexp L R) 
                          (coll (cons (car sexp) new_sexp) L R))))))
        (else 
         (M (car sexp) (lambda (new_sexp_car L_car R_car)
                        (M (cdr sexp) (lambda (new_sexp_cdr L_cdr R_cdr)
                                       (coll 
                                        (cons new_sexp_car new_sexp_cdr) 
                                        (+ L_car L_cdr) 
                                        (+ R_car R_cdr)))))))))))
         
    (define-syntax letcc
     (syntax-rules ()
      ((letcc label sexp ...) (call/cc (lambda (label) sexp ...)))))

    (define-syntax try
     (syntax-rules (else)
      ((try ((skip alpha) 
             (else beta)))
       (letcc success
        (letcc skip (success alpha))
        beta))))

(try ((rember-from-car (cons (rm a (car l) rember-from-car) (cdr l)))
      (else (cons (car l) (rm a (cdr l) oh)))))

    (define-syntax try
     (syntax-rules (else ->)
      ((try ((var cloj) alpha)
             (else beta))
       (letcc success
        (letcc skip 
         (let ((var (cloj skip)))
          (success alpha)))
        beta))))

(try 
 ((new-car (rm a (car l))) (cons new-car (cdr l)))
 (else (cons (car l) (rm a (cdr l) oh))))

;; the following definitions resembles this article of prof. Kozen:
;; http://www.cs.cornell.edu/courses/cs3110/2011sp/recitations/rec26-cps/cps.htm

    (define txp
     (lambda (x)
      (cond 
       ((equal? x 1)    (list 1))
       ((even? x)      (cons x (txp (/ x 2))))
       (else           (cons x (txp (+ 1 (* 3 x)))))))) 

    (define txp-cps
     (lambda (x)
      (letrec ((C (lambda (x col) ; `col` stands for `collector`
                   (cond 
                    ((equal? x 1)   (col (list 1)))
                    ((even? x)      (C (/ x 2) (lambda (lst) (col (cons x lst)))))
                    (else           (C (+ 1 (* 3 x)) (lambda (lst) (col (cons x lst)))))))))
       (C x (lambda (x) x)))))

    (define txp-cc
     (lambda (x)
      (letrec ((C (lambda (x)
                   (lambda (cont) ; `cont` stands for `continuation`
                    (cond
                     ((equal? x 1)   (cont (list 1)))
                     ((even? x)      (cons x (call/cc (C (/ x 2)))))
                     (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
       (call/cc (C x)))))

    (define txp-cc-abridged
     (lambda (x)
      (call/cc (lambda (hop)
                (letrec ((C (lambda (x)
                             (lambda (cont) ; `cont` stands for `continuation`
                              (cond
                               ((equal? x 1)   (hop (cont (list 1))))
                               ((even? x)      (cons x (call/cc (C (/ x 2)))))
                               (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
                 (call/cc (C x)))))))
