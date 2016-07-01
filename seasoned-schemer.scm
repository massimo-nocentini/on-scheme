
;; before attempting Kozen stuff, we start small with "The Little Schemer".

(module seasoned-schemer *

 (import scheme chicken data-structures)
 (use continuations)

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

    (define two-in-a-row?
     (lambda (lat) ; `lat` stands for `List of AToms`
      (cond 
       ((null? lat) #f)
       (else (let T ((preceding (car lat)) ; named-let: http://wiki.call-cc.org/man/4/The%20R5RS%20standard#iteration
                     (rest      (cdr lat)))
              (cond
               ((null? rest) #f)
               (else (or 
                      (equal? preceding (car rest)) 
                      (T (car rest) (cdr rest))))))))))

    (define two-in-a-row?&hop
     (lambda (lat) ; `lat` stands for `List of AToms`
      (cond 
       ((null? lat) #f)
       (else
        (letcc hop 
         (let T ((preceding (car lat)) ; named-let: http://wiki.call-cc.org/man/4/The%20R5RS%20standard#iteration
                 (rest      (cdr lat)))
          (cond
           ((null? rest) (hop #f))
           ((equal? preceding (car rest)) (hop #t))
           (else (T (car rest) (cdr rest))))))))))


    ) ; end of module `seasoned-schemer`
