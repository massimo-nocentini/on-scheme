
;; before attempting Kozen stuff, we start small with "The Little Schemer".

(module seasoned-schemer *

 (import scheme chicken)
 (use continuations matchable data-structures)

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


    (define intersect-old
     (lambda (this that)
      (letrec ((I (lambda (set)
                   (cond
                    ((null? set) (quote ()))
                    ((member (car set) that) (cons (car set) (I (cdr set))))
                    (else (I (cdr set)))))))
       (I this))))

    (define intersect
     (lambda (this that)
      (letrec ((I (match-lambda 
                   (() '())
                   ((i . is) (if (member i that) (cons i (I is)) (I is))))))
       (I this))))

    (define intersect+all
     (lambda (sets)
      (letcc hop
       (letrec ((intersect  ; 13th C.: "we can do whatever we want with the minor
                            ; version of `intersect`, nobody cares because it is protected".
                 (lambda (this that)
                  (letrec ((I (match-lambda 
                               (() '())
                               ((i . is) (if (member i that) (cons i (I is)) (I is))))))
                   (cond 
                    ((null? that) (hop (quote ()))) ; 14th C.: spot it in the middle of recursion,
                                                    ; use `hop` to return '() without further delay.
                    (else (I this))))))
                (A (match-lambda
                    ((() ...) (hop (quote ()))) ; 14th C.: "this is it: the result is '()
                                                ; and that's all there is to it",
                                                ; spot it while reading the input.
                    ((set) set)
                    ((this . rest) (intersect this (A rest))))))
        (cond
         ((null? sets) (quote '()))
         (else (A sets)))))))

    (define comb-upto-last
     (lambda (atom k lat)
      (reverse (letcc skip ; "skip" because we discard many skipping in favor of the last one.
                (letrec ((R (lambda (prefix k lat)
                             (match lat
                              (() prefix) ; to implement `rember-upto-last`, which takes an
                                            ; atom `a` and a list `lat`, then removes all the
                                            ; atoms from `lat` upto an including the last 
                                            ; occurrence of `a`. If there are no `a`s, returns `lat`.
                                            ; The new version would not stop looking at elements
                                            ; in `lat` but would also throw away everything
                                            ; it has seen so far, in the sense to forget some
                                            ; computation that it had remembered somewhere.
                                            ; Respect `intersect+all`, which knows what the result
                                            ; is when it finds '(), this function knows which
                                            ; pieces of the list are *not* in the result.
                                            ; So, suppose that this function sees the atom `a`,
                                            ; so it forget the pending computations and should it
                                            ; restart the process of searching through the `cdr`. 
                              ((a . as) 
                               (cond
                                ((equal? atom a)
                                 (cond
                                  ((zero? k) prefix) 
                                  (else (skip (R (quote ()) (sub1 k) as)))))
                                (else (R (cons a prefix) k as))))))))
                 (R (quote ()) (sub1 k) lat))))))

    ) ; end of module `seasoned-schemer`











