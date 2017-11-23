
(use test)
(use numbers)

    #;(define-syntax delay
     (syntax-rules ()
      ((delay expr) (make-promise expr))))

    (define stream-null? (compose null? force))
    (define stream-car (compose car force)) 
    (define stream-cdr (compose cdr force)) 

    (define empty-stream (delay '()))

    (define (stream-ref n s)
     (if (= n 0)
      (stream-car s)
      (stream-ref (- n 1) (stream-cdr s))))

    (define-syntax stream-cons
     (syntax-rules ()
      ((stream-cons a d) (delay (cons a d)))))

    (define (stream-map proc s)
     (if (stream-null? s)
      empty-stream
      (stream-cons
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

    (define (stream-for-each proc s)
     (if (stream-null? s)
      '()
      (begin 
       (proc (stream-car s))
       (stream-for-each proc (stream-cdr s)))))

    (define (stream-range low high)
     (if (>= low high)
      empty-stream
      (stream-cons low (stream-range (+ low 1) high))))

(define (stream-filter pred? s)
 (delay-force
  (cond 
   ((stream-null? s) empty-stream)
   ((pred? (stream-car s))
    (stream-cons 
     (stream-car s)
     (stream-filter pred? (stream-cdr s))))
   (else (stream-filter pred? (stream-cdr s))))))

    (define stream-take 
     (lambda (n s)
      (cond
       ((or (equal? n 0) (stream-null? s)) '())
       (else (cons (stream-car s) (stream-take (sub1 n) (stream-cdr s)))))))

    (define stream->list
     (lambda (s)
      (stream-take -1 s)))

    (define stream-from
     (lambda (n)
      (stream-cons n (stream-from (add1 n)))))

    (define (display-stream s)
     (newline)
     (stream-for-each display-line s) 
     (newline))

    (define (display-line x)
     (newline)
     (display x))

; ******************************************************************************

    (define divisible?
     (lambda (x y)
      (equal? (remainder x y) 0)))

    (define divisable-by?
     (lambda (y)
      (lambda (x)
       (divisible? x y))))

    (define accum
     (let ((sum 0))
      (lambda (x)
       (set! sum (+ x sum))
       sum)))

    (define fib-gen
     (lambda (a b)
      (stream-cons a (fib-gen b (+ a b)))))

    (define eratosthenes
     (lambda (s)
      (let ((prime (stream-car s)))
       (stream-cons prime (eratosthenes (stream-filter 
                                         (compose not (divisable-by? prime)) 
                                         (stream-cdr s)))))))

    (define stream-repeat
     (lambda (n)
      (letrec ((R (stream-cons n R))) 
       R)))

    (define stream-zip-with 
     (lambda (op)
      (letrec ((Z (lambda streams
                   (stream-cons 
                    (apply op (map stream-car streams)) 
                    (apply Z (map stream-cdr streams))))))
       Z)))
      

    #;(define stream-zip-with 
     (lambda (op)
      (letrec ((Z (lambda streams
                   (cond
                    ((any stream-null? streams) empty-stream)
                    (else (stream-cons 
                           (apply op (map stream-car streams)) 
                           (apply Z (map stream-cdr streams))))))))
      Z)))

    (define make-prime?
     (lambda (primes)
      (let ((prime? (lambda (n)
                     (letrec ((P (lambda (ps)
                                  (let ((p (stream-car ps)))
                                   (cond
                                    ((> p (sqrt n)) #t)
                                    ((divisible? n p) #f)
                                    (else (P (stream-cdr ps))))))))
                      (P primes)))))
       prime?)))

    (define stream-cumulatives 
     (lambda (op)
      (lambda (s)
       (letrec ((C (lambda streams
                    ;((compose display stream-car car) streams)
                    (stream-cons 
                     (apply op (map stream-car streams))
                     ;(apply C (cons (stream-cdr s) streams))))))
                     (apply C (cons ((compose stream-cdr car) streams) streams))))))
        (apply C (list s))))))

    (define list->stream
     (lambda (l)
      (cond 
       ((null? l) empty-stream)
       (else (stream-cons (car l) (list->stream (cdr l)))))))

    (define stream-append
     (letrec ((S (lambda (r s)
                  (cond 
                   ((stream-null? r) s)
                   (else (stream-cons (stream-car r) (S (stream-cdr r) s)))))))
      (lambda streams
       (delay-force
        (cond
         ((null? streams) empty-stream)
         (else (let ((first (car streams))
                     (rest (cdr streams)))
                (S first (apply stream-append rest)))))))))

    (define stream-binary-merge
     (lambda (pred?)
      (letrec ((M (lambda (s r)
                   (delay-force ; to allow *safe* composition in foldings
                    (cond
                     ((stream-null? s) r)
                     ((stream-null? r) s)
                     (else
                      (let ((scar (stream-car s))
                            (rcar (stream-car r)))
                       (cond
                        ((pred? scar rcar) (stream-cons scar (M (stream-cdr s) r)))
                        (else (stream-cons rcar (M s (stream-cdr r))))))))))))
       M)))

    (define stream-merge
     (lambda (pred?)
      (lambda streams
       (foldr (stream-binary-merge pred?) empty-stream streams))))

    (define radix-expand 
     (lambda (num den radix)
      (stream-cons
       (quotient (* num radix) den)
       (radix-expand (remainder (* num radix) den) den radix))))

    (define integrate-series
     (lambda (s)
      (letrec ((I (lambda (s n)
                   (stream-cons 
                    (/ (stream-car s) (stream-car n)) 
                    (I (stream-cdr s) (stream-cdr n))))))
       (I s (stream-from 1)))))

    (define add-series (stream-zip-with +))

    (define scale-series 
     (lambda (a)
      (lambda (s)
       ((stream-zip-with *) (stream-repeat a) s))))

    (define mul-series
     (lambda (s r)
      (stream-cons 
       (* (stream-car s) (stream-car r))
       (add-series
        ((scale-series (stream-car s)) (stream-cdr r))
        (mul-series (stream-cdr s) r)))))

    (define inverse-series
     (lambda (s)
      (let ((one (stream-cons 1 (stream-repeat 0))))
       (letrec ((I (stream-cons 1 ((scale-series -1) 
                                   (mul-series (stream-cdr s) I)))))
        I))))

    (define division-series
     (lambda (num denum)
      (mul-series num (inverse-series denum))))

    (define run-tests
     (lambda ()
      (let* (
             (first-10-nats '(0 1 2 3 4 5 6 7 8 9))
             (x (stream-range 0 10))
             (seq (stream-map accum (stream-range 1 20)))
             (evens (stream-filter even? seq))
             (multiples-of-5 (stream-filter (divisable-by? 5) seq))
             (nats (stream-from 0))
             (nats>0 (stream-cdr (stream-from 0)))
             (no-7s (stream-filter (compose not (divisable-by? 7)) nats))
             (fibs (fib-gen 0 1))
             (primes (eratosthenes (stream-from 2)))
             (ones (stream-repeat 1))
             (frac/1-7 (radix-expand 1 7 10))
             (frac/13-7 (radix-expand 13 7 10))
             (frac/3-8 (radix-expand 3 8 10))
            )
       (test 5 (stream-ref 5 x))
       (test 7 (stream-ref 7 x))
       (test
        '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190)
        (stream-take 19 seq))
       (test 120 (stream-ref 6 evens))
       (test '(3 4 5 6 7) (stream-take 5 (stream-from 3)))
       (test '(10 15 45 55 105 120 190) (stream->list multiples-of-5))
       (test '(0 1 2 3 4 5 6 7 8 9) (stream-take 10 nats))
       (test 117 (stream-ref 100 no-7s))
    (test first-10-nats ((compose stream->list list->stream) first-10-nats)) ; therefore `identity`
    (test '(0 1 1 2 3 5 8 13 21 34) (stream-take 10 fibs))
    (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
     (stream-take 100 primes))
    (test '(1 1 1 1 1 1 1 1 1 1) (stream-take 10 ones))
    (test '(1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) ; 1/7 \sim 0.142..
     (stream-take 20 frac/1-7))
    (test '(18 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5) ; 13/7 \sim 18.571.. wrong usage!!
     (stream-take 20 frac/13-7))
    (test '(3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) ; 3/8 = 0.375
     (stream-take 20 frac/3-8))
    (letrec ((nats (stream-cons 0 ((stream-zip-with +) ones nats)))
             (fibs (stream-cons 0 (stream-cons 1 ((stream-zip-with +) fibs (stream-cdr fibs)))))
             (squares (stream-cons 1 ((stream-zip-with *) squares (stream-repeat 2))))
             (primes (stream-cons 2 (stream-filter (make-prime? primes) (stream-from 3))))
             (doubles (stream-cons 1 ((stream-zip-with +) doubles doubles)))
             (factorials (stream-cons 1 ((stream-zip-with *) factorials nats>0)))
             (S (stream-cons 1 ((stream-merge <) 
                                ((stream-zip-with *) S (stream-repeat 2)) 
                                ((stream-zip-with *) S (stream-repeat 3)) 
                                ((stream-zip-with *) S (stream-repeat 5)))))
             (exponential-series (stream-cons 1 (integrate-series exponential-series)))
             (cosine-series (stream-cons 1 (integrate-series ((scale-series -1) sine-series))))
             (sine-series (stream-cons 0 (integrate-series cosine-series)))
            )
     (test '(0 1 2 3 4 5 6 7 8 9) (stream-take 10 nats))
     (test '(0 1 1 2 3 5 8 13 21 34) (stream-take 10 fibs))
     (test '(1 2 4 8 16 32 64 128 256 512) (stream-take 10 squares))
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
      (stream-take 100 primes))
     (test '(1 2 4 8 16 32 64 128 256 512) (stream-take 10 doubles))
     (test '(1 1 2 6 24 120 720 5040 40320 362880) (stream-take 10 factorials))
    (let
     ((nats-cumulatives ((stream-cumulatives +) nats))
      (fibs-cumulatives ((stream-cumulatives +) fibs))
      (fibs-produlatives ((stream-cumulatives *) (stream-cdr fibs))))
     (test '(0 1 3 6 10 15 21 28 36 45) (stream-take 10 nats-cumulatives))
     (test '(0 1 2 4 7 12 20 33 54 88) (stream-take 10 fibs-cumulatives)) ; https://oeis.org/A000071
     (test-assert (equal?
                   (stream-take 10 fibs-cumulatives)
                   (stream-take 10 ((stream-zip-with -) 
                                    ((compose stream-cdr stream-cdr) fibs) 
                                    (stream-repeat 1)))))
     (test '(1 1 2 6 30 240 3120 65520 2227680 122522400) (stream-take 10 fibs-produlatives)))
    (test '(1 2 3 4 5 6 6 8 9 10) (stream-take 10 S))
    (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880) (stream-take 10 exponential-series))
    (test '(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0) (stream-take 10 cosine-series))
    (test '(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880) (stream-take 10 sine-series))
    (test '(1 2 3 4 5 6 7 8 9 10) (stream-take 10 (mul-series ones ones)))
    (test '(0 1 3 6 10 15 21 28 36 45) (stream-take 10 (mul-series nats ones)))
    (let ((sine2+cosine2 (add-series
                          (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series)))
          (fibs>0 (stream-cdr fibs))
          (tangent-series (division-series sine-series cosine-series))
         )
     (test '(1 0 0 0 0 0 0 0 0 0) (stream-take 10 sine2+cosine2))
     (test '(1 -1 0 0 0 0 0 0 0 0) (stream-take 10 (inverse-series ones)))
     (test '(1 -2 1 0 0 0 0 0 0 0) (stream-take 10 (inverse-series nats>0)))
     (test '(1 0 0 0 0 0 0 0 0 0) (stream-take 10 (mul-series nats>0 (inverse-series nats>0))))
     (test '(1 -1 -1 0 0 0 0 0 0 0) (stream-take 10 (inverse-series (stream-cdr fibs))))
     (test '(1 0 0 0 0 0 0 0 0 0) (stream-take 10 (mul-series fibs>0 (inverse-series fibs>0))))
     (test '(0 1 0 1/3 0 2/15 0 17/315 0 62/2835) (stream-take 10 tangent-series)))
    ))
    ))

(run-tests)

    ;(test-exit)
