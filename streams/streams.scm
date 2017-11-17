
(require-extension test)

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

    (define stream-mature
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

    (let* ((x (stream-range 0 10))
           (seq (stream-map accum (stream-range 1 20)))
           (evens (stream-filter even? seq))
           (multiples-of-5 (stream-filter (divisable-by? 5) seq))
           (nats (stream-from 0))
           (nats>0 (stream-cdr (stream-from 0)))
           (no-7s (stream-filter (compose not (divisable-by? 7)) nats))
           (fibs (fib-gen 0 1))
           (primes (eratosthenes (stream-from 2)))
           (ones (stream-repeat 1))
          )
     (test 5 (stream-ref 5 x))
     (test 7 (stream-ref 7 x))
     (test
      '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190)
      (stream-take 19 seq))
     (test 120 (stream-ref 6 evens))
     (test '(3 4 5 6 7) (stream-take 5 (stream-from 3)))
     (test '(10 15 45 55 105 120 190) (stream-mature multiples-of-5))
     (test '(0 1 2 3 4 5 6 7 8 9) (stream-take 10 nats))
     (test 117 (stream-ref 100 no-7s))
     (test '(0 1 1 2 3 5 8 13 21 34) (stream-take 10 fibs))
    (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
     (stream-take 100 primes))
    (test '(1 1 1 1 1 1 1 1 1 1) (stream-take 10 ones))
    (letrec ((nats (stream-cons 0 ((stream-zip-with +) ones nats)))
             (fibs (stream-cons 0 (stream-cons 1 ((stream-zip-with +) fibs (stream-cdr fibs)))))
             (squares (stream-cons 1 ((stream-zip-with *) squares (stream-repeat 2))))
             (primes (stream-cons 2 (stream-filter (make-prime? primes) (stream-from 3))))
             (doubles (stream-cons 1 ((stream-zip-with +) doubles doubles)))
             (factorials (stream-cons 1 ((stream-zip-with *) factorials nats>0)))
             (nats-cumulatives ((stream-cumulatives +) nats>0))
            )
     (test '(0 1 2 3 4 5 6 7 8 9) (stream-take 10 nats)) 
     (test '(0 1 1 2 3 5 8 13 21 34) (stream-take 10 fibs))
     (test '(1 2 4 8 16 32 64 128 256 512) (stream-take 10 squares))
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
      (stream-take 100 primes))
     (test '(1 2 4 8 16 32 64 128 256 512) (stream-take 10 doubles))
     (test '(1 1 2 6 24 120 720 5040 40320 362880) (stream-take 10 factorials))
     (test '(1 3 6 10 15 21 28 36 45 55) (stream-take 10 nats-cumulatives))
    )
    )

;(test-exit)
