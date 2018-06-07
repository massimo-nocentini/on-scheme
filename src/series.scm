
(module series *

 (import chicken scheme)

 (use numbers data-structures)

 (use commons streams)

 (define range
  (lambda (low high #!key (by add1))
   (letrec ((R (Λ (low)
                (cond
                 ((>= low high) stream:empty)
                 (else (stream:cons low (R (by low))))))))
    (R low))))

 (define divisable-by?
  (lambda (y)
   (lambda (x)
    (equal? (remainder x y) 0))))

 (define eratosthenes
  (Λ (s)
   (stream:dest/car+cdr ((s (prime primes)))
    (stream:cons prime (eratosthenes
                        ((stream:filter (compose not (divisable-by? prime)))
                         primes))))))

 (define fibonacci/stateful
  (Λ (a b)
   (stream:cons a (fibonacci/stateful b (+ a b)))))

 (define make-prime?
  (lambda (primes)
   (let ((prime? (lambda (n)
                  (letrec ((P (lambda (α)
                               (let ((p (stream:car α)))
                                (cond
                                 ((> p (sqrt n)) #t)
                                 (((divisable-by? p) n) #f)
                                 (else (P (stream:cdr α))))))))
                   (P primes)))))
    prime?)))

 (define multiples-of (compose stream:filter divisable-by?))
 (define not-multiples-of 
  (lambda (p) 
   (stream:filter (compose not (divisable-by? p)))))

 (define evens (stream:filter even?))
 (define odds (stream:filter odd?))

 (define range/0-9 (range 0 10))

 (define numbers/nats (stream:from 0))
 (define numbers/nats>0 (stream:cdr numbers/nats))
 (define numbers/nats>1 (stream:cdr numbers/nats>0))
 (define numbers/triangular ((stream:map (accumulator + 0)) numbers/nats))
 (define numbers/fibonacci (fibonacci/stateful 0 1))
 (define numbers/fibonacci>0 (stream:cdr numbers/fibonacci))
 (define numbers/fibonacci-triangular ((stream:map (accumulator + 0)) numbers/fibonacci))

 (define primes/eratosthenes (eratosthenes numbers/nats>1)) ; start with 2 as a first prime number

 )
