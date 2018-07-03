
(module series *

 (import chicken scheme)

 (use numbers data-structures)

 ;(import (prefix streams streams))

 (use commons streams)


 (define series:0 (stream:repeat 0))

 (define stream:const
  (Λ (n)
   (stream:cons n series:0)))

 (define stream:1 (stream:const 1))
 (define stream:1s (stream:repeat 1))

 (define range
  (lambda (low high #!key (by add1))
   (letrec ((R (Λ (low)
                (cond
                 ((>= low high) stream:empty)
                 (else (stream:cons low (R (by low))))))))
    (R low))))

 (define stream:from
  (Λ (n)
   (stream:cons n (stream:from (add1 n)))))

    (define expt-series
     (lambda (n)
      (lambda (s)
       (letrec ((E (Λ (n)
                    (cond
                     ((zero? n) stream:1)
                     (else (mul-series s (E (sub1 n))))))))
        (E n)))))

    (define list->poly      (list-> series:0))

    (define radix-expand
     (Λ (num den radix)
      (stream:cons
       (quotient (* num radix) den)
       (radix-expand (remainder (* num radix) den) den radix))))

    (define integrate-series
     (Λ (s)
      (letrec ((I (Λ (s n)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:dest/car+cdr (n ∅)
                                  ((ncar ncdr) (stream:cons 
                                                (/ scar ncar) 
                                                (I scdr ncdr)))))))))
       (stream:cons 0 (I s (stream:from 1))))))

    (define derivative-series
     (Λ (s)
      (letrec ((D (Λ (s n)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:dest/car+cdr (n ∅)
                                  ((ncar ncdr) (stream:cons 
                                                (* scar ncar) 
                                                (D scdr ncdr)))))))))
       (D (stream:cdr s) (stream:from 1)))))

    (define add-series (stream:zip-with +))
    (define sub-series (stream:zip-with -))

    (define scale-series
     (lambda (a)
      (Λ (s)
       ((stream:zip-with *) (stream:repeat a) s))))

    (define mul-series (stream:convolution * scale-series add-series))

    (define mul-series*
     (lambda series
      (letrec ((M (stream:convolution
                   (lambda (a b)
                    (cond
                     ((and (promise? a) (promise? b)) (mul-series a b))
                     ((and (promise? a) (number? b)) ((scale-series b) a))
                     ((and (number? a) (promise? b)) ((scale-series a) b))
                     ((and (number? a) (number? b)) (stream:const (* a b)))
                     (else (error "mul-series" "f₀ not a number" a b))))
                   (lambda (a)
                    (lambda (b)
                     (cond
                      ((number? a) ((stream:map (scale-series a)) b))
                      (else ((stream:zip-with mul-series) a b)))))
                   (stream:zip-with add-series))))
       (foldr M stream:1 series))))

    (define inverse-series
     (Λ (s)
      (stream:dest/car+cdr (s ∅)
       ((scar scdr) (letdelay ((I (stream:cons 
                                   (/ 1 scar) 
                                   ((scale-series (/ -1 scar))
                                    (mul-series scdr I)))))
                     I)))))

    (define division-series&inversion
     (lambda (num denum)
      (mul-series num (inverse-series denum))))

    (define division-series
     (Λ (α β)
      (stream:dest/car+cdr (α ∅)
       ((a α-cdr) (stream:dest/car+cdr (β ∅)
                   ((b β-cdr) (cond
                               ((and (zero? a) (zero? b)) (division-series α-cdr β-cdr))
                               (else (let ((q (/ a b)))
                                      (stream:cons q (division-series
                                                      (sub-series α-cdr ((scale-series q) β-cdr))
                                                      β)))))))))))

    (define integral-series&co
     (lambda (init dt)
      (lambda (s)
       (letdelay ((I (stream:cons init (add-series ((scale-series dt) s) I))))
        I))))

    (define integral-series&rec
     (lambda (init dt)
      (Λ (s)
       (stream:cons init (stream:dest/car+cdr (s ∅)
                          ((scar scdr) ((integral-series&rec (+ (* dt scar) init) dt) scdr)))))))

    (define ode-solve-1st
     (lambda (integral)
      (lambda (f)
       (letdelay ((y (integral dy))
                  (dy ((stream:map f) y)))
        y))))

    (define ode-solve-2nd
     (lambda (integral-1st integral-2nd)
      (lambda (f)
       (letdelay ((y (integral-1st dy))
                  (dy (integral-2nd ddy))
                  (ddy ((stream:zip-with f) y dy)))
        y))))

    (define stream:sqrt
     (lambda (n)
      (let* ((average (lambda args (/ (foldr + 0 args) (length args))))
             (improve (lambda (guess) (average guess (/ n guess)))))
       (letdelay ((guesses (stream:cons 1 ((stream:map improve) guesses))))
        guesses))))

    (define pi-series
     (letrec ((summands (Λ (n)
                         (stream:cons (/ 1 n) ((stream:map -)
                                               (summands (+ n 2)))))))
      ((scale-series 4) ((stream:scan +) (summands 1)))))

    (define log2-series
     (letrec ((summands (Λ (n)
                         (stream:cons (/ 1 n) ((stream:map -) (summands (add1 n)))))))
      ((stream:scan +) (summands 1))))

    (define sqrt-series
     (Λ (α)
      (cond
       ((and (zero? (stream:car α)) (zero? (stream:cadr α)))
        (stream:cons 0 (sqrt-series (stream:cddr α))))
       (else (letdelay ((Q (add-series
                            stream:1
                            (integrate-series (division-series
                                               (derivative-series α)
                                               ((scale-series 2) Q))))))
              Q)))))

    (define exp-series
     (Λ (α)
      (cond
       (((○ not equal?) 0 (stream:car α))
        (error "exp-series" "α₀ not zero"))
       (else (letdelay ((Y (add-series
                            stream:1
                            (integrate-series
                             (mul-series Y (derivative-series α))))))
              Y)))))

    (define euler-transform
     (Λ (s)
      (let ((n-1 (stream:car s))
            (n (stream:cadr s))
            (n+1 (stream:caddr s))
            (square (lambda (x) (* x x))))
       (stream:cons
        (- n+1 (/ (square (- n+1 n)) (+ n+1 n-1 (* -2 n))))
        (euler-transform (stream:cdr s))))))

    (define Pythagorean-triples
     (lambda (n)
      (let ((F (lambda (triple)
                (equal?
                 (+ (expt (car triple) n) (expt (cadr triple) n))
                 (expt (caddr triple) n))))
            (nats (stream:from 1))
            (slow-enumeration stream:enumerate-upper)
            (fast-enumeration (stream:enumerate-weighted
                               (lambda (i j #!optional (k 0))
                                ((○ abs -)
                                 (+ (expt i n) (expt j n))
                                 (expt k n))))
            ))
       ((stream:filter F) (fast-enumeration nats nats nats)))))

    (define random-numbers
     (let ((rand-update (lambda (u) (random 1000))))
      (lambda (init)
       (letdelay ((R (stream:cons init ((stream:map rand-update) R))))
        R))))

    (define stream:montecarlo
     (lambda (tosses)
      (letrec ((MC (Λ (t s u)
                    (let ((N (lambda (s u)
                              (stream:cons (/ s (+ s u)) (MC (stream:cdr t) s u)))))
                     (cond
                      ((stream:car t) (N (add1 s) u))
                      (else (N s (add1 u))))))))
       (MC tosses 0 0))))

    (define riordan-array
     (Λ (d h)
      (stream:dest/car+cdr (d ∅)
       ((dcar dcdr) (stream:cons
                     (list dcar)
                     ((stream:zip-with cons) dcdr (riordan-array (mul-series d h) h)))))))

    (define formalvar-series
     (Λ (n)
      (cond
       ((zero? n) stream:1)
       (else (stream:cons 0 (formalvar-series (sub1 n)))))))

    (define catalan-series
     (letdelay ((C (stream:cons 1 (mul-series C C))))
      C))

    (define fibonacci-series
     (letdelay ((t (formalvar-series 1))
                (F (stream:cons 1 (add-series F (mul-series t F)))))
      F))

    (define compose-series
     (Λ (α β)
      (let-values (((M B) (let ((β₀ (stream:car β)))
                           (cond
                            ((equal? β₀ 0)            (values mul-series identity))       ; univariate series
                            ((equal? β₀ series:0)  (values mul-series* stream:const))  ; bivariate series
                            (else (error "compose-series" "β₀ neither 0 nor series:0" β₀))))))
       (letrec ((C (Λ (α β)
                    (stream:dest/car+cdr (α ∅)
                     ((a αs) (stream:dest/car+cdr (β ∅)
                              ((b βs) (stream:cons (B a) (M (C αs β) βs)))))))))
        (C α β)))))

    (define revert-series
     (Λ (α)
      (stream:dest/car+cdr (α ∅)
       ((α₀ αs) (cond
                 ((equal? α₀ 0) (letdelay ((R (stream:cons 0 (inverse-series
                                                              (compose-series αs R)))))
                                 R))
                 (else (error "revert-series" "α₀ not zero" α₀)))))))

 (define divisable-by?
  (lambda (y)
   (lambda (x)
    (equal? (remainder x y) 0))))

 (define eratosthenes
  (Λ (s)
   (stream:dest/car+cdr s
    ((prime primes) (stream:cons prime (eratosthenes
                                        ((stream:filter 
                                          (compose not (divisable-by? prime))) primes))))
    (else (error "Primes are infinite.")))))

 (define fibonacci/stateful
  (Λ (a b)
   (stream:cons a (fibonacci/stateful b (+ a b)))))

 (define powers-of/∞
  (lambda(n)
   (letdelay ((α (stream:cons 1 ((stream:zip-with *) α (stream:repeat n)))))
    α)))

 (define multiples-of (○ stream:filter divisable-by?))

 (define not-multiples-of
  (lambda (p)
   (stream:filter (○ not (divisable-by? p)))))

 (define evens (stream:filter even?))
 (define odds (stream:filter odd?))

 (define range/0-9 (range 0 10))

 (define numbers/nats
  (stream:from 0))

 (define numbers/nats>0
  (stream:cdr numbers/nats))

 (define numbers/nats>1
  (stream:cdr numbers/nats>0))

 (define numbers/triangular
  ((stream:map (accumulator + 0)) numbers/nats))

 (define numbers/fibonacci
  (fibonacci/stateful 0 1))

 (define numbers/fibonacci>0
  (stream:cdr numbers/fibonacci))

 (define numbers/fibonacci-triangular
  ((stream:map (accumulator + 0)) numbers/fibonacci))

 (define-delay numbers/nats/∞
  (stream:cons 0 ((stream:zip-with +) numbers/nats/∞ stream:1s)))

 (define-delay numbers/fibs/∞
  (:⁺ 0 1 ((stream:zip-with +) numbers/fibs/∞ (stream:cdr numbers/fibs/∞))))
 ;(define-delay numbers/fibs/∞ (stream:cons 0 (stream:cons 1 ((stream:zip-with +) numbers/fibs/∞ (stream:cdr numbers/fibs/∞)))))

 (define-delay numbers/lucas/∞
  (:⁺ 2 1 ((stream:zip-with +) numbers/lucas/∞ (stream:cdr numbers/lucas/∞))))

 (define numbers/fibs>0/∞
  (stream:cdr numbers/fibs/∞))

 (define numbers/triangular/+scan
  ((stream:scan +) numbers/nats/∞))

 (define numbers/fibs/+scan
  ((stream:scan +) numbers/fibs/∞))

 (define numbers/fibs/∙scan
  ((stream:scan *) (stream:cdr numbers/fibs/∞)))

 (define numbers/powers-of-2
  (powers-of/∞ 2))

 (define numbers/powers-of-3
  (powers-of/∞ 3))

 (define primes/eratosthenes
  (eratosthenes numbers/nats>1))

 (define-delay primes/∞
  (let ((prime? (lambda (n)
                 (letrec ((P (lambda (α)
                              (let ((p (stream:car α)))
                               (cond
                                ((> p (sqrt n)) #t)
                                (((divisable-by? p) n) #f)
                                (else (P (stream:cdr α))))))))
                  (P primes/∞))))) ; here is the "whole meal" chunck
   (stream:cons 2 ((stream:filter prime?) (stream:from 3)))))

 (define-delay numbers/factorials/∞
  (stream:cons 1 ((stream:zip-with *) numbers/factorials/∞ numbers/nats>0)))

 (define-delay numbers/central-polygonal/∞
  (stream:cons 1 ((stream:zip-with +) numbers/central-polygonal/∞ numbers/nats>0)))

 (define-delay numbers/triangular/∞
  (stream:cons 0 ((stream:zip-with +) numbers/triangular/∞ numbers/nats>0)))

 (define numbers/powers-of-2/∞
  (stream:cons 1 ((stream:zip-with +) numbers/powers-of-2/∞ numbers/powers-of-2/∞)))

 (define numbers/powers-of-3/∞
  (stream:cons 1 ((stream:zip-with +) 
                  numbers/powers-of-3/∞ 
                  numbers/powers-of-3/∞ 
                  numbers/powers-of-3/∞)))

 (define max-dividing-power
  (lambda (n)
   (lambda (p)
    (letrec ((M (lambda (n c)
                 (let-values (((q r) (quotient&remainder n p)))
                  (cond
                   ((zero? r) (M q (add1 c))) ; tail-call so no need to use `call/cc`
                   (else c))))))
     (M n 0)))))

 (define factorization
  (Λ (n)
   ((stream:map (max-dividing-power n)) primes/∞)))

 (define prime?
  (lambda (n)
   (letrec ((P (lambda (α)
                (stream:dest/car+cdr (α ∅)
                 ((α₀ α⁺) (cond
                           ((equal? n α₀) #t)
                           ((< n α₀) #f)
                           (else (P α⁺))))))))
    (P primes/eratosthenes))))

    (define-delay ; mutually recursive definition
     (taylor/cosine (add-series stream:1 ((scale-series -1)
                                          (integrate-series taylor/sine))))
     (taylor/sine (integrate-series taylor/cosine)))


    (define-delay taylor/exponential
     (add-series stream:1 (integrate-series taylor/exponential)))

 )
