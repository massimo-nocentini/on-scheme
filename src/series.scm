
(module series *

 (import scheme (chicken base))
 (import (chicken random))
 (import commons streams)

 (define series:0 (stream:repeat 0))

 (define list->poly (list-> series:0))

 (define series:const
  (Λ (n)
   (stream:cons n series:0)))

 (define stream:1 (series:const 1))

 (define stream:1s (stream:repeat 1))

 (define series:range
  (lambda (low high #!key (by add1))
   (letrec ((R (Λ (low)
                (cond
                 ((>= low high) stream:empty)
                 (else (stream:cons low (R (by low))))))))
    (R low))))

 (define series:from
  (Λ (n)
   (stream:cons n (series:from (add1 n)))))

 (define series:+ (stream:zip-with +))
 (define series:- (stream:zip-with -))

    (define series:*
     (lambda (a)
      (Λ (s)
       ((stream:zip-with *) (stream:repeat a) s))))

    (define series:expt
     (lambda (n)
      (lambda (s)
       (letrec ((E (Λ (n)
                    (cond
                     ((zero? n) stream:1)
                     (else (series:× s (E (sub1 n))))))))
        (E n)))))

    (define series:∫
     (Λ (s)
      (letrec ((I (Λ (s n)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:dest/car+cdr (n ∅)
                                  ((ncar ncdr) (stream:cons
                                                (/ scar ncar)
                                                (I scdr ncdr)))))))))
       (stream:cons 0 (I s (series:from 1))))))

    (define series:∂
     (Λ (s)
      (letrec ((D (Λ (s n)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:dest/car+cdr (n ∅)
                                  ((ncar ncdr) (stream:cons
                                                (* scar ncar)
                                                (D scdr ncdr)))))))))
       (D (stream:cdr s) (series:from 1)))))

    (define series:× (stream:convolution * series:* series:+))

    (define series:×*
     (lambda series
      (letrec ((M (stream:convolution
                   (lambda (a b)
                    (cond
                     ((and (promise? a) (promise? b)) (series:× a b))
                     ((and (promise? a) (number? b)) ((series:* b) a))
                     ((and (number? a) (promise? b)) ((series:* a) b))
                     ((and (number? a) (number? b)) (series:const (* a b)))
                     (else (error "series:×" "f₀ not a number" a b))))
                   (lambda (a)
                    (lambda (b)
                     (cond
                      ((number? a) ((stream:map (series:* a)) b))
                      (else ((stream:zip-with series:×) a b)))))
                   (stream:zip-with series:+))))
       (foldr M stream:1 series))))

    (define series:⁻¹
     (Λ (s)
      (stream:dest/car+cdr (s ∅)
       ((scar scdr) 
        (letdelay ((I (stream:cons (⁻¹ scar) ((series:* (/ -1 scar)) 
                                              (series:× scdr I)))))
         I)))))

    (define series:/&inversion
     (lambda (num denum)
      (series:× num (series:⁻¹ denum))))

    (define series:/
     (Λ (α β)
      (stream:dest/car+cdr (α ∅)
       ((a α-cdr) (stream:dest/car+cdr (β ∅)
                   ((b β-cdr) (cond
                               ((and (zero? a) (zero? b)) (series:/ α-cdr β-cdr))
                               (else (let ((q (/ a b)))
                                      (stream:cons q (series:/
                                                      (series:- α-cdr ((series:* q) β-cdr))
                                                      β)))))))))))

    (define series:√
     (lambda (n)
      (let* ((average (lambda args (/ (foldr + 0 args) (length args))))
             (improve (lambda (guess) (average guess (/ n guess)))))
       (letdelay ((guesses (stream:cons 1 ((stream:map improve) guesses))))
        guesses))))

    (define series:○
     (Λ (α β)
      (let-values (((M B) (let ((β₀ (stream:car β)))
                           (cond
                            ((equal? β₀ 0)            (values series:× identity))       ; univariate series
                            ((equal? β₀ series:0)  (values series:×* series:const))  ; bivariate series
                            (else (error "series:○" "β₀ neither 0 nor series:0" β₀))))))
       (letrec ((C (Λ (α β)
                    (stream:dest/car+cdr (α ∅)
                     ((a αs) (stream:dest/car+cdr (β ∅)
                              ((b βs) (stream:cons (B a) (M (C αs β) βs)))))))))
        (C α β)))))
    
    (define series:√/∞
     (Λ (α)
      (cond
       ((and (zero? (stream:car α)) (zero? (stream:cadr α)))
        (stream:cons 0 (series:√/∞ (stream:cddr α))))
       (else (letdelay ((Q (series:+ 
                            stream:1 
                            (series:∫ (series:/ 
                                       (series:∂ α) 
                                       ((series:* 2) Q))))))
              Q)))))

    (define series:exp
     (Λ (α)
      (cond
       (((○ not equal?) 0 (stream:car α))
        (error "series:exp" "α₀ not zero"))
       (else (letdelay ((Y (series:+ stream:1 (series:∫
                                               (series:× Y (series:∂ α))))))
              Y)))))

    (define series:log₂
     (letrec ((summands (Λ (n)
                         (stream:cons (⁻¹ n) ((stream:map -) (summands (add1 n)))))))
      ((stream:scan +) (summands 1))))

    (define series:Euler-transform
     (Λ (s)
      (let* ((n-1 (stream:car s))
             (n (stream:cadr s))
             (n+1 (stream:caddr s))
             (square (lambda (x) (* x x)))
             (t (- n+1 (/ (square (- n+1 n)) (+ n+1 n-1 (* -2 n))))))
       (stream:cons t (series:Euler-transform (stream:cdr s))))))

    (define series:integrator/∞
     (lambda (init dt)
      (lambda (s)
       (letdelay ((I (stream:cons init (series:+ ((series:* dt) s) I))))
        I))))

    (define series:integrator
     (lambda (init dt)
      (Λ (s)
       (stream:cons init (stream:dest/car+cdr (s ∅)
                          ((scar scdr) ((series:integrator (+ (* dt scar) init) dt) scdr)))))))

    (define series:ode-solver-1st
     (lambda (integral)
      (lambda (f)
       (letdelay ((y (integral dy))
                  (dy ((stream:map f) y)))
        y))))

    (define series:ode-solver-2nd
     (lambda (integral-1st integral-2nd)
      (lambda (f)
       (letdelay ((y (integral-1st dy))
                  (dy (integral-2nd ddy))
                  (ddy ((stream:zip-with f) y dy)))
        y))))




    (define Pythagorean-triples
     (lambda (n)
      (let ((F (lambda (triple)
                (equal?
                 (+ (expt (car triple) n) (expt (cadr triple) n))
                 (expt (caddr triple) n))))
            (nats (series:from 1))
            (slow-enumeration stream:enumerate-upper)
            (fast-enumeration (stream:enumerate-weighted
                               (lambda (i j #!optional (k 0))
                                ((○ abs -)
                                 (+ (expt i n) (expt j n))
                                 (expt k n))))
            ))
       ((stream:filter F) (fast-enumeration nats nats nats)))))

    (define random-numbers
     (let ((rand-update (lambda (u) (pseudo-random-integer 1000))))
      (lambda (init)
       (letdelay ((R (stream:cons init ((stream:map rand-update) R))))
        R))))

    (define series:Montecarlo
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
                     ((stream:zip-with cons) dcdr (riordan-array (series:× d h) h)))))))

    (define formalvar-series
     (Λ (n)
      (cond
       ((zero? n) stream:1)
       (else (stream:cons 0 (formalvar-series (sub1 n)))))))

    (define series:◇
     (Λ (α)
      (stream:dest/car+cdr (α ∅)
       ((α₀ αs) (cond
                 ((equal? α₀ 0) (letdelay ((R (stream:cons 0 (series:⁻¹ (series:○ αs R)))))
                                 R))
                 (else (error "series:◇" "α₀ not zero" α₀)))))))

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

 (define series:range/0-9 (series:range 0 10))

 (define numbers/nats
  (series:from 0))

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
   (stream:cons 2 ((stream:filter prime?) (series:from 3)))))

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

 (define radix-expansion
  (Λ (num den radix)
   (let-values (((q r) (quotient&remainder (* num radix) den)))
    (stream:cons q (radix-expansion r den radix)))))

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
     (taylor/cosine (series:+ stream:1 ((series:* -1) (series:∫ taylor/sine))))
     (taylor/sine (series:∫ taylor/cosine)))

    (define-delay taylor/exponential
     (series:+ stream:1 (series:∫ taylor/exponential)))

    (define-delay taylor/catalan
     (stream:cons 1 (series:× taylor/catalan taylor/catalan)))

    (define-delay taylor/fibonacci
     (let ((t (formalvar-series 1)))
      (stream:cons 1 (series:+ 
                      taylor/fibonacci 
                      (series:× t taylor/fibonacci)))))

    (define taylor/π
     (letrec ((summands (Λ (n)
                         (stream:cons (⁻¹ n) ((stream:map -) (summands (+ n 2)))))))
      ((series:* 4) ((stream:scan +) (summands 1)))))

 )
