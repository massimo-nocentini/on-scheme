
(use test)
(use numbers)

    (define stream-null? (compose null? force))
    (define stream-car (compose car force)) 
    (define stream-cdr (compose cdr force))
    (define stream-cadr (compose stream-car stream-cdr))
    (define stream-caddr (compose stream-car stream-cdr stream-cdr))

    (define-syntax stream-cons
     (syntax-rules ()
      ((stream-cons a d) (delay-force (cons a d)))))

    (define-syntax stream-dest/car+cdr 
     (syntax-rules ()
      ((stream-dest/car+cdr ((s (a d)) ...) body ...) 
       (delay-force 
        (let ((a (stream-car s)) ...
              (d (stream-cdr s)) ...)
         body ...)))))

    (define empty-stream (delay-force '()))

    (define stream-ref
     (lambda (n s)
      (cond
       ((zero? n) (stream-car s))
       (else (stream-ref (sub1 n) (stream-cdr s))))))

    (define stream-foldr
     (lambda (func init)
      (letrec ((F (lambda (s)
                   (delay-force
                    (cond
                     ((stream-null? s) init)
                     (else (func (stream-car s) (F (stream-cdr s)))))))))
       F)))

    (define stream-map
     (lambda (func)
      (letrec ((M (lambda (s)
                   (delay-force
                    (cond 
                     ((stream-null? s) empty-stream)
                     (else (stream-cons (func (stream-car s)) (M (stream-cdr s)))))))))
       M)))

      (define stream-map2
       (lambda (func)
        (stream-foldr (lambda (a d) (stream-cons (func a) d)) empty-stream)))

    (define stream-range 
     (lambda (low high)
      (letrec ((R (lambda (low)
                   (delay-force
                    (cond
                     ((>= low high) empty-stream)
                     (else (stream-cons low (R (add1 low)))))))))
       (R low))))

    (define stream-filter 
     (lambda (pred?)
      (letrec ((F (lambda (s)
                   (delay-force
                    (cond 
                     ((stream-null? s) empty-stream)
                     (else (stream-dest/car+cdr ((s (a d)))
                            (cond
                             ((pred? a) (stream-cons a (F d)))
                             (else (F d))))))))))
       F)))

    (define stream-take
     (lambda (n)
      (lambda (s)
       (letrec ((T (lambda (i r)
                    (delay-force
                     (cond
                      ((or (> i n) (stream-null? r)) empty-stream)
                      (else (stream-dest/car+cdr ((r (rcar rcdr)))
                             (stream-cons rcar (T (add1 i) rcdr)))))))))
        (T 1 s)))))

    (define stream->list
     (compose force (stream-foldr (lambda (a d) (cons a (force d))) '())))

    (define stream->list2
     (lambda (s)
      (cond
       ((stream-null? s) '())
       (else (cons (stream-car s) (stream->list (stream-cdr s)))))))

    (define stream-from
     (lambda (n)
      (stream-cons n (stream-from (add1 n))))) ; no `delay-force` here?

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
      (stream-dest/car+cdr ((s (prime primes)))
       (stream-cons prime (eratosthenes 
                           ((stream-filter (compose not (divisable-by? prime))) 
                            primes))))))

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
     (let ((shift-first (compose stream-cdr car)))
      (lambda (op)
       (lambda (s)
        (letrec ((C (lambda streams
                     (stream-cons
                      (apply op (map stream-car streams))
                      (apply C (cons (shift-first streams) streams))))))
         (apply C (list s)))))))

    (define list->stream
     (lambda (l)
      (delay-force
       (cond 
        ((null? l) empty-stream)
        (else (stream-cons (car l) (list->stream (cdr l))))))))

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

    (define stream-merge
     (lambda (pred?)
      (letrec ((M (lambda (s r) ; binary merge strategy
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
       (lambda streams
        (foldr M empty-stream streams)))))

    (define radix-expand 
     (lambda (num den radix)
      (stream-cons
       (quotient (* num radix) den)
       (radix-expand (remainder (* num radix) den) den radix))))

    (define integrate-series
     (lambda (s)
      (letrec ((I (lambda (s n)
                   (stream-dest/car+cdr ((s (scar scdr))
                                         (n (ncar ncdr)))
                    (stream-cons (/ scar ncar) (I scdr ncdr))))))
      (I s (stream-from 1)))))

    (define add-series (stream-zip-with +))

    (define scale-series 
     (lambda (a)
      (lambda (s)
       ((stream-zip-with *) (stream-repeat a) s))))

    (define mul-series
     (lambda (s r)
      (stream-dest/car+cdr ((s (scar scdr))
                            (r (rcar rcdr)))
       (stream-cons (* scar rcar) (add-series 
                                   ((scale-series scar) rcdr) 
                                   (mul-series scdr r))))))

    (define inverse-series
     (lambda (s)
      (stream-dest/car+cdr ((s (scar scdr)))
       (letrec ((I (stream-cons (/ 1 scar) ((scale-series (/ -1 scar)) 
                                            (mul-series scdr I)))))
        I))))

    (define division-series
     (lambda (num denum)
      (mul-series num (inverse-series denum))))

    (define integral-series&co
     (lambda (init dt)
      (lambda (s)
       (letrec ((I (stream-cons init (add-series ((scale-series dt) s) I))))
        I))))

    (define integral-series&rec
     (lambda (init dt)
      (lambda (s)
       (stream-cons init (cond
                          ((stream-null? s) empty-stream)
                          (else ((integral-series&rec (+ (* dt (stream-car s)) init) dt)
                                 (stream-cdr s))))))))

    (define ode-solve-1st
     (lambda (integral)
      (lambda (f)
       (letrec ((y (integral (delay-force dy)))
                (dy ((stream-map f) (delay-force y))))
        y))))

    (define ode-solve-2nd
     (lambda (integral-1st integral-2nd)
      (lambda (f)
       (letrec ((y (integral-1st (delay-force dy)))
                (dy (integral-2nd (delay-force ddy)))
                (ddy ((stream-zip-with f) (delay-force y) (delay-force dy))))
        y))))

    (define stream-sqrt
     (lambda (n)
      (letrec ((average (lambda args (/ (foldr + 0 args) (length args))))
               (improve (lambda (guess) (average guess (/ n guess))))
               (guesses (stream-cons 1 ((stream-map improve) guesses))))
       guesses)))

    (define pi-series
     (letrec ((summands (lambda (n) 
                         (stream-cons (/ 1 n) ((stream-map -) (summands (+ n 2)))))))
      ((scale-series 4) ((stream-cumulatives +) (summands 1)))))

    (define log2-series
     (letrec ((summands (lambda (n) 
                         (stream-cons (/ 1 n) ((stream-map -) (summands (add1 n)))))))
      ((stream-cumulatives +) (summands 1))))

    (define euler-transform
     (lambda (s)
      (let ((n-1 (stream-car s))
            (n (stream-cadr s))
            (n+1 (stream-caddr s))
            (square (lambda (x) (* x x))))
       (stream-cons 
        (- n+1 (/ (square (- n+1 n)) (+ n+1 n-1 (* -2 n))))
        (euler-transform (stream-cdr s))))))

    (define stream-tableau
     (lambda (transform)
      (letrec ((T (lambda (s)
                   (stream-cons s ((compose T transform) s)))))
       (lambda (s)
        ((stream-map stream-car) (T s))))))

    (define stream-enumerate
     (letrec ((tuple (compose flatten list))
              (B (lambda (s r)
                  (delay-force
                   (cond
                    ((stream-null? s) r)
                    ((stream-null? r) s)
                    (else (stream-dest/car+cdr ((s (scar scdr)) (r (rcar rcdr)))
                           (stream-cons 
                            (tuple scar rcar)
                            (interleave 
                             ((stream-map (lambda (ri) (tuple scar ri))) rcdr)
                             (B scdr rcdr)))))))))
              (interleave (lambda (s r)
                           (delay-force
                            (cond
                             ((stream-null? s) r)
                             (else (stream-dest/car+cdr ((s (scar scdr)))
                                    (stream-cons scar (interleave r scdr)))))))))
      (lambda streams
       (foldr B empty-stream streams))))

    (define stream-enumerate-all
     (letrec ((tuple (compose flatten list))
              (B (lambda (s r)
                  (delay-force
                   (cond
                    ((stream-null? s) r)
                    ((stream-null? r) s)
                    (else (stream-dest/car+cdr ((s (scar scdr)) (r (rcar rcdr)))
                           (stream-cons 
                            (tuple scar rcar)
                            (interleave
                             (interleave 
                              ((stream-map (lambda (ri) (tuple scar ri))) rcdr)
                              (B scdr rcdr))
                             ((stream-map (lambda (si) (tuple si rcar))) scdr)))))))))
              (interleave (lambda (s r)
                           (delay-force
                            (cond
                             ((stream-null? s) r)
                             (else (stream-dest/car+cdr ((s (scar scdr)))
                                    (stream-cons scar (interleave r scdr)))))))))
      (lambda streams
       (foldr B empty-stream streams))))

    (define stream-enumerate-weighted
     (lambda (weight)
      (letrec ((make-tuple (compose flatten list))
               (B (lambda (s r)
                   (delay-force
                    (cond
                     ((stream-null? s) r)
                     ((stream-null? r) s)
                     (else (stream-dest/car+cdr ((s (scar scdr)) (r (rcar rcdr)))
                            (stream-cons 
                             (make-tuple scar rcar) 
                             (interleave 
                              ((stream-map (lambda (ri) (make-tuple scar ri))) rcdr)
                              (B scdr rcdr)))))))))
               (interleave (lambda (s r)
                            (delay-force
                             (cond
                              ((stream-null? s) r)
                              ((stream-null? r) s)
                              (else
                               (let ((scar (stream-car s))
                                     (rcar (stream-car r)))
                                (cond
                                 ((< (apply weight scar) (apply weight rcar)) 
                                  (stream-cons scar (interleave (stream-cdr s) r)))
                                 (else (stream-cons rcar (interleave s (stream-cdr r))))))))))))
    (lambda streams
     (foldr B empty-stream streams)))))

    (define stream-take-while
     (lambda (pred?)
      (letrec ((stop? (compose not pred?))
               (W (lambda (s)
                   (delay-force
                    (cond
                     ((stream-null? s) empty-stream)
                     (else (let ((scar (stream-car s)))
                            (cond
                             ((stop? scar) (stream-cons scar empty-stream))
                             (else (stream-cons scar (W (stream-cdr s))))))))))))
       W)))

    (define Pythagorean-triples
     (lambda (n)
      (let ((F (lambda (triple) 
                (equal? 
                 (+ (expt (car triple) n) (expt (cadr triple) n))
                 (expt (caddr triple) n))))
            (nats (stream-from 1)))
       ((stream-filter F) (stream-enumerate nats nats nats)))))

;________________________________________________________________________________

    (define run-tests
     (lambda ()
      (let* (
             (take (lambda (n) (compose stream->list (stream-take n)))) ; fetching function
             (first-10-nats '(0 1 2 3 4 5 6 7 8 9))
             (x (stream-range 0 10))
             (seq ((stream-map accum) (stream-range 1 20)))
             (evens ((stream-filter even?) seq))
             (multiples-of-5 ((stream-filter (divisable-by? 5)) seq))
             (nats (stream-from 0))
             (nats>0 (stream-cdr (stream-from 0)))
             (no-7s ((stream-filter (compose not (divisable-by? 7))) nats))
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
        ((take 19) seq))
       (test 120 (stream-ref 6 evens))
       (test '(3 4 5 6 7) ((take 5) (stream-from 3)))
       (test '(10 15 45 55 105 120 190) (stream->list multiples-of-5))
       (test '(0 1 2 3 4 5 6 7 8 9) ((take 10) nats))
       (test 117 (stream-ref 100 no-7s))
    (test first-10-nats ((compose stream->list list->stream) first-10-nats)) ; therefore `identity`
    (test '(0 1 1 2 3 5 8 13 21 34) ((take 10) fibs))
    (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
     ((take 100) primes))
    (test '(1 1 1 1 1 1 1 1 1 1) ((take 10) ones))
    (test '(1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) ; 1/7 \sim 0.142..
     ((take 20) frac/1-7))
    (test '(18 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5) ; 13/7 \sim 18.571.. wrong usage!!
     ((take 20) frac/13-7))
    (test '(3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) ; 3/8 = 0.375
     ((take 20) frac/3-8))
    (letrec ((nats (stream-cons 0 ((stream-zip-with +) ones nats)))
             (fibs (stream-cons 0 (stream-cons 1 ((stream-zip-with +) fibs (stream-cdr fibs)))))
             (squares (stream-cons 1 ((stream-zip-with *) squares (stream-repeat 2))))
             (primes (stream-cons 2 ((stream-filter (make-prime? primes)) (stream-from 3))))
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
     (test '(0 1 2 3 4 5 6 7 8 9) ((take 10) nats))
     (test '(0 1 1 2 3 5 8 13 21 34) ((take 10) fibs))
     (test '(1 2 4 8 16 32 64 128 256 512) ((take 10) squares))
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
      ((take 100) primes))
     (test '(1 2 4 8 16 32 64 128 256 512) ((take 10) doubles))
     (test '(1 1 2 6 24 120 720 5040 40320 362880) ((take 10) factorials))
    (let
     ((nats-cumulatives ((stream-cumulatives +) nats))
      (fibs-cumulatives ((stream-cumulatives +) fibs))
      (fibs-produlatives ((stream-cumulatives *) (stream-cdr fibs))))
     (test '(0 1 3 6 10 15 21 28 36 45) ((take 10) nats-cumulatives))
     (test '(0 1 2 4 7 12 20 33 54 88) ((take 10) fibs-cumulatives)) ; https://oeis.org/A000071
     (test-assert (equal?
                   ((take 10) fibs-cumulatives)
                   ((take 10) ((stream-zip-with -) 
                                    ((compose stream-cdr stream-cdr) fibs) 
                                    (stream-repeat 1)))))
     (test '(1 1 2 6 30 240 3120 65520 2227680 122522400) ((take 10) fibs-produlatives)))
    (test '(1 2 3 4 5 6 6 8 9 10 10 12 12 12 15 15 16 18 18 18 20 20 20 24 24 24 24 25 27 30 30 30 30 30 30 32 36 36 36 36 36 36 40 40 40 40 45 45 45 48) 
     ((take 50) S))
    (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880) ((take 10) exponential-series))
    (test '(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0) ((take 10) cosine-series))
    (test '(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880) ((take 10) sine-series))
    (test '(1 2 3 4 5 6 7 8 9 10) ((take 10) (mul-series ones ones)))
    (test '(0 1 3 6 10 15 21 28 36 45) ((take 10) (mul-series nats ones)))
    (let ((sine2+cosine2 (add-series
                          (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series)))
          (fibs>0 (stream-cdr fibs))
          (tangent-series (division-series sine-series cosine-series))
         )
     (test '(1 0 0 0 0 0 0 0 0 0) ((take 10) sine2+cosine2))
     (test '(1 -1 0 0 0 0 0 0 0 0) ((take 10) (inverse-series ones)))
     (test '(1 -2 1 0 0 0 0 0 0 0) ((take 10) (inverse-series nats>0)))
     (test '(1 0 0 0 0 0 0 0 0 0) ((take 10) (mul-series nats>0 (inverse-series nats>0))))
     (test '(1 -1 -1 0 0 0 0 0 0 0) ((take 10) (inverse-series (stream-cdr fibs))))
     (test '(1 0 0 0 0 0 0 0 0 0) ((take 10) (mul-series fibs>0 (inverse-series fibs>0))))
     (let ((non-unary-series (stream-cons 4 nats>0)))
      (test '(1/4 -1/16 -7/64 -33/256 -119/1024 -305/4096 -231/16384 3263/65536 26537/262144 133551/1048576) 
       ((take 10) (inverse-series non-unary-series)))
      (test '(1 0 0 0 0 0 0 0 0 0) 
       ((take 10) (mul-series non-unary-series (inverse-series non-unary-series)))))
     (test '(0 1 0 1/3 0 2/15 0 17/315 0 62/2835) ((take 10) tangent-series)))
    (test '(1 3/2 17/12 577/408 665857/470832) ((take 5) (stream-sqrt 2)))
    (test '(4 8/3 52/15 304/105 1052/315 10312/3465 147916/45045 135904/45045 2490548/765765 44257352/14549535) 
     ((take 10) pi-series))
    (test '(19/6 47/15 1321/420 989/315 21779/6930 141481/45045 1132277/360360 801821/255255 91424611/29099070 45706007/14549535) 
     ((take 10) (euler-transform pi-series)))
    (test '(4 19/6 597/190 73480501/23389520 908158408158535624217/289075793976001466280 6122842329181964122242157261623055408836895066390212805633550889340420878203921614979/1948961244684053510230839929806362227580597549551747444462336644887607671758516827680) 
     ((take 6) ((stream-tableau euler-transform) pi-series)))
    (test '(1 1/2 5/6 7/12 47/60 37/60 319/420 533/840 1879/2520 1627/2520) 
     ((take 10) log2-series))
    (test '(7/10 29/42 25/36 457/660 541/780 97/140 9901/14280 33181/47880 1747/2520 441871/637560)
     ((take 10) (euler-transform log2-series)))
    (test '(1 7/10 165/238 380522285/548976276 755849325680052062216639661/1090460049411856348776491380 318738655178511632543822227346530350595387994474669640697143248267438214457834012964733985868157066661175569469393/459842677096914359400941379802880332404679211833600390612039625007123278498884893986945137648853585966630779010940)
     ((take 6) ((stream-tableau euler-transform) log2-series)))

    (test '(0 1 2 3 4 5 6 7 8 9) ((take 10) (stream-enumerate nats)))
    (test '((0 0) (0 1) (1 1) (0 2) (1 2) (0 3) (2 2) (0 4) (1 3) (0 5)) 
     ((take 10) (stream-enumerate nats nats)))
    (test '((0 0 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 2) (2 1 1) (0 1 2) (1 0 2) (0 0 3)) 
     ((take 10) (stream-enumerate nats nats nats)))
    (test '((0 0 0 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (2 1 0 1) (0 1 1 1) (1 0 1 1) (0 0 0 2)) 
     ((take 10) (stream-enumerate nats nats nats nats)))
    (test '((0 0) (0 1) (1 0) (1 1) (2 0) (0 2) (3 0) (1 2) (4 0) (0 3)) 
     ((take 10) (stream-enumerate-all nats nats)))
    (test 397
     ((compose length stream->list (stream-take-while
                                    (lambda (pair)
                                     ((compose not equal?) pair '(1 100)))))
      (stream-enumerate nats nats)))
    (test 1559
     ((compose length stream->list (stream-take-while
                                    (lambda (pair)
                                     (not (equal? pair '(3 100))))))
      (stream-enumerate nats nats)))
    #;(test '((3 4 5) (4 3 5) (6 8 10) (8 6 10) (5 12 13)) 
     ((take 5) (Pythagorean-triples 2)))
    (test '((0 0) (0 1) (1 1) (0 2) (1 2) (0 3) (2 2) (1 3) (0 4) (2 3)) 
     ((take 10) ((stream-enumerate-weighted +) nats nats)))
       
    (let* ((F (lambda (x) 
               (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5)))))
           (O (lambda (i j)
               (+ (* i 2) (* j 3) (* 5 i j))))
           (pairs ((stream-enumerate-weighted O)
                   ((stream-filter F) nats>0)
                   ((stream-filter F) nats>0))))
    (test '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13))
     ((take 20) pairs))
    (test '(10 58 90 106 138 154 186 234 250 280 298 330 346 378 394 426 432 474 490 508)
     ((take 20) ((stream-map (lambda (p) (apply O p))) pairs))))

    (let* ((W (lambda (i j)
               (+ (expt i 3) (expt j 3))))
           (sums-of-cubes ((stream-enumerate-weighted W) nats>0 nats>0)))
           ;(sums-of-cubes (stream-enumerate nats>0 nats>0))) ; no answer
     (letrec ((F (lambda (s)
                  (let ((c (stream-car s)) 
                        (o (stream-cadr s)))
                   (delay-force
                    (cond
                     ((equal? (apply W c) (apply W o)) 
                      (stream-cons (list (apply W c) c o) (F (stream-cdr s))))
                     (else (F (stream-cdr s)))))))))
      (test '((1729 (9 10) (1 12)) 
              (4104 (9 15) (2 16)) 
              (13832 (18 20) (2 24)) 
              (20683 (19 24) (10 27)) 
              (32832 (18 30) (4 32)) 
              (39312 (15 33) (2 34)) 
              (40033 (16 33) (9 34)) 
              (46683 (27 30) (3 36)) 
              (64232 (26 36) (17 39)) 
              (65728 (31 33) (12 40)))
       ((take 10) (F sums-of-cubes)))
     ))

    (let* ((W (lambda (i j)
               (+ (expt i 2) (expt j 2))))
           (sums-of-squares ((stream-enumerate-weighted W) nats>0 nats>0)))
           ;(sums-of-squares (stream-enumerate nats>0 nats>0))) ; no answer
     (letrec ((F (lambda (s)
                  (let ((c (stream-car s)) 
                        (o (stream-cadr s))
                        (p (stream-caddr s)))
                   (delay-force
                    (cond
                     ((and
                       (equal? (apply W c) (apply W o))
                       (equal? (apply W o) (apply W p)))
                      (stream-cons (list (apply W c) c o p) (F (stream-cdr s))))
                     (else (F (stream-cdr s)))))))))
      (test '((325 (10 15) (6 17) (1 18)) 
              (425 (13 16) (8 19) (5 20)) 
              (650 (17 19) (11 23) (5 25)) 
              (725 (14 23) (10 25) (7 26)) 
              (845 (19 22) (13 26) (2 29)) 
              (850 (15 25) (11 27) (3 29)) 
              (925 (21 22) (14 27) (5 30)) 
              (1025 (20 25) (8 31) (1 32)) 
              (1105 (23 24) (12 31) (9 32)) 
              (1105 (12 31) (9 32) (4 33)))
       ((take 10) (F sums-of-squares)))
     ))

    (let ((expected '(0 1/2 3/2 3 5 15/2 21/2 14 18 45/2)))
     (test expected ((take 10) ((integral-series&rec 0 1/2) nats>0)))
     (test expected ((take 10) ((integral-series&co 0 1/2) nats>0))))

    (let ((expected 2716923932235892457383088121947577188964315018836572803722354774868894945523768158997885697298661429053421034015406256924859461187617653889457753593083386399572063538500432650176144488046171044844121805479607648086607018742077798375087855857012278053105042704758822511824867218226931719410407150364389665913091822576819072281835735365786202176167228686198158460724641052407506305826211156964723064441295969498221919251479211700941935114755531972677360157561485144237786816579422141378066423317811515462669946309306263409027388915931082226854264858661420878279983534424128672461206356847463821364630504359665171573635397346037274752410368174877433941234543153511100471651472869116068528478976916600585383497180172395573924789047989563714318957536493108041591460911612078698461739084741934442448701416575483263891529095158013233115648534154086009312190489168546024398834243847135102411661996020129557921444666343641039137906807591342742464200991933722791531063202677650581946360422027765645970182463780273161113009717582155489902677095053354207944772439271656447869921825959042801322775729022491402012084605367784456090892987682547811360481731795980637847551788259384243997341190753089343387201753821360405430310320564488741142120089460368986590136324737459372963666586532443570474179352656517635333744783401695951969936296323256525034685525470426185224036844803487442831639483152362831735350269624668701702424450940840884555271325190876102665277858154695092765613639718577127438538649414492678358762110235621776218781360881010654696273264706319088453035858355052988808507775439561385232652305316287705653436727647681405618323757201022946801118770148072424021385261829594248369890171583993147934044232792517118743393217276416179842097554494269012251329134783596037733973478306188255291484352384699871420472711423079586319041837563678498472779422282261024744394844558738378027105699691260086532632930941478779680554645850778168703661423819000515895232903243738763481571999080702098369316199601942246247887808385073821861517636839926907458184604648942036355256683219218129910422822177336785268627274482037476294341444562207197209503659518266210432791078248321015453218019586608696207295299183111963158564162419152742807437346241667671688466998244424726765837682151606230638111654756595917019206453978024157097042546937345673337179165242325399648121877178987723999503839197328183925340949191821443698275476295245249466361817367207248089144718808572152781037112209285944844021186534832159964297181970584453756163204297111185823467744743465840230098261424789313315093951766314459027947176701489215746884363426961577348384651887153140609616362927338107686794499974902581579897076172716541504294334300741444106749994715713419630688719451362658288812132056854807330827050505064714442618243101018812153563795539024370219967801515099970721926240625418512417940854760415566229746248973756297569452302821563467574313259066016089521122779204844875998864114930516063910324359331903843040069467324167490917499501000001/1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     ;(test expected (stream-ref 1000 ((ode-solve-1st (integral-series&rec 1 1/1000)) (lambda (y) y))))
     (test expected (stream-ref 1000 ((ode-solve-1st (integral-series&co 1 1/1000)) (lambda (y) y)))))
    
    #;(test '()
     (stream-ref 1000 ((ode-solve-2nd
                        (integral-series&co 2 1/1000) 
                        (integral-series&co -1 1/1000)) 
                       (lambda (y dy) (+ (* -9 y) dy)))))


    (let ((integral-v (integral-series&co 10 1/100))    
          (integral-i (integral-series&co 0 1/100))
          (func-i (lambda (i v) (+ (* -1 i) v)))
          (func-v (lambda (v i) (* -5 i))))
     (letrec ((v (integral-v (delay-force dv)))
              (i (integral-i (delay-force di)))
              (di ((stream-zip-with func-i) (delay-force i) (delay-force v)))
              (dv ((stream-zip-with func-v) (delay-force v) (delay-force i))))
      (test '()
       (map (lambda (p)
             (list (exact->inexact (car p)) (exact->inexact (cadr p))))
        ((take 500) ((stream-zip-with list) v i))))))



    ))
    ))

(run-tests)

;(test-exit)
