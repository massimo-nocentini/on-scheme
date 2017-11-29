
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

    (define stream-map
     (lambda (func)
      (letrec ((M (lambda (s)
                   (delay-force
                    (cond 
                     ((stream-null? s) empty-stream)
                     (else (stream-dest/car+cdr ((s (a d)))
                            (stream-cons (func a) (M d)))))))))
       M)))

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

    (define stream-foldr
     (lambda (func init)
      (letrec ((F (lambda (s)
                   (delay-force
                    (cond
                     ((stream-null? s) init)
                     (else (func (stream-car s) (F (stream-cdr s)))))))))
       F)))

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

    (define integral-series
     (lambda (init dt)
      (lambda (s)
       (letrec ((I (stream-cons init (add-series ((scale-series dt) (force s)) I))))
        I))))

    (define solve-diffeq
     (lambda (integral)
      (lambda (f)
       (letrec ( 
                 (y (integral (delay dy)))
                 (dy (stream-cons 0 ((stream-map f) y)) )
                 )
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

    (test '(0 1/2 3/2 3 5 15/2 21/2 14 18 45/2)
      ((take 10) ((integral-series 0 1/2) nats>0)))

    (test 2711510385063038184554582148025009427385145263359209457911835653221988864985065203856631678002232620534402061080494155033762462228923969700210224770791600005017667658552232805689051399234406246976720512041593001913227333944632316514825292178834480670914226933631181326179369443993712335090413341505973352373034623018114166459796277008059130410338874225517209113673093844493261493781889829680001888988051902981217140357447890238829068720653119504910093628744215647204665486215271368764890205992214158584100789379727836843458618055469396308281602497086484827205166156369130622868636103816355581577008071766425607421042367710815211492681377204744801136195752145388037905551222565006950338865474972439785427276517948920160083802494701850196289450930993500989786756629227623450997547504125743786944235336378989442868350866422076559961360245633476373744404078782269798871625909235692492153936337380781976852321135521028864887029666358636815103573661806597708801629250378129011107381444723549770115579026900117308415223069114430139561124911515279615908502358741619826114337569206590115333590236702954804045945737785248646523435453730999817283302637387981477365192509485754324325698046108815373543501964674969641254207607061371614585594212500879711236238275528886631667831326177659561704077680259694658809712578952476729742713681847788418343344912065889264690855110528316571031880257213340891027004183749607537656227436349710751525161000079099344572199316883713483291190920886855527886975531525481248000250001/1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
     (stream-ref 1000 ((solve-diffeq (integral-series 1 1/1000)) (lambda (y) y))))

    ))
    ))

(run-tests)

;(test-exit)
