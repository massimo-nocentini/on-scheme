


(import chicken scheme)

(use srfi-1 srfi-13)
(use test numbers)

(use commons streams series)

    (test '(a b c d) ((compose stream:->list list->stream) '(a b c d))) ; therefore `identity`

     (test 0 ((stream:ref 0) series:range/0-9))
     (test 5 ((stream:ref 5) series:range/0-9))
     (test 9 ((stream:ref 9) series:range/0-9))
     (test-error ((stream:ref 10) series:range/0-9))
     (test-error ((stream:ref 101) series:range/0-9))
     (test '(3 4 5 6 7) ((list○take 5) (series:from 3)))
     (test '(1 1 1 1 1 1 1 1 1 1) ((list○take 10) stream:1s))
     (test '(0 1 2 3 4 5 6 7 8 9) ((list○take 10) numbers/nats))
     (test '(0 1 2 3 4 5 6 7 8 9) ((list○take 10) ((stream:append-map list) numbers/nats)))
     (test  117 ((stream:ref 100) ((not-multiples-of 7) numbers/nats)))
     (test '(0 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190) ((list○take 20) numbers/triangular))
     (test '(0 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190) ((list○take 20) numbers/triangular/∞))
     (test 78 ((stream:ref 6) (evens numbers/triangular)))
     ;(✓ (stream:ref (expt 10 6)) 0 numbers/fibonacci)  ; Francesco Frosini's stress test!!
     ;(✓ (stream:ref (expt 10 6)) 0 numbers/fibs/∞)     ; Francesco Frosini's stress test!!

     (test '(0 10 15 45 55 105 120 190) ((list○take 8) ((multiples-of 5) numbers/triangular)))

     (test '(0 1 1 2 3 5 8 13 21 34) ((list○take 10) numbers/fibonacci))
     (test '(0 1 2 4 7 12 20 33 54 88 143 232 376 609 986 1596 2583 4180 6764 10945) ((list○take 20) numbers/fibonacci-triangular))
     (test ((list○take 10) ((stream:map sub1) numbers/fibonacci>0)) ((list○take 10) (stream:cons 0 numbers/fibonacci-triangular)))

     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
      ((list○take 100) primes/eratosthenes))

     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
      ((list○take 100) primes/∞))

     (test '(1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) ; 1/7 \sim 0.142..
      ((list○take 20) (radix-expansion 1 7 10)))
     (test '(18 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5) ; 13/7 \sim 18.571.. wrong usage!!
      ((list○take 20) (radix-expansion 13 7 10)))
     (test '(3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) ; 3/8 = 0.375
      ((list○take 20) (radix-expansion 3 8 10)))

     (test '((0 0 0) (1 1 1) (2 1 3) (3 2 6) (4 3 10) (5 5 15) (6 8 21) (7 13 28) (8 21 36) (9 34 45))
      ((list○take 10) (stream:zip numbers/nats numbers/fibonacci numbers/triangular)))

    (test #f ((stream:foldr
               (lambda (a acc) (and a (stream:car acc)))
               (lambda () (list (/ 1 0))))
              (:⁺ #t #t #t #f #t))) ; #f prevents (/ 1 0) from being evaluated

    (test #f ((stream:foldr
               (lambda (a acc) (and a (stream:car acc)))
               (lambda () 'useless))
              (:⁺ #t #t #t #f series:0))) ; #f prevents enumerating zeros forever

    (test '(0 1 2 0 1 2 0 1 2 0) ((list○take 10) (stream:§ 
                                                  (stream:repeat 0) 
                                                  (stream:repeat 1) 
                                                  (stream:repeat 2)))) 

     (let ((next (stream:iterator numbers/nats)))
      (test '(0 1 2 3) (collect-values (lambda () (values (next) (next) (next) (next))))))

    (let* ((C (○ eval car))
           (L (lambda (p q)
               `((* ,(C p) ,(C q)) ,(list (cadr p) (cadr q))))))
     (letdelay ((α (stream:cons '(1 i) ((stream:merge (lambda (p q) (< (C p) (C q))))
                                        ((stream:zip-with L) α (stream:repeat '(2 a)))
                                        ((stream:zip-with L) α (stream:repeat '(3 b)))
                                        ((stream:zip-with L) α (stream:repeat '(5 c)))))))
      (test '((1 i)
              ((* 1 2) (i a))
              ((* 1 3) (i b))
              ((* 2 2) ((i a) a))
              ((* 1 5) (i c))
              ((* 2 3) ((i a) b))
              ((* 3 2) ((i b) a))
              ((* 4 2) (((i a) a) a))
              ((* 3 3) ((i b) b))
              ((* 2 5) ((i a) c))
              ((* 5 2) ((i c) a))
              ((* 4 3) (((i a) a) b))
              ((* 6 2) (((i a) b) a))
              ((* 6 2) (((i b) a) a))
              ((* 3 5) ((i b) c))
              ((* 5 3) ((i c) b))
              ((* 8 2) ((((i a) a) a) a))
              ((* 6 3) (((i a) b) b))
              ((* 6 3) (((i b) a) b))
              ((* 9 2) (((i b) b) a))
              ((* 4 5) (((i a) a) c))
              ((* 10 2) (((i a) c) a))
              ((* 10 2) (((i c) a) a))
              ((* 8 3) ((((i a) a) a) b))
              ((* 12 2) ((((i a) a) b) a))
              ((* 12 2) ((((i a) b) a) a))
              ((* 12 2) ((((i b) a) a) a))
              ((* 5 5) ((i c) c))
              ((* 9 3) (((i b) b) b))
              ((* 6 5) (((i a) b) c))
              ((* 6 5) (((i b) a) c))
              ((* 10 3) (((i a) c) b))
              ((* 10 3) (((i c) a) b))
              ((* 15 2) (((i b) c) a))
              ((* 15 2) (((i c) b) a))
              ((* 16 2) (((((i a) a) a) a) a))
              ((* 12 3) ((((i a) a) b) b))
              ((* 12 3) ((((i a) b) a) b))
              ((* 12 3) ((((i b) a) a) b))
              ((* 18 2) ((((i a) b) b) a))
              ((* 18 2) ((((i b) a) b) a))
              ((* 18 2) ((((i b) b) a) a))
              ((* 8 5) ((((i a) a) a) c))
              ((* 20 2) ((((i a) a) c) a))
              ((* 20 2) ((((i a) c) a) a))
              ((* 20 2) ((((i c) a) a) a))
              ((* 9 5) (((i b) b) c))
              ((* 15 3) (((i b) c) b))
              ((* 15 3) (((i c) b) b))
              ((* 16 3) (((((i a) a) a) a) b)))
      ((list○take 50) α))))

     (test '(0 1 2 3 4 5 6 7 8 9) ((list○take 10) numbers/nats/∞))
     (test '(0 1 1 2 3 5 8 13 21 34) ((list○take 10) numbers/fibs/∞))
     (test '(2 1 3 4 7 11 18 29 47 76) ((list○take 10) numbers/lucas/∞))
     (test '(1 2 4 8 16 32 64 128 256 512) ((list○take 10) numbers/powers-of-2))
     (test '(1 2 4 8 16 32 64 128 256 512) ((list○take 10) numbers/powers-of-2/∞))
     (test '(1 3 9 27 81 243 729 2187 6561 19683) ((list○take 10) numbers/powers-of-3))
     (test '(1 3 9 27 81 243 729 2187 6561 19683) ((list○take 10) numbers/powers-of-3/∞))
     (test '(1 1 2 6 24 120 720 5040 40320 362880) ((list○take 10) numbers/factorials/∞))
     (test '(1 2 4 7 11 16 22 29 37 46) ((list○take 10) numbers/central-polygonal/∞))
     (test '(1 0 1 0 0 0 0 0 0 0) ((list○take 10) (factorization 10)))
     (test '(0 0 0 0 0 0 1 0 0 0) ((list○take 10) (factorization 17)))
     (test '(3 1 0 0 0 0 0 0 0 0) ((list○take 10) (factorization 24)))
     (test #t ((○ not prime?) 55))
     (test #t (prime? 17))
     ;(test '() ((list○take 10000) (factorization 248475639))) ; both sexps takes very long time
     ;(test #t (prime? 82825213))

     (test '(0 1 3 6 10 15 21 28 36 45) ((list○take 10) numbers/triangular/+scan))
     (test '(0 1 2 4 7 12 20 33 54 88) ((list○take 10) numbers/fibs/+scan)) ; https://oeis.org/A000071
     (test
      ((list○take 10) numbers/fibs/+scan)
      ((list○take 10) ((stream:zip-with -) (stream:cddr numbers/fibs/∞) stream:1s)))
     (test '(1 1 2 6 30 240 3120 65520 2227680 122522400) ((list○take 10) numbers/fibs/∙scan))

     (test '(1 1/2 3/8 5/16 35/128 63/256 231/1024 429/2048 6435/32768 12155/65536)
      ((list○take 10) (series:√/∞ stream:1s)))
    (test ((list○take 10) stream:1s) ((list○take 10) ((compose (series:expt 2) series:√/∞) stream:1s)))
    (test '(1 2 3 4 5 6 7 8 9 10) ((list○take 10) (series:× stream:1s stream:1s)))
    (test '(0 1 3 6 10 15 21 28 36 45) ((list○take 10) (series:× numbers/nats/∞ stream:1s)))


     (test-error ((list○take 10) (series:exp stream:1)))
     (test '(1 0 0 0 0 0 0 0 0 0) ((list○take 10) (series:exp series:0)))
     (test '(1 0 0 0 0 0 0 0 0 0) ((list○take 10) (series:○ taylor/exponential series:0)))
     (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
      ((list○take 10) taylor/exponential))
     (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
      ((list○take 10) (series:exp `(0 ,@stream:1))))
     (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
      ((list○take 10) (series:exp (list->poly '(0 1)))))
     (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
      ((list○take 10) (series:○ taylor/exponential (list->poly '(0 1)))))
     (test '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
      ((list○take 10) (series:exp (list->poly '(0 1)))))
     (test '(1 1 3/2 13/6 73/24 167/40 4051/720 37633/5040 43817/4480 4596553/362880)
      ((list○take 10) (series:exp `(0 ,@stream:1s))))
     (test '(1 1 3/2 13/6 73/24 167/40 4051/720 37633/5040 43817/4480 4596553/362880)
      ((list○take 10) (series:○ taylor/exponential `(0 ,@stream:1s))))
     (test '(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0) ((list○take 10) taylor/cosine))
     (test '(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880) ((list○take 10) taylor/sine))
    (let ((sine2+cosine2 (series:+
                          (series:× taylor/sine taylor/sine)
                          (series:× taylor/cosine taylor/cosine)))
          (tangent-series (series:/ taylor/sine taylor/cosine))
          (arctan-series (series:∫ (series:/ stream:1 (list->poly '(1 0 1)))))
         )
     (test '(1 0 0 0 0 0 0 0 0 0) ((list○take 10) sine2+cosine2))
     (test '(0 1 0 1/3 0 2/15 0 17/315 0 62/2835) ((list○take 10) tangent-series))
     (test '(0 1 0 -1/3 0 1/5 0 -1/7 0 1/9) ((list○take 10) arctan-series))
     (test ((list○take 10) series:0)
      ((list○take 10)
       ((stream:zip-with -)
        (series:/ taylor/sine taylor/cosine) ; tangent-series
        (series:◇ arctan-series)))) ; (equal? ((compose T arctan-series) x) x) ↔ (equal? T tangent-series)
    )
    (test '(1 -1 0 0 0 0 0 0 0 0) ((list○take 10) (series:⁻¹ stream:1s)))
    (test '(1 -2 1 0 0 0 0 0 0 0) ((list○take 10) (series:⁻¹ numbers/nats>0)))
    (test '(1 0 0 0 0 0 0 0 0 0) ((list○take 10) (series:× numbers/nats>0 (series:⁻¹ numbers/nats>0))))
    (test '(1 -1 -1 0 0 0 0 0 0 0) ((list○take 10) (series:⁻¹ (stream:cdr numbers/fibs/∞))))
    (test '(1 0 0 0 0 0 0 0 0 0) ((list○take 10) (series:× numbers/fibs>0/∞ (series:⁻¹ numbers/fibs>0/∞))))
(let ((non-unary-series (stream:cons 4 numbers/nats>0)))
 (test '(1/4 -1/16 -7/64 -33/256 -119/1024 -305/4096 -231/16384 3263/65536 26537/262144 133551/1048576)
  ((list○take 10) (series:⁻¹ non-unary-series)))
 (test '(1 0 0 0 0 0 0 0 0 0)
  ((list○take 10) (series:× non-unary-series (series:⁻¹ non-unary-series)))))
    (test '(1 0 -6 0 12 0 -8 0 0 0)
     ((list○take 10) ((series:expt 3) (list->poly '(1 0 -2)))))
    (test '(1 1 1 1 1 1 1 1 1 1)
     ((list○take 10) (series:/ stream:1 (list->poly '(1 -1)))))
    (test '(0 1 1 2 3 5 8 13 21 34)
     ((list○take 10) (series:/ (formalvar-series 1) (list->poly '(1 -1 -1)))))
    (test '(1 2 3 4 5 6 7 8 9 10)
     ((list○take 10) (series:/ stream:1 ((series:expt 2) (list->poly '(1 -1))))))
    (test '(0 1 -1 1 -1 1 -1 1 -1 1)
     ((list○take 10) (series:◇ (stream:cons 0 stream:1s))))
    (test '(0 1 -1 1 -1 1 -1 1 -1 1)
     ((list○take 10) (series:/ (formalvar-series 1) (list->poly '(1 1)))))
    (test '((1)
            (1 1)
            (1 2 1)
            (1 3 3 1)
            (1 4 6 4 1)
            (1 5 10 10 5 1)
            (1 6 15 20 15 6 1)
            (1 7 21 35 35 21 7 1)
            (1 8 28 56 70 56 28 8 1)
            (1 9 36 84 126 126 84 36 9 1))
     ((list○take 10) (riordan-array stream:1s stream:1s)))
    (test ; h(t) = tA(h(t)) where h(t) = 1/(1-t), aka the Pascal comp inverse
     ((list○take 10) (series:/ (formalvar-series 1) `(1 1 . ,series:0)))
     ((list○take 10) (series:◇ (stream:cons 0 stream:1s))))
(let ((rows ((list○take 10)
             (series:○ ; multivariate
              (series:/ stream:1 (list->poly '(1 -1)))
              (list->poly `( ,(list->poly '()) ,(list->poly '(1 1))))))))
 (test '((1)
         (1 1)
         (1 2 1)
         (1 3 3 1)
         (1 4 6 4 1)
         (1 5 10 10 5 1)
         (1 6 15 20 15 6 1)
         (1 7 21 35 35 21 7 1)
         (1 8 28 56 70 56 28 8 1)
         (1 9 36 84 126 126 84 36 9 1))
  ((map/with-index list○take 1) rows)))
    (test '((1 0 0 0 0 0 0 0 0 0)
            (1 1 1 1 1 1 1 1 1 1)
            (1 2 3 4 5 6 7 8 9 10)
            (1 3 6 10 15 21 28 36 45 55)
            (1 4 10 20 35 56 84 120 165 220)
            (1 5 15 35 70 126 210 330 495 715)
            (1 6 21 56 126 252 462 792 1287 2002)
            (1 7 28 84 210 462 924 1716 3003 5005)
            (1 8 36 120 330 792 1716 3432 6435 11440)
            (1 9 45 165 495 1287 3003 6435 12870 24310))
     (let ((rows ((list○take 10)
                  (series:○ ; multivariate
                   (series:/ stream:1 (list->poly '(1 -1)))
                   (list->poly `( ,(list->poly '()) ,stream:1s))))))
      (map (list○take 10) rows)))
    (test '((1 1 1 1 1 1 1 1 1 1)
            (0 1 2 3 4 5 6 7 8 9)
            (0 0 1 3 6 10 15 21 28 36)
            (0 0 0 1 4 10 20 35 56 84)
            (0 0 0 0 1 5 15 35 70 126)
            (0 0 0 0 0 1 6 21 56 126)
            (0 0 0 0 0 0 1 7 28 84)
            (0 0 0 0 0 0 0 1 8 36)
            (0 0 0 0 0 0 0 0 1 9)
            (0 0 0 0 0 0 0 0 0 1))
     (let ((rows ((list○take 10) ; a pure bivariate gf definition of a Riordan array
                  ((stream:zip-with series:×)
                   stream:1s
                   (series:○ ; multivariate
                    (series:/ stream:1 (list->poly '(1 -1)))
                    (list->poly `( ,(list->poly '()) ,(stream:cons 0 stream:1s)))
                   )))))
      (map (list○take 10) rows)))
    (test '((1 0 0 0 0 0 0 0 0 0)
            (0 1 1 1 1 1 1 1 1 1)
            (0 0 1 2 3 4 5 6 7 8)
            (0 0 0 1 3 6 10 15 21 28)
            (0 0 0 0 1 4 10 20 35 56)
            (0 0 0 0 0 1 5 15 35 70)
            (0 0 0 0 0 0 1 6 21 56)
            (0 0 0 0 0 0 0 1 7 28)
            (0 0 0 0 0 0 0 0 1 8)
            (0 0 0 0 0 0 0 0 0 1))
     (let ((rows ((list○take 10)
                  (series:○ ; multivariate
                   (series:/ stream:1 (list->poly '(1 -1)))
                   (list->poly `( ,(list->poly '()) ,(stream:cons 0 stream:1s)))))))
      (map (list○take 10) rows)))
    (test '((1 0 0 0 0 0 0 0 0 0)
            (0 1 1 1 1 1 1 1 1 1)
            (0 0 2 4 6 8 10 12 14 16)
            (0 0 0 2 6 12 20 30 42 56)
            (0 0 0 0 2 8 20 40 70 112)
            (0 0 0 0 0 2 10 30 70 140)
            (0 0 0 0 0 0 2 12 42 112)
            (0 0 0 0 0 0 0 2 14 56)
            (0 0 0 0 0 0 0 0 2 16)
            (0 0 0 0 0 0 0 0 0 2))
     (let ((rows ((list○take 10)
                  (series:○ ; multivariate
                   (series:/ stream:1 (list->poly '(1 -1)))
                   (list->poly `( ,(list->poly '())
                                  ,(stream:cons 0 stream:1s)
                                  ,(stream:cons 0 (stream:cons 0 stream:1s))))))))
      (map (list○take 10) rows)))
    (test '((1)
            (1 1)
            (1 3 1)
            (1 6 5 1)
            (1 10 15 7 1)
            (1 15 35 28 9 1)
            (1 21 70 84 45 11 1)
            (1 28 126 210 165 66 13 1)
            (1 36 210 462 495 286 91 15 1)
            (1 45 330 924 1287 1001 455 120 17 1))
     ((list○take 10) (riordan-array stream:1s (series:from 1))))
    (test '(1 1 2 5 14 42 132 429 1430 4862) ((list○take 10) taylor/catalan))
    (test '(0 1 1 2 5 14 42 132 429 1430)
     ((list○take 10)
      (series:/
       (series:-
        stream:1
        (series:√/∞ (list->poly '(1 -4))))
       (list->poly '(2)))))
    (letdelay (; again looking for Catalan numbers
               (tree (stream:cons 0 forest))
               (lst (stream:cons 1 lst))
               (forest (series:○ lst tree)))
     (test '(0 1 1 2 5 14 42 132 429 1430) ((list○take 10) tree)))
    (test '((1)
            (1 1)
            (2 2 1)
            (5 5 3 1)
            (14 14 9 4 1)
            (42 42 28 14 5 1)
            (132 132 90 48 20 6 1)
            (429 429 297 165 75 27 7 1)
            (1430 1430 1001 572 275 110 35 8 1)
            (4862 4862 3432 2002 1001 429 154 44 9 1))
     ((list○take 10) (riordan-array taylor/catalan taylor/catalan)))
    (test '(1 1 2 3 5 8 13 21 34 55) ((list○take 10) taylor/fibonacci))
    (test '((1)
            (1 1)
            (2 2 1)
            (3 5 3 1)
            (5 12 9 4 1)
            (8 31 26 14 5 1)
            (13 85 77 46 20 6 1)
            (21 248 235 150 73 27 7 1)
            (34 762 741 493 258 108 35 8 1)
            (55 2440 2406 1644 903 410 152 44 9 1))
     ((list○take 10) (riordan-array taylor/fibonacci taylor/catalan)))
    (test '((1)
            (1 1)
            (2 2 1)
            (3 5 3 1)
            (5 10 9 4 1)
            (8 20 22 14 5 1)
            (13 38 51 40 20 6 1)
            (21 71 111 105 65 27 7 1)
            (34 130 233 256 190 98 35 8 1)
            (55 235 474 594 511 315 140 44 9 1))
     ((list○take 10) (riordan-array taylor/fibonacci taylor/fibonacci)))
    (test '((1)
            (1 1)
            (2 2 1)
            (3 4 3 1)
            (5 7 7 4 1)
            (8 12 14 11 5 1)
            (13 20 26 25 16 6 1)
            (21 33 46 51 41 22 7 1)
            (34 54 79 97 92 63 29 8 1)
            (55 88 133 176 189 155 92 37 9 1))
     ((list○take 10) (riordan-array taylor/fibonacci stream:1s)))
    (test '((1)
            (1 1)
            (2 2 1)
            (6 5 3 1)
            (24 15 9 4 1)
            (120 53 29 14 5 1)
            (720 222 102 49 20 6 1)
            (5040 1120 400 178 76 27 7 1)
            (40320 6849 1809 689 289 111 35 8 1)
            (362880 50111 9791 2942 1133 444 155 44 9 1))
     ((list○take 10) (riordan-array numbers/factorials/∞ taylor/catalan)))
    (test '((1)
            (1 1)
            (2 2 1)
            (6 5 3 1)
            (24 16 9 4 1)
            (120 64 31 14 5 1)
            (720 312 126 52 20 6 1)
            (5040 1812 606 217 80 27 7 1)
            (40320 12288 3428 1040 345 116 35 8 1)
            (362880 95616 22572 5768 1661 519 161 44 9 1))
     ((list○take 10) (riordan-array numbers/factorials/∞ numbers/factorials/∞)))

    (test '(1 3/2 17/12 577/408 665857/470832) ((list○take 5) (series:√ 2)))
    (test '(4 8/3 52/15 304/105 1052/315 10312/3465 147916/45045 135904/45045 2490548/765765 44257352/14549535)
     ((list○take 10) taylor/π))
    (test '(19/6 47/15 1321/420 989/315 21779/6930 141481/45045 1132277/360360 801821/255255 91424611/29099070 45706007/14549535)
     ((list○take 10) (series:Euler-transform taylor/π)))
    (test '(4 19/6 597/190 73480501/23389520 908158408158535624217/289075793976001466280 6122842329181964122242157261623055408836895066390212805633550889340420878203921614979/1948961244684053510230839929806362227580597549551747444462336644887607671758516827680)
     ((list○take 6) ((stream:tableau series:Euler-transform) taylor/π)))
    (test '(1 1/2 5/6 7/12 47/60 37/60 319/420 533/840 1879/2520 1627/2520)
     ((list○take 10) series:log₂))
    (test '(7/10 29/42 25/36 457/660 541/780 97/140 9901/14280 33181/47880 1747/2520 441871/637560)
     ((list○take 10) (series:Euler-transform series:log₂)))
    (test '(1 7/10 165/238 380522285/548976276 755849325680052062216639661/1090460049411856348776491380 318738655178511632543822227346530350595387994474669640697143248267438214457834012964733985868157066661175569469393/459842677096914359400941379802880332404679211833600390612039625007123278498884893986945137648853585966630779010940)
     ((list○take 6) ((stream:tableau series:Euler-transform) series:log₂)))
    (test '(0 1 2 3 4 5 6 7 8 9) ((list○take 10) (stream:enumerate-upper numbers/nats/∞)))
    (test '(0 1 2 3 4 5 6 7 8 9) ((list○take 10) (stream:enumerate-lower numbers/nats/∞)))
    (test '((0 0) (0 1) (1 1) (0 2) (1 2) (0 3) (2 2) (0 4) (1 3) (0 5))
     ((list○take 10) (stream:enumerate-upper numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 2) (2 1 1) (0 1 2) (1 0 2) (0 0 3))
     ((list○take 10) (stream:enumerate-upper numbers/nats/∞ numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0 0 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (2 1 0 1) (0 1 1 1) (1 0 1 1) (0 0 0 2))
     ((list○take 10) (stream:enumerate-upper numbers/nats/∞ numbers/nats/∞ numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0) (1 0) (1 1) (2 0) (2 1) (3 0) (2 2) (4 0) (3 1) (5 0))
     ((list○take 10) (stream:enumerate-lower numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0 0) (1 0 0) (1 1 0) (2 0 0) (2 1 0) (3 0 0) (2 1 1) (4 0 0) (3 1 0) (5 0 0))
     ((list○take 10) (stream:enumerate-lower numbers/nats/∞ numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0 0 0) (1 0 0 0) (1 1 0 0) (2 0 0 0) (2 1 0 0) (3 0 0 0) (2 1 1 0) (4 0 0 0) (3 1 0 0) (5 0 0 0))
     ((list○take 10) (stream:enumerate-lower numbers/nats/∞ numbers/nats/∞ numbers/nats/∞ numbers/nats/∞)))
    (test '((0 0) (0 1) (1 0) (1 1) (2 0) (0 2) (3 0) (1 2) (4 0) (0 3))
     ((list○take 10) (stream:enumerate-all numbers/nats/∞ numbers/nats/∞)))
    (test 397
     ((compose length stream:->list (stream:take-while
                                    (lambda (pair)
                                     ((compose not equal?) pair '(1 100)))))
      (stream:enumerate-upper numbers/nats/∞ numbers/nats/∞)))
    (test 1559
     ((compose length stream:->list (stream:take-while
                                    (lambda (pair)
                                     (not (equal? pair '(3 100))))))
      (stream:enumerate-upper numbers/nats/∞ numbers/nats/∞)))
    (test '((4 3 5) (3 4 5) (8 6 10) (6 8 10) (12 5 13) (12 9 15) (15 8 17) (5 12 13) (16 12 20) (9 12 15))
     ((list○take 10) (Pythagorean-triples 2)))
    ;(test '() ((list○take 1) (Pythagorean-triples 3))) ; no answer because of Fermat's Last Theorem
    (test '((0 0) (0 1) (1 1) (0 2) (1 2) (0 3) (2 2) (1 3) (0 4) (2 3))
     ((list○take 10) ((stream:enumerate-weighted +) numbers/nats/∞ numbers/nats/∞)))

(let* ((F (lambda (x)
           (not (or ((divisable-by? 2) x) ((divisable-by? 3) x) ((divisable-by? 5) x)))))
       (O (lambda (i j)
           (+ (* i 2) (* j 3) (* 5 i j))))
       (pairs ((stream:enumerate-weighted O)
               ((stream:filter F) numbers/nats>0)
               ((stream:filter F) numbers/nats>0))))
 (test '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13))
  ((list○take 20) pairs))
 (test '(10 58 90 106 138 154 186 234 250 280 298 330 346 378 394 426 432 474 490 508)
  ((list○take 20) ((stream:map (lambda (p) (apply O p))) pairs))))

(let* ((W (lambda (i j)
           (+ (expt i 3) (expt j 3))))
       (sums-of-cubes ((stream:enumerate-weighted W) numbers/nats>0 numbers/nats>0)))
 ;(sums-of-cubes (stream:enumerate-upper numbers/nats>0 numbers/nats>0))) ; no answer
    (letrec ((F (lambda (s)
                 (let ((c (stream:car s))
                       (o (stream:cadr s)))
                  (delay-force
                   (cond
                    ((equal? (apply W c) (apply W o))
                     (stream:cons (list (apply W c) c o) (F (stream:cdr s))))
                    (else (F (stream:cdr s)))))))))
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
      ((list○take 10) (F sums-of-cubes)))
    )

(let* ((W (lambda (i j)
           (+ (expt i 2) (expt j 2))))
       (sums-of-squares ((stream:enumerate-weighted W) numbers/nats>0 numbers/nats>0)))
 ;(sums-of-squares (stream:enumerate-upper numbers/nats>0 numbers/nats>0))) ; no answer
    (letrec ((F (lambda (s)
                 (let ((c (stream:car s))
                       (o (stream:cadr s))
                       (p (stream:caddr s)))
                  (delay-force
                   (cond
                    ((and
                      (equal? (apply W c) (apply W o))
                      (equal? (apply W o) (apply W p)))
                     (stream:cons (list (apply W c) c o p) (F (stream:cdr s))))
                    (else (F (stream:cdr s)))))))))
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
      ((list○take 10) (F sums-of-squares)))
     )

    (let ((expected '(0 1/2 3/2 3 5 15/2 21/2 14 18 45/2)))
     (test expected ((list○take 10) ((series:integrator 0 1/2) numbers/nats>0)))
     (test expected ((list○take 10) ((series:integrator/∞ 0 1/2) numbers/nats>0))))

(let ((expected 2716923932235892457383088121947577188964315018836572803722354774868894945523768158997885697298661429053421034015406256924859461187617653889457753593083386399572063538500432650176144488046171044844121805479607648086607018742077798375087855857012278053105042704758822511824867218226931719410407150364389665913091822576819072281835735365786202176167228686198158460724641052407506305826211156964723064441295969498221919251479211700941935114755531972677360157561485144237786816579422141378066423317811515462669946309306263409027388915931082226854264858661420878279983534424128672461206356847463821364630504359665171573635397346037274752410368174877433941234543153511100471651472869116068528478976916600585383497180172395573924789047989563714318957536493108041591460911612078698461739084741934442448701416575483263891529095158013233115648534154086009312190489168546024398834243847135102411661996020129557921444666343641039137906807591342742464200991933722791531063202677650581946360422027765645970182463780273161113009717582155489902677095053354207944772439271656447869921825959042801322775729022491402012084605367784456090892987682547811360481731795980637847551788259384243997341190753089343387201753821360405430310320564488741142120089460368986590136324737459372963666586532443570474179352656517635333744783401695951969936296323256525034685525470426185224036844803487442831639483152362831735350269624668701702424450940840884555271325190876102665277858154695092765613639718577127438538649414492678358762110235621776218781360881010654696273264706319088453035858355052988808507775439561385232652305316287705653436727647681405618323757201022946801118770148072424021385261829594248369890171583993147934044232792517118743393217276416179842097554494269012251329134783596037733973478306188255291484352384699871420472711423079586319041837563678498472779422282261024744394844558738378027105699691260086532632930941478779680554645850778168703661423819000515895232903243738763481571999080702098369316199601942246247887808385073821861517636839926907458184604648942036355256683219218129910422822177336785268627274482037476294341444562207197209503659518266210432791078248321015453218019586608696207295299183111963158564162419152742807437346241667671688466998244424726765837682151606230638111654756595917019206453978024157097042546937345673337179165242325399648121877178987723999503839197328183925340949191821443698275476295245249466361817367207248089144718808572152781037112209285944844021186534832159964297181970584453756163204297111185823467744743465840230098261424789313315093951766314459027947176701489215746884363426961577348384651887153140609616362927338107686794499974902581579897076172716541504294334300741444106749994715713419630688719451362658288812132056854807330827050505064714442618243101018812153563795539024370219967801515099970721926240625418512417940854760415566229746248973756297569452302821563467574313259066016089521122779204844875998864114930516063910324359331903843040069467324167490917499501000001/1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
 ;(✓ (stream:ref 1000) expected ((series:ode-solver-1st (series:integrator 1 1/1000)) (lambda (y) y)))
 (test  expected ((stream:ref 1000) ((series:ode-solver-1st (series:integrator/∞ 1 1/1000)) (lambda (y) y)))))

    #;(✓ (stream:ref 1000) '() ((series:ode-solver-2nd
                               (series:integrator/∞ 2 1/1000)
                               (series:integrator/∞ -1 1/1000))
                              (lambda (y dy) (+ (* -9 y) dy)))) ; FAILING TEST!


    (let ((integral-v (series:integrator/∞ 10 1/100))
          (integral-i (series:integrator/∞ 0 1/100))
          (func-i (lambda (i v) (+ (* -1 i) v)))
          (func-v (lambda (v i) (* -5 i))))
     (letdelay ((v (integral-v dv))
                (i (integral-i di))
                (di ((stream:zip-with func-i) i v))
                (dv ((stream:zip-with func-v) v i)))
      0
#;(test '() ; FAILING TEST!
    (map (lambda (p)
          (list (exact->inexact (car p)) (exact->inexact (cadr p))))
     ((list○take 500) ((stream:zip-with list) v i))))))

    (let* ((cesaro (stream:map-consecutive-pairs
                    (lambda (n m) (equal? (gcd n m) 1))))
           (pi ((stream:map (lambda (p) (sqrt (/ 6 p))))
                (series:Montecarlo (cesaro (random-numbers 1))))))
     0
#;(test '() ; FAILING TEST!
    ((list○take 1000) pi)))


    ))



    ;(run-tests)
