
(module streams *

 (import chicken scheme)

 (use srfi-1)
 (use test numbers data-structures)
 ;(use random-bsd)

 (use commons)

 (define stream:null? (compose null? force))

 ;(define stream:car (compose car force))
 (define stream:car (compose
                     (lambda (i)
                      (cond
                       ((pair? i) (car i))
                       (else i)))
                     force))
 ;(define stream:cdr (compose cdr force))
 (define stream:cdr (compose
                     (lambda (i)
                      (cond
                       ((pair? i) (cdr i))
                       (else i)))
                     force))
    (define stream:cadr (compose stream:car stream:cdr))
    (define stream:cddr (compose stream:cdr stream:cdr))
    (define stream:caddr (compose stream:car stream:cddr))

    (define-syntax stream:cons
     (syntax-rules ()
      ((stream:cons a d) (delay (cons a d)))))


    (define-syntax letdelay
     (syntax-rules ()
      ((letdelay ((bind sexp) ...) body ...)
       (letrec ((bind (delay-force sexp)) ...) body ...))))

    (define-syntax define-delay
     (syntax-rules ()
      ((define-delay bind sexp) (define bind (letdelay ((α sexp)) α)))))

    (define-syntax stream:dest/car+cdr
     (syntax-rules (else)

      ((stream:dest/car+cdr ((s (a d)) ...) body ...)
       (let-values (((a d) (values (stream:car s) (stream:cdr s))) ...); inefficient because `force` will be called twice
        body ...))

      ((stream:dest/car+cdr () body ...) (begin body ...)) ; base case for `recursion`

      ((stream:dest/car+cdr ((s (a d) (else n))
                             (r (b e) (else m)) ...)
        body ...)
       (cond
        ((stream:null? s) n)
        (else (stream:dest/car+cdr ((s (a d)))
               (stream:dest/car+cdr ((r (b e) (else m)) ...)
                body ...)))))
     ))

    (define-syntax stream:dest/car+cdr!
     (syntax-rules ()
      ((stream:dest/car+cdr! ((s (a d)) ...) body ...)
       (let ((dest (compose car+cdr force)))
        (let*-values (((a d) (dest s)) ...)
         body ...)))))

    (define-syntax Λ ; "big-lambda", a functional abstraction that returns *streams* of values
     (syntax-rules ()
      ((Λ args body ...)
       (lambda args (delay-force (begin body ...))))))

    (define-delay stream:empty '())

    (define-syntax :⁺
     (syntax-rules ()
      ((:⁺ α) α)
      ((:⁺ a b ... ) (stream:cons a (:⁺ b ...)))))

    (define stream:singleton
     (Λ (a)
      (stream:cons a stream:empty)))

    (define stream:ref
     (lambda (n)
      (lambda (α)
       (letrec ((R (lambda (m s)
                    (cond
                     ((zero? m) ((compose car force) s))
                     (else (R (sub1 m) (stream:cdr s)))))))
        (R n α)))))

    (define stream:foldr
     (lambda (func init)
      (letrec ((F (Λ (s)
                   (stream:dest/car+cdr ((s (scar scdr) (else (force init))))
                    (func scar (F scdr))))))
       F)))

    (define stream:map
     (lambda (func)
      (letrec ((M (Λ (s)
                   (stream:dest/car+cdr ((s (scar scdr) (else stream:empty)))
                    (stream:cons (func scar) (M scdr))))))
       M)))

    #;(define stream:map
     (lambda (func)
      (letrec ((M (Λ (s)
                   (stream:dest/car+cdr s
                    ((scar scdr) (stream:cons (func scar) (M scdr)))
                    (else stream:empty))
                  )))
       M)))


    (define stream:filter
     (lambda (pred?)
      (letrec ((F (Λ (s)
                   (stream:dest/car+cdr ((s (a d) (else stream:empty)))
                    (cond
                     ((pred? a) (stream:cons a (F d)))
                     (else (F d)))))))
       F)))

    (define stream:take
     (lambda (n)
      (lambda (s)
       (letrec ((T (Λ (i r)
                    (cond
                     ((> i n) stream:empty)
                     (else (stream:dest/car+cdr ((r (rcar rcdr) (else stream:empty)))
                            (stream:cons rcar (T (add1 i) rcdr))))))))
        (T 1 s)))))

    (define stream:->list
     (lambda (s)
      (stream:dest/car+cdr ((s (scar scdr) (else '())))
       (cons scar (stream:->list scdr)))))

    (define list○take (lambda (n) (○ stream:->list (stream:take n))))

    (define stream:from
     (Λ (n)
      (stream:cons n (stream:from (add1 n)))))

    (define stream:iterator
     (lambda (s)
      (let ((α (stream:cons 'useless s)))
       (lambda ()
        (set! α (stream:cdr α))
        (stream:car α)))))

    (define stream:repeat
     (lambda (n)
      (letdelay ((R (stream:cons n R)))
       R)))

    (define stream:0s (stream:repeat 0))
    (define stream:1s (stream:repeat 1))

    (define stream:const
     (Λ (n)
      (stream:cons n stream:0s)))

    (define stream:1 (stream:const 1))

    #;(define stream:in?
     (lambda (n)
      (lambda (α)
       (letrec ((P (lambda (α)
                    (stream:dest/car+cdr ((α (α₀ α⁺)))
                     (cond
                      ((equal? n α₀) #t)
                      ((< n α₀) #f)
                      (else (P α⁺)))))))
        (P α)))))

    (define stream:zip-with
     (lambda (op)
      (letrec ((Z (Λ streams
                   (stream:cons
                    (apply op (map stream:car streams))
                    (apply Z (map stream:cdr streams))))))
       Z)))

    (define stream:convolution
     (lambda (func scale comb)
      (letrec ((C (Λ (s r)
                   (stream:dest/car+cdr ((s (scar scdr))
                                         (r (rcar rcdr)))
                    (stream:cons
                     (func scar rcar)
                     (comb ((scale scar) rcdr) (C scdr r)))))))
       C)))

    (define expt-series
     (lambda (n)
      (lambda (s)
       (letrec ((E (Λ (n)
                    (cond
                     ((zero? n) stream:1)
                     (else (mul-series s (E (sub1 n))))))))
        (E n)))))

    (define stream:tails
     (let ((shift-first (compose stream:cdr car)))
      (lambda (s)
       (letrec ((C (Λ streams
                    (stream:cons
                     streams
                     (apply C (cons (shift-first streams) streams))))))
        (C s)))))

    (define stream:prefixes
     (○ (stream:map (fmap stream:car)) stream:tails))

    (define stream:scan
     (lambda (op)
      (○ (stream:map (fapply op)) stream:prefixes)))

    (define list->
     (lambda (tail)
      (letrec ((L (Λ (l)
                   (cond
                    ((null? l) tail)
                    (else (stream:cons (car l) (L (cdr l))))))))
       (lambda (l)
        (cond
         ((null? l) tail)
         (else (L l)))))))

    (define list->stream    (list-> stream:empty))
    (define list->poly      (list-> stream:0s))

#;(define stream:append*
        (Λ (sos asos) ; which stands for 'Stream Of Streams' and 'Another Stream of Streams', respectively
         (stream:dest/car+cdr ((sos (α sos') (else asos))
                               (α (? (stream:append* sos' asos)) (else (a αs))))
          (stream:cons a (stream:append* (stream:cons αs sos') asos)))))

    (define stream:append
     (letrec ((S (Λ (r s)
                  (stream:dest/car+cdr ((r (rcar rcdr) (else s)))
                   (stream:cons rcar (S rcdr s))))))
      (Λ streams
       (cond
        ((null? streams) stream:empty)
        (else (let-values (((first rest) (car+cdr streams)))
               (S first (apply stream:append rest))))))))

    (define stream:merge
     (lambda (pred?)
      (letrec ((M (Λ (s r) ; binary merge strategy
                   (stream:dest/car+cdr ((s (scar scdr) (else r))
                                         (r (rcar rcdr) (else s)))
                    (cond
                     ((equal? (pred? scar rcar) #t)
                      (stream:cons scar (M scdr r)))
                     (else
                      (stream:cons rcar (M s rcdr))))))))
       (lambda streams
        (foldr M stream:empty streams)))))

    (define radix-expand
     (Λ (num den radix)
      (stream:cons
       (quotient (* num radix) den)
       (radix-expand (remainder (* num radix) den) den radix))))

    (define integrate-series
     (Λ (s)
      (letrec ((I (Λ (s n)
                   (stream:dest/car+cdr ((s (scar scdr))
                                         (n (ncar ncdr)))
                    (stream:cons (/ scar ncar) (I scdr ncdr))))))
       (stream:cons 0 (I s (stream:from 1))))))

    (define derivative-series
     (Λ (s)
      (letrec ((D (Λ (s n)
                   (stream:dest/car+cdr ((s (scar scdr))
                                         (n (ncar ncdr)))
                    (stream:cons (* scar ncar) (D scdr ncdr))))))
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
      (stream:dest/car+cdr ((s (scar scdr)))
       (letdelay ((I (stream:cons (/ 1 scar) ((scale-series (/ -1 scar))
                                              (mul-series scdr I)))))
        I))))

    (define division-series&inversion
     (lambda (num denum)
      (mul-series num (inverse-series denum))))

    (define division-series
     (Λ (α β)
      (stream:dest/car+cdr ((α (a α-cdr))
                            (β (b β-cdr)))
       (cond
        ((and (zero? a) (zero? b)) (division-series α-cdr β-cdr))
        (else (let ((q (/ a b)))
               (stream:cons q (division-series
                               (sub-series α-cdr ((scale-series q) β-cdr))
                               β))))))))

    (define integral-series&co
     (lambda (init dt)
      (lambda (s)
       (letdelay ((I (stream:cons init (add-series ((scale-series dt) s) I))))
        I))))

    (define integral-series&rec
     (lambda (init dt)
      (Λ (s)
       (stream:cons init (stream:dest/car+cdr ((s (scar scdr) (else stream:empty)))
                          ((integral-series&rec (+ (* dt scar) init) dt) scdr))))))

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
       (((compose not equal?) 0 (stream:car α))
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

    (define stream:tableau
     (lambda (transform)
      (letrec ((T (Λ (s)
                   (stream:cons s ((compose T transform) s)))))
       (lambda (s)
        ((stream:map stream:car) (T s))))))

    (define stream:enumerate-upper
     (letrec ((tuple (compose flatten list))
              (B (Λ (s r)
                  (stream:dest/car+cdr ((s (scar scdr) (else r))
                                        (r (rcar rcdr) (else s)))
                   (stream:cons
                    (tuple scar rcar)
                    (interleave
                     ((stream:map (lambda (ri) (tuple scar ri))) rcdr)
                     (B scdr rcdr))))))
              (interleave (Λ (s r)
                           (stream:dest/car+cdr ((s (scar scdr) (else r)))
                            (stream:cons scar (interleave r scdr))))))
      (lambda streams
       (foldr B stream:empty streams))))

    (define stream:enumerate-lower
     (letrec ((tuple (compose flatten list))
              (B (Λ (s r)
                  (stream:dest/car+cdr ((s (scar scdr) (else r))
                                        (r (rcar rcdr) (else s)))
                   (stream:cons
                    (tuple scar rcar)
                    (interleave
                     ((stream:map (lambda (si) (tuple si rcar))) scdr)
                     (B scdr rcdr))))))
              (interleave (Λ (s r)
                           (stream:dest/car+cdr ((s (scar scdr) (else r)))
                            (stream:cons scar (interleave r scdr))))))
      (lambda streams
       (foldr B stream:empty streams))))

    (define stream:enumerate-all
     (letrec ((tuple (compose flatten list))
              (B (Λ (s r)
                  (stream:dest/car+cdr ((s (scar scdr) (else r))
                                        (r (rcar rcdr) (else s)))
                   (stream:cons
                    (tuple scar rcar)
                    (interleave
                     (interleave
                      ((stream:map (lambda (ri) (tuple scar ri))) rcdr)
                      (B scdr rcdr))
                     ((stream:map (lambda (si) (tuple si rcar))) scdr))))))
              (interleave (Λ (s r)
                           (stream:dest/car+cdr ((s (scar scdr) (else r)))
                            (stream:cons scar (interleave r scdr))))))
      (lambda streams
       (foldr B stream:empty streams))))

    (define stream:enumerate-weighted
     (lambda (weight)
      (letrec ((make-tuple (compose flatten list))
               (B (Λ (s r)
                   (stream:dest/car+cdr ((s (scar scdr) (else r))
                                         (r (rcar rcdr) (else s)))
                    (stream:cons
                     (make-tuple scar rcar)
                     (interleave
                      ((stream:map (lambda (ri) (make-tuple scar ri))) rcdr)
                      (B scdr rcdr))))))
               (interleave (Λ (s r)
                            (stream:dest/car+cdr ((s (scar scdr) (else r))
                                                  (r (rcar rcdr) (else s)))
                             (cond
                              ((< (apply weight scar) (apply weight rcar))
                               (stream:cons scar (interleave scdr r)))
                              (else (stream:cons rcar (interleave s rcdr))))))))
       (lambda streams
        (foldr B stream:empty streams)))))

    (define stream:take-while
     (lambda (pred?)
      (letrec ((stop? (compose not pred?))
               (W (Λ (s)
                   (stream:dest/car+cdr ((s (scar scdr) (else stream:empty)))
                    (cond
                     ((stop? scar) (stream:cons scar stream:empty))
                     (else (stream:cons scar (W scdr))))))))
       W)))

    (define Pythagorean-triples
     (lambda (n)
      (let ((F (lambda (triple)
                (equal?
                 (+ (expt (car triple) n) (expt (cadr triple) n))
                 (expt (caddr triple) n))))
            (nats (stream:from 1)))
       ;((stream:filter F) (stream:enumerate-upper nats nats nats)))))
((stream:filter F)
 ((stream:enumerate-weighted
   (lambda (i j #!optional (k 0))
    ((compose abs -)
     (+ (expt i n) (expt j n))
     (expt k n))))
  nats nats nats)))))

    (define random-numbers
     (let ((rand-update (lambda (u) (random 1000))))
      (lambda (init)
       (letdelay ((R (stream:cons init ((stream:map rand-update) R))))
        R))))

    (define stream:map-consecutive-pairs
     (lambda (func)
      (letrec ((P (Λ (s)
                   (stream:cons
                    (func (stream:car s) (stream:cadr s))
                    ;(P (stream:cdr s)))))             ; overlapping consecutive pairs
               (P (stream:cddr s)))))) ; disjoint consecutive pairs
    P)))

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
      (stream:dest/car+cdr ((d (dcar dcdr)))
       (stream:cons
        (list dcar)
        ((stream:zip-with cons) dcdr (riordan-array (mul-series d h) h))))))

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
                            ((equal? β₀ stream:0s)  (values mul-series* stream:const))  ; bivariate series
                            (else (error "compose-series" "β₀ neither 0 nor stream:0s" β₀))))))
       (letrec ((C (Λ (α β)
                    (stream:dest/car+cdr ((α (a αs))
                                          (β (b βs)))
                     (stream:cons (B a) (M (C αs β) βs))))))
        (C α β)))))

    (define revert-series
     (Λ (α)
      (stream:dest/car+cdr ((α (α₀ αs)))
       (cond
        ((equal? α₀ 0)
         (letdelay ((R (stream:cons 0 (inverse-series
                                       (compose-series αs R)))))
          R))
        (else (error "revert-series" "α₀ not zero" α₀))))))

)
