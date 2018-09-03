
(module streams *

 (import chicken scheme)

 (use srfi-1)
 (use test numbers data-structures)
 ;(use random-bsd)

 (use commons)

 (define stream:null? (○ null? force))

 ;(define stream:car (○ car force))
 (define stream:car (○
                     (lambda (i)
                      (cond
                       ((pair? i) (car i))
                       (else i)))
                     force))
 ;(define stream:cdr (○ cdr force))
 (define stream:cdr (○
                     (lambda (i)
                      (cond
                       ((pair? i) (cdr i))
                       (else i)))
                     force))
    (define stream:cadr (○ stream:car stream:cdr))
    (define stream:cddr (○ stream:cdr stream:cdr))
    (define stream:caddr (○ stream:car stream:cddr))

    (define-syntax stream:cons
     (syntax-rules ()
      ((stream:cons a d) (delay-force (cons a d)))))

    (define-syntax letdelay
     (syntax-rules ()
      ((letdelay ((bind sexp) ...) body ...)
       (letrec ((bind (delay-force sexp)) ...) body ...))))

    (define-syntax define-delay
     (syntax-rules ()
      ((define-delay (bind sexp) ...) (define-values (bind ...)
          (letdelay ((bind sexp) ...)
           (values bind ...))))
      ((define-delay bind sexp) (define bind (letdelay ((α sexp)) α)))
     ))

    (define-syntax stream:dest/car+cdr
     (syntax-rules (else ∅)

      ((stream:dest/car+cdr (s ∅) bind)
       (stream:dest/car+cdr s bind (else stream:empty)))

      ((stream:dest/car+cdr s ((a d) body) (else sexp))
       (cond
        ((stream:null? s) sexp)
        (else (let-values (((a d) (values (stream:car s) (stream:cdr s)))); inefficient because `force` will be called twice
               body))))
     ))

    #;(define-syntax stream:dest/car+cdr!
     (syntax-rules ()
      ((stream:dest/car+cdr! ((s (a d)) ...) body ...)
       (let ((dest (○ car+cdr force)))
        (let*-values (((a d) (dest s)) ...)
         body ...)))))

    (define-syntax Λ ; "big-lambda", a functional abstraction that returns a *stream* of values
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
                     ((zero? m) ((○ car force) s))
                     (else (R (sub1 m) (stream:cdr s)))))))
        (R n α)))))

    (define stream:foldr
     (lambda (func init)
      (letrec ((F (Λ (s)
                   (stream:dest/car+cdr s
                    ((scar scdr) (func scar (F scdr)))
                    (else (init))))))
       (○ force F))))

    (define stream:map
     (lambda (func #!key (* #f))    ; `*` in the sense of *starred* defs in 'The Little Schemer',
                                    ; namely to perform __tree recursion__ over streams.
      (letrec ((M (Λ (s)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (cond
                                  ((and * (promise? scar)) (stream:cons (M scar) (M scdr)))
                                  ;(else (stream:cons (collect-values (λ () (func scar))) (M scdr)))))))))
                                  (else (stream:cons
                                         (collect-values (λ () (func scar)))
                                         (M scdr)))))))))
       M)))

    (define stream:append-map
     (lambda (f)
      (letrec ((M (Λ (s)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:append (f scar) (M scdr)))))))
       M)))

    (define stream:filter
     (lambda (pred?)
      (letrec ((F (Λ (s)
                   (stream:dest/car+cdr (s ∅)
                    ((a d) (cond
                            ((pred? a) (stream:cons a (F d)))
                            (else (F d))))))))
       F)))

    (define stream:take
     (lambda (n)
      (lambda (s)
       (letrec ((T (Λ (i r)
                    (cond
                     ((> i n) stream:empty)
                     (else (stream:dest/car+cdr (r ∅)
                            ((rcar rcdr) (stream:cons rcar (T (add1 i) rcdr)))))))))
        (T 1 s)))))

    (define stream:->list
     (lambda (s)
      (stream:dest/car+cdr s
       ((scar scdr) (cons scar (stream:->list scdr)))
       (else '()))))

(define list○take (lambda (n) (○ stream:->list (stream:take n))))

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

    (define stream:zip (stream:zip-with list))

    (define stream:convolution
     (lambda (func scale comb)
      (letrec ((C (Λ (s r)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (stream:dest/car+cdr (r ∅)
                                  ((rcar rcdr) (stream:cons
                                                (func scar rcar)
                                                (comb ((scale scar) rcdr)
                                                      (C scdr r))))))))))
       C)))


    (define stream:tails
     (let ((shift-first (○ stream:cdr car)))
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

    (define list->stream (list-> stream:empty))

#;(define stream:append*
        (Λ (sos asos) ; which stands for 'Stream Of Streams' and 'Another Stream of Streams', respectively
         (stream:dest/car+cdr ((sos (α sos') (else asos))
                               (α (? (stream:append* sos' asos)) (else (a αs))))
          (stream:cons a (stream:append* (stream:cons αs sos') asos)))))

    (define stream:append
     (letrec ((S (Λ (r s)
                  (stream:dest/car+cdr r
                   ((rcar rcdr) (stream:cons rcar (S rcdr s)))
                   (else s)))))
      (Λ streams
       (cond
        ((null? streams) stream:empty)
        (else (let-values (((first rest) (car+cdr streams)))
               (S first (apply stream:append rest))))))))

    (define stream:merge
     (lambda (pred?)
      (letrec ((M (Λ (s r) ; binary merge strategy
                   (cond
                    ((stream:null? r) s)
                    (else (stream:dest/car+cdr s
                           ((scar scdr)
                            (stream:dest/car+cdr (r ∅)
                             ((rcar rcdr) (cond
                                           ((equal? (pred? scar rcar) #t)
                                            (stream:cons scar (M scdr r)))
                                           (else (stream:cons rcar (M s rcdr)))))))
                           (else r)))))))
       (lambda streams
        (foldr M stream:empty streams)))))

    (define stream:tableau
     (lambda (transform)
      (letrec ((T (Λ (s)
                   (stream:cons s ((○ T transform) s)))))
       (lambda (s)
        ((stream:map stream:car *: #f) (T s))))))

    (define-values (stream:enumerate-upper
                    stream:enumerate-lower
                    stream:enumerate-all)
     (letrec ((tuple (○ flatten list))
              (upper (Λ (s r)
                      (stream:dest/car+cdr s
                       ((scar scdr) (stream:dest/car+cdr r
                                     ((rcar rcdr) (stream:cons
                                                   (tuple scar rcar)
                                                   (interleave
                                                    ((stream:map (lambda (ri)
                                                                  (tuple scar ri))) rcdr)
                                                    (upper scdr rcdr))))
                                     (else s)))
                       (else r))))
              (lower (Λ (s r)
                      (stream:dest/car+cdr s
                       ((scar scdr) (stream:dest/car+cdr r
                                     ((rcar rcdr) (stream:cons
                                                   (tuple scar rcar)
                                                   (interleave
                                                    ((stream:map (lambda (si)
                                                                  (tuple si rcar))) scdr)
                                                    (lower scdr rcdr))))
                                     (else s)))
                       (else r))))
              (all (Λ (s r)
                    (stream:dest/car+cdr s
                     ((scar scdr) (stream:dest/car+cdr r
                                   ((rcar rcdr) (stream:cons
                                                 (tuple scar rcar)
                                                 (interleave
                                                  (interleave
                                                   ((stream:map (lambda (ri)
                                                                 (tuple scar ri))) rcdr)
                                                   (all scdr rcdr))
                                                  ((stream:map (lambda (si) (tuple si rcar))) scdr))))
                                   (else s)))
                     (else r))))
              (interleave (Λ (s r)
                           (stream:dest/car+cdr s
                            ((scar scdr) (stream:cons scar (interleave r scdr)))
                            (else r)))))
    (values
     (lambda streams (foldr upper stream:empty streams))
     (lambda streams (foldr lower stream:empty streams))
     (lambda streams (foldr all stream:empty streams)))))


    (define stream:enumerate-weighted
     (lambda (weight)
      (letrec ((make-tuple (○ flatten list))
               (B (Λ (s r)
                   (stream:dest/car+cdr s
                    ((scar scdr) (stream:dest/car+cdr r
                                  ((rcar rcdr) (stream:cons
                                                (make-tuple scar rcar)
                                                (interleave
                                                 ((stream:map (lambda (ri)
                                                               (make-tuple scar ri))) rcdr)
                                                 (B scdr rcdr))))
                                  (else s)))
                    (else r))))
               (interleave (Λ (s r)
                            (stream:dest/car+cdr s
                             ((scar scdr) (stream:dest/car+cdr r
                                           ((rcar rcdr) (cond
                                                         ((< (apply weight scar) (apply weight rcar))
                                                          (stream:cons scar (interleave scdr r)))
                                                         (else (stream:cons rcar (interleave s rcdr)))))
                                           (else s)))
                             (else r)))))
       (lambda streams
        (foldr B stream:empty streams)))))

    (define stream:take-while
     (lambda (pred?)
      (letrec ((stop? (○ not pred?))
               (W (Λ (s)
                   (stream:dest/car+cdr (s ∅)
                    ((scar scdr) (cond
                                  ((stop? scar) (stream:cons scar stream:empty))
                                  (else (stream:cons scar (W scdr)))))))))
       W)))


    (define stream:map-consecutive-pairs
     (lambda (func)
      (letrec ((P (Λ (s)
                   (stream:cons
                    (func (stream:car s) (stream:cadr s))
                    (P (stream:cddr s)))))) ; disjoint consecutive pairs
       P)))

    (define stream:map-overlapping-pairs
     (lambda (func)
      (letrec ((P (Λ (s)
                   (stream:cons
                    (func (stream:car s) (stream:cadr s))
                    (P (stream:cdr s)))))) ; consecutive overlapping pairs
       P)))

    (define stream:§₂   ; binary `mplus`, for pure recursion.
     (Λ (α β)
      (stream:dest/car+cdr α
       ((α₀ α₊) (stream:cons α₀ (stream:§₂ β α₊)))
       (else β))))

    (define stream:§    ; monadic `mplus`, for a *finite* number of streams.
     (Λ streams
      (cond
       ((null? streams) stream:empty)
       (else (let-values (((α streams₊) (car+cdr streams)))
              (stream:dest/car+cdr α
               ((α₀ α₊) (stream:cons α₀ (apply stream:§ (append streams₊ (list α₊)))))
               (else (apply stream:§ streams₊))))))))

    (define stream:>>=  ; monadic `bind`
     (Λ (α β)
      (stream:dest/car+cdr (α ∅)
       ((α₀ α₊) (stream:dest/car+cdr β
                 ((β₀ β₊) (stream:§₂ (β₀ α₀) (stream:>>= α₊ β₊)))
                 (else (error "binders stream should match the bindees one.")))))))

    (define stream:iterative-deepening
     (lambda (m M depth-getter)
      (stream:filter (○ (within? m M) depth-getter))))

    )
