
(module reasoned-schemer *

 (import chicken scheme)

 (use streams microkanren)
 
 (use commons)

 (define nullº
  (lambda (l)
   (≡ l '())))

 (define consº
  (lambda (a d p)
   (≡ (cons a d) p)))

 (define carº
  (lambda (p a)
   (fresh (d)
    (consº a d p))))

 (define cdrº
  (lambda (p d)
   (fresh (a)
    (consº a d p))))

 (define pairº
  (lambda (p)
   (fresh (a d)
    (consº a d p))))

 (define tea-cupº
  (lambda (v)
   (condº
    ((≡ v 'tea))
    ((≡ v 'cup)))))

 (define split-peaº
  (lambda (v w)
   (condº
    ((≡ v 'split) (≡ w 'pea))
    ((≡ v 'red) (≡ w 'bean)))))

 (define split-pea₁º
  (lambda (v w)
   (condº
    ((tea-cupº v) (tea-cupº v))
    ((≡ v #f) (tea-cupº w)))))

    (define listº
     (lambda (l)
      (condº
       ((nullº l))
       ((fresh (d)
         (∧ (cdrº l d) (listº d)))))))

    (define appendº
     (lambda (x y z)
      (condº
       ((nullº x) (≡ z y))
       (else (fresh (x₀ x₊ w)
              (∧
               (consº x₀ x₊ x)
               (consº x₀ w z)
               (appendº x₊ y w)))))))

    (define dyckº
     (lambda (α)
      (condº
       ((nullº α))
       ((fresh (β γ) (∧
                      (dyckº β)
                      (dyckº γ)
                      (appendº `(○ . ,β) `(● . ,γ) α)))))))

    (define fibonacciº
     (lambda (depth n α)
      (cond
       ((zero? depth)   (≡ α (list n)))
       (else            (fresh (β γ)
                         (∧
                          (fibonacciº (sub1 depth) (sub1 n) β)
                          (fibonacciº (sub1 depth) (sub2 n) γ)
                          (appendº β γ α)))))))

    (define tartagliaº
     (lambda (depth n k α)
      (cond
       ((zero? depth)   (≡ α (list (list n k))))
       (else            (fresh (β γ)
                         (∧
                          (tartagliaº (sub1 depth) (sub1 n) (sub1 k) β)
                          (tartagliaº (sub1 depth) (sub1 n) k γ)
                          (appendº β γ α)))))))

) ; module's closing paren
