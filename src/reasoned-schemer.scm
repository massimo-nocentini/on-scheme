
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
   (condº/§
    ((≡ v 'tea))
    ((≡ v 'cup)))))

 (define split-peaº
  (lambda (v w)
   (condº/§
    ((≡ v 'split) (≡ w 'pea))
    ((≡ v 'red) (≡ w 'bean)))))

 (define split-pea₁º
  (lambda (v w)
   (condº/§
    ((tea-cupº v) (tea-cupº v))
    ((≡ v #f) (tea-cupº w)))))

    (define listº
     (lambda (l)
      (condº/§
       ((nullº l))
       ((fresh (d)
         (∧ (cdrº l d) (listº d)))))))

    (define appendº
     (lambda (x y z)
      (condº/§
       ((nullº x) (≡ z y))
       ((fresh (x₀ x₊ w)
         (∧
          (consº x₀ x₊ x)
          (consº x₀ w z)
          (appendº x₊ y w)))))))

    (define alwaysº 
     (condº/§ 
      (✓) 
      ((fresh () alwaysº))))

    (define neverº 
     (fresh () neverº))

    (define dyckº
     (lambda (α)
      (condº/§
       ((nullº α))
       ((fresh (β γ) (∧
                      (dyckº β)
                      (dyckº γ)
                      (appendº `(○ . ,β) `(● . ,γ) α)))))))

    (define fibonacciº
     (lambda (depth n α)
      (cond
       ((zero? depth) (≡ α (list n)))
       (else (fresh (β γ)
              (∧
               (fibonacciº (sub1 depth) (sub1 n) β)
               (fibonacciº (sub1 depth) (sub2 n) γ)
               (appendº β γ α)))))))

    (define tartagliaº
     (lambda (depth n k α)
      (cond
       ((zero? depth) (≡ α (list (list n k))))
       (else (fresh (β γ)
              (∧
               (tartagliaº (sub1 depth) (sub1 n) (sub1 k) β)
               (tartagliaº (sub1 depth) (sub1 n) k γ)
               (appendº β γ α)))))))

    (define stacksort_º
     (lambda (R O S I)
      (condº/§
       ((nullº S) (nullº I) (≡ R O))
       ((fresh (a d s)
         (∧
          (consº a d I)
          (consº a S s)
          (stacksortº R O s d))))
       ((fresh (a d o) (∧ (consº a d S) (consº a O o) (stacksortº R o d I)))))))

    (define stacksort__º
     (lambda (P R O S I path)
      (condº/§
       ((nullº S) (nullº I) (≡ R O) (≡ P path))
       ((fresh (a d s) (∧ (consº a d I) (consº a S s) (stacksortº P R O s d (cons #\) path)))))
       ((fresh (a d o) (∧ (consº a d S) (consº a O o) (stacksortº P R o d I (cons #\( path))))))))

    (define stacksortº
     (lambda (P R I)
      (letrec ((ssº (lambda (O S I path)
                    (condº/§
                     ((nullº S) (nullº I) (≡ R O) (≡ P path))
                     ((fresh (a d s) (∧
                                      (consº a d I)
                                      (consº a S s)
                                      (ssº O s d (cons #\) path)))))
                     ((fresh (a d o) (∧
                                      (consº a d S)
                                      (consº a O o)
                                      (ssº o d I (cons #\( path)))))))))
       (ssº '() '() I '()))))

    (define 2stacksort_º
     (lambda (R O S₂ S₁ I)
      (condº/§
       ((nullº S₂) (nullº S₁) (nullº I) (≡ R O))
       ((fresh (a d s) (∧ (consº a d I) (consº a S₁ s)  (2stacksortº R O S₂ s d))))
       ((fresh (a d s) (∧ (consº a d S₁) (consº a S₂ s) (2stacksortº R O s d I))))
       ((fresh (a d o) (∧ (consº a d S₂) (consº a O o)  (2stacksortº R o d S₁ I)))))))

    (define 2stacksort__º
     (lambda (P R O S₂ S₁ I path)
      (condº/§
       ((nullº S₂) (nullº S₁) (nullº I) (≡ R O) (≡ P path))
       ((fresh (a d s) (∧ (consº a d I) (consº a S₁ s)  (2stacksortº P R O S₂ s d (cons #\) path)))))
       ((fresh (a d s) (∧ (consº a d S₁) (consº a S₂ s) (2stacksortº P R O s d I (cons #\- path)))))
       ((fresh (a d o) (∧ (consº a d S₂) (consº a O o)  (2stacksortº P R o d S₁ I (cons #\( path))))))))

    (define 2stacksortº
     (lambda (P R I)
      (letrec ((2ssº (lambda (O S₂ S₁ I path)
                      (condº/§
                       ((nullº S₂) (nullº S₁) (nullº I) (≡ R O) (≡ P path))
                       ((fresh (a d s) (∧
                                        (consº a d I)
                                        (consº a S₁ s)
                                        (2ssº O S₂ s d (cons #\) path)))))
                       ((fresh (a d s) (∧
                                        (consº a d S₁)
                                        (consº a S₂ s)
                                        (2ssº O s d I (cons #\- path)))))
                       ((fresh (a d o) (∧
                                        (consº a d S₂)
                                        (consº a O o)
                                        (2ssº o d S₁ I (cons #\( path)))))))))
       (2ssº '() '() '() I '()))))

) ; module's closing paren
