
(import chicken scheme)

(use data-structures datatype)
(use commons)

    (define extend
     (lambda (E #!key (same? equal?))
      (let₁ (extend₁ (lambda (p E)
                      (lambda-tabled (z)
                       (format #t "~a ~a\n" 'world z)
                       (let-values (((x y) (car+cdr p)))
                        (cond
                         ((same? x z) y)
                         (else (E z)))))))
       (lambda assocs
        (foldr extend₁ E assocs)))))

(format #t "~a\n" 'hello₀)
(define E₀ (lambda _ (void)))
(format #t "~a\n" 'hello₁)
(define E₁ ((extend E₀) '(a . 3)))
(format #t "~a\n" 'hello₂)
(define E₂ ((extend E₁) '(b . 4)))

(define E-null? (=to? E₀ same?: eq?))

(print (E₂ 'a) (E₂ 'b) (E₂ 'c) (E₂ 'a))
(print (E-null? E₀) (E-null? E₁))

    (define-datatype combination combination?
     (Id    (identifier symbol?))
     (Comb  (rator combination?) (rand combination?)))

    (define curryfy (foldl1
                     (lambda (acc c)
                      (cond
                       ((list? c) (Comb acc (curryfy c)))
                       (else (Comb acc (Id c)))))
                     H₀: (lambda (i) (Id i))))

    (define-record-printer combination
     (lambda (c out)
      (cases combination c
       (Id (id) (format out "~a" id))
       (Comb (rator rand) (format out "(~a ~a)" rator rand)))))

    (define value/combination
     (lambda (E)
      (rec V (lambda₁-cases combination
              (Id (id) (E id))
              (Comb (rator rand) ((V rator) (V rand)))))))

      (print ((value/combination E₂) (Id 'a)))
      (print ((value/combination ((extend E₂) `(add1 . ,add1))) (Comb (Id 'add1) (Id 'a))))

(define-record combination-status S C)

    (define-record-printer combination-status
     (lambda (s out)
      (let₁ (P (dbind/combination-status (lambda (s S C)
                                          (format out "(~a ~a)" S C))))
       (P s))))

    (define dbind/combination-status
     (lambda (recv)
      (lambda (s)
       (recv s (combination-status-S s) (combination-status-C s)))))

    (define transition/combination
     (let₁ (sym/apply (gensym 'apply))
      (lambda (E)
       (let₁ (→ (dbind/combination-status
                 (lambda (s S C)
                  (cond
                   ((null? C) s)
                   (else (let-values (((C₀ C₊) (car+cdr C)))
                          (cond
                           ((and (symbol? C₀) (eq? C₀ sym/apply))
                            (match₁ ((f y . S₊) S)
                             (make-combination-status (cons (f y) S₊) C₊)))
                           (else (cases combination C₀
                                  (Id (id) (let₁ (stack (cons (E id) S))
                                            (make-combination-status stack C₊)))
                                  (Comb (rator rand) (let₁ (control (append (list rand rator sym/apply) C₊))
                                                      (make-combination-status S control))))))))))))
        (letrec ((→* (lambda (s α)
                      (cond
                       ((null? (combination-status-C s)) α)
                       (else (let₁ (r (→ s))
                              (→* r (cons r α))))))))
         (lambda (s) (reverse! (→* s (list s)))))))))


     (print (curryfy '(p a b)))
     (print (curryfy '(p (f a c) (m c (p b a)))))

     (print (equal?
             (curryfy '(p (m (p a b) c) (f a c)))
             (Comb
              (Comb
               (Id 'p)
               (Comb
                (Comb
                 (Id 'm)
                 (Comb
                  (Comb (Id 'p) (Id 'a))
                  (Id 'b)))
                (Id 'c)))
              (Comb
               (Comb (Id 'f) (Id 'a))
               (Id 'c)))))

    (print (equal?
            (curryfy '(p (f a c) (m c (p b a))))
            (Comb
             (Comb
              (Id 'p)
              (Comb
               (Comb (Id 'f) (Id 'a))
               (Id 'c)))
             (Comb
              (Comb (Id 'm) (Id 'c))
              (Comb
               (Comb (Id 'p) (Id 'b))
               (Id 'a))))))

    (define-datatype instruction instruction?
     (Load (selector procedure?))
     (Apply))

    (define-record-printer instruction
     (lambda (i out)
      (cases instruction i
       (Load (selector) (format out "(Load ~a)" (selector (gensym))))
       (Apply () (format out "Apply")))))

    (define compile
     (lambda (E)
      (rec C (lambda₁-cases combination
              (Id (id) (list (Load (K (E id)))))
              (Comb (rator rand) (append (C rand) (C rator) (list (Apply))))))))

    (define transition/compiled
     (lambda (E)
      (let₁ (→ (dbind/combination-status
                (lambda (s S C)
                 (cond
                  ((null? C) s)
                  (else (let-values (((C₀ C₊) (car+cdr C)))
                         (cases instruction C₀
                          (Load (selector) (let₁ (stack (cons (selector E) S))
                                            (make-combination-status stack C₊)))
                          (Apply () (match₁ ((f y . S₊) S)
                                     (make-combination-status (cons (f y) S₊) C₊))))))))))
       (letrec ((→* (lambda (s α)
                     (cond
                      ((null? (combination-status-C s)) α)
                      (else (let₁ (r (→ s))
                             (→* r (cons r α))))))))
        (lambda (s) (reverse! (→* s (list s))))))))

    (let* ((p (lambda (a) (lambda (b) (+ a b))))
           (m (lambda (a) (lambda (b) (- a b))))
           (f (lambda (a) (lambda (b) (+ (² a) (² b)))))
           (control (curryfy '(p (m (p a b) c) (f a c))))
           (control₁ (curryfy '(p (f a c) (m c (p b a)))))
           (E ((extend E₀) `(a . 1) `(b . 2) `(c . 3) `(p . ,p) `(m . ,m) `(f . ,f)))
           (→ (transition/combination E))
           (→/compiled (transition/compiled E))
           (F (lambda (s) (format #t "~a\n" s))))
     ((fmap F) (→ (make-combination-status '() (list control))))
     ((fmap F) (→/compiled (make-combination-status '() ((compile E) control))))
     ((fmap F) (→ (make-combination-status '() (list control₁)))))

