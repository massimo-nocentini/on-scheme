
(module SECD-machine *

 (import chicken scheme)

 (use srfi-1 srfi-69 srfi-13)
 (use data-structures datatype extras matchable)
 (use commons continuations)


    (define-datatype expression expression?
     (Id        (identifier symbol?))
     (Lambda    (var symbol?) (body expression?))
     (Comb      (rator expression?) (rand expression?))
     (If        (question expression?) (answer expression?) (otherwise expression?))
     (J         (body expression?)))

    (define expression-J?
     (lambda₁-cases expression
      (J (_) #t)
      (else #f)))

    (define-record-printer expression
     (lambda (e out)
      (cases expression e
       (Id      (id) (format out "~a" id))
       (Lambda  (var body) (format out "(λ (~a) ~a)" var body))
       (Comb    (rator rand) (format out "(~a ~a)" rator rand))
       (If      (q a o) (format out "(if ~a ~a ~a)" q a o))
       (J       (body) (format out "(J ~a)" body)))))

    (define curryfy
     (lambda (sexp)
      (cond
       ((symbol? sexp) (Id sexp))
       ((list? sexp) (match sexp
                      (('J body) (J (curryfy body)))
                      (('cond (question answer) ('else otherwise))
                       (If (curryfy question) (curryfy answer) (curryfy otherwise)))
                      (('cond
                        (question answer)
                        clauses ...)
                       (If (curryfy question) (curryfy answer) (curryfy `(cond ,@clauses))))
                      (('λ (x) body) (Lambda x (curryfy body)))
                      (('λ (x y ...) body) (Lambda x (curryfy `(λ (,@y) ,body))))
                      ((rator rand) (Comb (curryfy rator) (curryfy rand)))
                      ((rator rand ... rand₊) (Comb
                                                (curryfy `(,rator ,@rand))
                                                (curryfy rand₊)))
                      (else (error "match error for" sexp))))
       (else (error "cond error for:" sexp)))))

    (define value
     (lambda (E)
      (lambda (e)
       (letrec ((V (lambda₁-cases expression
                    (Id (id) (E id))
                    (Lambda (var body) (lambda (x)
                                          (let₁ (E₁ ((extend E) `(,var . ,x)))
                                           ((value E₁) body))))
                    (Comb (rator rand) (cond
                                        ((expression-J? rand) 
                                         (letcc skip 
                                          ((V rator) ((V rand) skip))))
                                        (else ((V rator) (V rand)))))
                    (If (q a o) (if (V q) (V a) (V o)))
                    (J (body) (lambda (skip)
                               (cond/λ (V body)
                                (procedure? (lambda (B)
                                             (lambda (x)
                                              (skip (B x)))))
                                (else skip)))))))
        (cond
         ((expression-J? e) (call/cc₊ (V e)))
         (else (V e)))))))

    (define-record status S E C D)

    (define status-init
     (lambda (E C)
       (make-status '() E C (void))))

    (define-record-printer status
     (lambda (s out)
      (format out "~a" ((status-printer 0) s))))

    (define status-printer
     (lambda (indent)
      (dbind/status
       (lambda (s S E C D)
        (let* ((indents (make-string indent #\space))
               (L (fmap (lambda (e)
                         (string-append
                          (make-string (+ indent 4) #\space)
                          (to-string e)))))
               (P (lambda (sym l #!key (indents indents))
                   (match l
                    (() (format #f "~a(~a ~a)" indents sym '()))
                    ((l₀) (format #f "~a(~a (~a))" indents sym l₀))
                    ((l₀ . l₊) (let₁ (rest (string-join (L l₊) "\n"))
                                (format #f "~a(~a (~a\n~a))" indents sym l₀ rest))))))
               (Ss (P "S" S indents: ""))
               (Es (P "E" (if (list? E) E (E->alist E))))
               (Cs (P "C" C))
               (Ds (cond/λ D
                    (undefined? (K (format #f "~a(D ~a)" indents D)))
                    (else (K (format #f "~a(~a)"
                              indents ((status-printer (add1 indent)) D)))))))
         (format #f "~a\n~a\n~a\n~a" Ss Es Cs Ds))))))

    (define dbind/status
     (lambda (recv)
      (lambda (s)
       (recv s (status-S s) (status-E s) (status-C s) (status-D s)))))

    (define-record closure C₁ var E)

    (define dbind/closure
     (lambda (recv)
      (lambda (c)
       (recv c (closure-C₁ c) (closure-var c) (closure-E c)))))

    (define-record-printer closure
     (lambda (c out)
      (let₁ (P (dbind/closure (lambda (_ C₁ var E)
                               (format out "[~a ~a ~a]" C₁ var E))))
       (P c))))

    (define →/interpreted
     (let* ((sym/apply (gensym 'apply))
            (is-apply? (=to? sym/apply)))
      (dbind/status
       (lambda (s S E C D)
        (cond
         ((and (null? C) (undefined? D)) s) ; termination condition for fixed-point
         ((null? C)
          (let₁ (extend-dump (dbind/status
                              (lambda (_ S₁ E₁ C₁ D₁)
                               (let₁ (S₂ (cons (car S) S₁))
                                (make-status S₂ E₁ C₁ D₁)))))
           (extend-dump D)))
         (else (match₁ ((C₀ . C₊) C)
                (cond
                 ((is-apply? C₀) (match₁ ((f y . S₊) S)
                                  (cond/λ f
                                   (closure? (dbind/closure
                                              (lambda (_ C₁ j E₁)
                                               (let ((S₂ '())
                                                     (E₂ ((extend E₁) `(,j . ,y)))
                                                     (C₂ (list C₁))
                                                     (D₂ (make-status S₊ E C₊ D)))
                                                (make-status S₂ E₂ C₂ D₂)))))
                                   (else (K (make-status (cons (f y) S₊) E C₊ D))))))
                 (else (cases expression C₀
                        (Id (id) (let₁ (S₁ (cons (E id) S))
                                  (make-status S₁ E C₊ D)))
                        (Lambda (var body) (let₁ (c (make-closure body var E))
                                            (make-status (cons c S) E C₊ D)))
                        (Comb (rator rand) (let* ((cmds (list rand rator sym/apply))
                                                  (C₁ (append cmds C₊)))
                                            (make-status S E C₁ D)))))))))))))

    (define-datatype de-bruijn de-bruijn?
     (Id₋       (id symbol?))
     (Id₊       (index number?))
     (Lambda₊   (body de-bruijn?))
     (Comb₊     (rator de-bruijn?) (rand de-bruijn?))
     (If₊       (question de-bruijn?) (answer de-bruijn?) (otherwise de-bruijn?)))

    (define-record-printer de-bruijn
     (lambda (e out)
      (cases de-bruijn e
       (Id₋     (id) (format out "~a" id))
       (Id₊     (index) (format out "~a" index))
       (Lambda₊ (body) (format out "(λ ~a)" body))
       (Comb₊   (rator rand) (format out "(~a ~a)" rator rand))
       (If₊     (q a o) (format out "(if ~a ~a ~a)" q a o)))))

    (define expression->de-bruijn
     (lambda (e)
      (letrec ((deB (lambda (e F)
                     (let₁ (position (lambda (v) (list-index (=to? v) F)))
                      (cases expression e
                       (Id (i)              (cond/λ (position i)
                                             (number?   (lambda (p) (Id₊ p)))
                                             (else      (K (Id₋ i)))))
                       (Lambda (var body)   (Lambda₊ (deB body (cons var F))))
                       (Comb (rator rand)   (Comb₊ (deB rator F) (deB rand F)))
                       (If  (q a o)         (If₊ (deB q F) (deB a F) (deB o F))))))))
       (deB e '()))))

    (define value₊
     (lambda (E)
      (lambda (e)
       (letrec ((deB (lambda (e F)
                      (cases de-bruijn e
                       (Id₋ (id) (E id))
                       (Id₊ (index) (list-ref F index))
                       (Lambda₊ (body) (lambda (y) (deB body (cons y F))))
                       (Comb₊ (rator rand) ((deB rator F) (deB rand F)))
                       (If₊ (q a o) (if (deB q F) (deB a F) (deB o F)))))))
        (deB e '())))))

    (define-datatype instruction instruction?
     (Load (selector procedure?))
     (Apply)
     (Position (index number?))
     (Position&Apply  (index number?))
     (Closure   (control list?)) ; [instruction], precisely.
     (Enter)
     (Exit)
     (Test (control list?)) ; [instruction], precisely.
    )

    (define-record-printer instruction
     (lambda (i out)
      (cases instruction i
       (Load (selector) (format out "(Load ~a)" (selector identity)))
       (Apply () (format out "Apply"))
       (Enter () (format out "Enter"))
       (Exit () (format out "Exit"))
       (Test (instructions) (format out "(Test ~a)" instructions))
       (Position (index) (format out "(Position ~a)" index))
       (Position&Apply (index) (format out "(Position&Apply ~a)" index))
       (Closure (instructions) (format out "(Closure ~a)" instructions)))))

    (define compile
     (rec C (lambda₁-cases de-bruijn
             (Id₋ (id) (list (Load ($ id))))
             (Id₊ (index) (list (Position index)))
             (Lambda₊ (body) (list (Closure (C body))))
             (Comb₊ (rator rand) `(,@(C rand) ,@(C rator) ,(Apply)))
             (If₊ (q a o) `(,@(C q) ,(Test (C o)) ,@(C a))))))

    (define-record closure₊ C E)

    (define dbind/closure₊
     (lambda (recv)
      (lambda (c)
       (recv c (closure₊-C c) (closure₊-E c)))))

    (define-record-printer closure₊
     (lambda (c out)
      (format out "[~a ~a]" (closure₊-C c) (closure₊-E c))))

    (define closure₊-printer
     (lambda (indent)
      (dbind/closure₊
       (lambda (_ C E)
        (let* ((indents (make-string indent #\space))
               (L (fmap (lambda (e)
                         (string-append
                          (make-string (+ indent 5) #\space)
                          (to-string e)))))
               (Es (match (if (list? E) E (E->alist E))
                    (() (format #f "~a(E ~a)]" indents '()))
                    ((e₀) (format #f "~a(E (~a))]" indents e₀))
                    ((e₀ . E₊) (let₁ (rest (string-join (L E₊) "\n"))
                                (format #f "~a(E (~a\n~a))]" indents e₀ rest)))))
               (Cs (match C
                    (() (format #f "~a[(C ~a)" indents C))
                    ((c₀) (format #f "~a[(C (~a))" indents c₀))
                    ((c₀ . C₊) (let₁ (rest (string-join (L C₊) "\n"))
                                (format #f "~a[(C (~a\n~a))" indents c₀ rest))))))
         (format #f "~a ~a" Cs Es))))))

    (define →/compiled
     (lambda (E₀)
      (dbind/status
       (lambda (s S E C D)
        (cond
         ((and (null? C) (undefined? D)) s) ; termination condition for fixed-point
         ((null? C)
          (let₁ (extend-dump (dbind/status
                              (lambda (_ S₁ E₁ C₁ D₁)
                               (let₁ (S₂ (cons (car S) S₁))
                                (make-status S₂ E₁ C₁ D₁)))))
           (extend-dump D)))
         (else (match₁ ((C₀ . C₊) C)
                (cases instruction C₀
                 (Load (selector) (let₁ (S₁ (cons (selector E₀) S))
                                   (make-status S₁ E C₊ D)))
                 (Position (index) (let₁ (S₁ (cons (list-ref E index) S))
                                    (make-status S₁ E C₊ D)))
                 (Closure (instructions) (let₁ (c₊ (make-closure₊ instructions E))
                                          (make-status (cons c₊ S) E C₊ D)))
                 (Test (instructions) (match₁ ((boolean . S₊) S)
                                        (cond
                                         (boolean (make-status S₊ E C₊ D))
                                         (else (make-status S₊ E instructions D)))))
                 (Apply () (match₁ ((f y . S₊) S)
                            (cond/λ f
                             (closure₊? (dbind/closure₊
                                         (lambda (_ C₁ E₁)
                                          (let ((S₂ '() )
                                                (E₂ (cons y E₁))
                                                (C₂ (identity C₁))
                                                (D₂ (make-status S₊ E C₊ D)))
                                           (make-status S₂ E₂ C₂ D₂)))))
                             (else (K (make-status (cons (f y) S₊) E C₊ D))))))))))))))

    (define compile⁺
     (rec C (lambda₁-cases de-bruijn
             (Id₋ (id) (list (Load ($ id))))
             (Id₊ (index) (list (Position index)))
             (Lambda₊ (body) (list (Closure (C body))))
             (Comb₊ (rator rand) (cases de-bruijn rator
                                  (Lambda₊ (body) `(,@(C rand) ,(Enter) ,@(C body) ,(Exit)))
                                  (Id₊ (index) `(,@(C rand) ,(Position&Apply index)))
                                  (else `(,@(C rand) ,@(C rator) ,(Apply)))))
             (If₊ (q a o) `(,@(C q) ,(Test (C o)) ,@(C a))))))

    (define →/compiled⁺
     (let ((PA (lambda (f y S E C D)
                (cond/λ f
                 (closure₊? (dbind/closure₊
                             (lambda (_ C₁ E₁)
                              (let ((S₂ '() )
                                    (E₂ (cons y E₁))
                                    (C₂ (identity C₁))
                                    (D₂ (make-status S E C D)))
                               (make-status S₂ E₂ C₂ D₂)))))
                 (else (K (make-status (cons (f y) S) E C D)))))))
      (lambda (E₀)
       (dbind/status
        (lambda (s S E C D)
         (cond
          ((and (null? C) (undefined? D)) s) ; termination condition for fixed-point
          ((null? C)
           (let₁ (extend-dump (dbind/status
                               (lambda (_ S₁ E₁ C₁ D₁)
                                (let₁ (S₂ (cons (car S) S₁))
                                 (make-status S₂ E₁ C₁ D₁)))))
            (extend-dump D)))
          (else (match₁ ((C₀ . C₊) C)
                 (cases instruction C₀
                  (Load (selector) (let₁ (S₁ (cons (selector E₀) S))
                                    (make-status S₁ E C₊ D)))
                  (Position (index) (let₁ (S₁ (cons (list-ref E index) S))
                                     (make-status S₁ E C₊ D)))
                  (Closure (instructions) (let₁ (c₊ (make-closure₊ instructions E))
                                           (make-status (cons c₊ S) E C₊ D)))
                  (Enter () (match₁ ((s₀ . S₊) S)
                             (let₁ (D₁ (make-status S₊ '() '() D))
                              (make-status '() (cons s₀ E) C₊ D₁))))
                  (Exit () (let ((S₁ (cons (car S) (status-S D)))
                                 (E₁ (cdr E))
                                 (C₁ C₊)
                                 (D₁ (status-D D)))
                            (make-status S₁ E₁ C₁ D₁)))
                  (Test (instructions) (match₁ ((boolean . S₊) S)
                                        (cond
                                         (boolean (make-status S₊ E C₊ D))
                                         (else (make-status S₊ E instructions D)))))
                  (Position&Apply (index) (let₁ (f (list-ref E index))
                                           (match₁ ((y . S₊) S)
                                            (PA f y S₊ E C₊ D))))
                  (Apply () (match₁ ((f y . S₊) S)
                             (PA f y S₊ E C₊ D))))))))))))

    )
