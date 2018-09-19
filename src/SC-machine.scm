
(module SC-machine *

 (import chicken scheme)

 (use srfi-1)
 (use data-structures datatype extras)
 (use commons)


    (define-datatype combination combination?
     (Id    (identifier symbol?))
     (Comb  (rator combination?) (rand combination?)))

    (define-record-printer combination
     (lambda (c out)
      (cases combination c
       (Id (id) (format out "~a" id))
       (Comb (rator rand) (format out "(~a ~a)" rator rand)))))

    (define curryfy (foldl1
                     (lambda (acc c)
                      (cond
                       ((list? c) (Comb acc (curryfy c)))
                       (else (Comb acc (Id c)))))
                     H₀: (lambda (i) (Id i))))

    (define value
     (lambda (E)
      (rec V (lambda₁-cases combination
              (Id (id) (E id))
              (Comb (rator rand) ((V rator) (V rand)))))))

    (define-record status S C)

    (define-record-printer status
     (lambda (s out)
      (let₁ (P (dbind/status (lambda (s S C)
                              (format out "(~a ~a)" S C))))
       (P s))))

    (define dbind/status
     (lambda (recv)
      (lambda (s)
       (recv s (status-S s) (status-C s)))))

    (define →/interpreted
     (let* ((sym/apply (gensym 'apply))
            (is-apply? (=to? sym/apply)))
      (lambda (E)
       (dbind/status
        (lambda (s S C)
         (cond
          ((null? C) s) ; fixed-point termination condition
          (else (match₁ ((C₀ . C₊) C)
                 (cond
                  ((is-apply? C₀) (match₁ ((f y . S₊) S)
                                   (make-status (cons (f y) S₊) C₊)))
                  (else (cases combination C₀
                         (Id (id) (let₁ (stack (cons (E id) S))
                                   (make-status stack C₊)))
                         (Comb (rator rand) (let* ((cmds (list rand rator sym/apply))
                                                   (control (append cmds C₊)))
                                             (make-status S control))))))))))))))

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

    (define →/compiled
     (lambda (E)
      (dbind/status
       (lambda (s S C)
        (cond
         ((null? C) s)
         (else (let-values (((C₀ C₊) (car+cdr C)))
                (cases instruction C₀
                 (Load (selector) (let₁ (stack (cons (selector E) S))
                                   (make-status stack C₊)))
                 (Apply () (match₁ ((f y . S₊) S)
                            (make-status (cons (f y) S₊) C₊)))))))))))


    )
