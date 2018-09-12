
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following content has been inspired by "The Seasoned Schemer" book.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module continuations *

 (import scheme chicken)

 (use srfi-1)

 (use matchable data-structures)

 (use commons)

 (define continuation->λ (curry₁ continuation-return))

 (define-syntax letcc
  (syntax-rules ()
   ((letcc (raw λ) sexp more ...)
    (continuation-capture (lambda (raw)
                           (let₁ (λ (continuation->λ raw))
                            sexp more ...))))
   ((letcc cont sexp more ...)
    (letcc (_ cont) sexp more ...))))

 (define apply/cc
  (lambda (λ-sexp . args)
   (match args ; non-exhaustive matching to signal a mistake at call-time
    ((arg ... (last ...)) ; here both `arg` and `last` are lists in the sense of `match`
     (letcc cont (apply λ-sexp (append arg last (list cont))))))))

 (define-syntax escape
  (syntax-rules (else =>)
   ((escape
     (out alpha)
     (else beta))
    (begin
     (letcc out (begin alpha beta))))
   ((escape
     (out alpha)
     (=> beta))
    (begin
     (letcc out (let ((result alpha)) (beta result)))))
  ))


    (define-syntax try
     (syntax-rules (else =>)
      ((try
        (skip alpha) ...
        (else beta))
       (letcc success
        (letcc skip (success alpha)) ...
        beta))
      ((try
        (skip alpha) ...
        (else => beta))
       (letcc success
        (let ((outputs (list (letcc skip (success alpha)) ...)))
         (beta outputs))))
      ((try ((=> λ-sexp (arg ...)) ...
             (else beta)))
       (letcc success
        (letcc cont (success (λ-sexp arg ... cont))) ...
        beta))))

    (define-syntax set/cc!
     (syntax-rules ()
      ((set/cc! v) (set! v (letcc (raw cont) cont))))) ; what happens if the last expression after `set!` might be `(void)` in order to return the `undefined?` value.

    (define-syntax cond/cc
     (syntax-rules (else)
      ((cond/cc
        (pred? handling) ...
        (else recv))
       (letcc (cont λ)
        (cond
         ((continuation? cont) (recv λ))
         ((pred? cont) (handling cont)) ...
         (else (error "Contract violation")))))))

    (define current-continuation/cont
     (τ
      (continuation-capture
       (lambda (cont)
        ((continuation->λ cont) cont)))))

    (define current-continuation/λ
     (τ (continuation-capture (○ Φ continuation->λ))))

    (define amb
     (lambda (recv)
      (letcc K
       (let* ((fail-stack '())
              (results '())
              (✓ (lambda (v) (push! results v)))
              (✗ (lambda ()
                  (cond
                   ((pair? fail-stack) (let₁ (flag (void))
                                        (set!-values
                                         (flag fail-stack)
                                         (car+cdr fail-stack))
                                        (flag flag)))
                   (else (K (reverse! results)))))) ; hard skip, any better idea?
              (ε (lambda (choices)
                  ;(let₁ (cc (continuation-capture continuation->λ)) ; this also works.
                  (let₁ (cc (current-continuation/λ)) ; Matt's version.
                   (cond
                    ((null? choices) (✗))
                    (else (let₁ (choice (void))
                           (set!-values (choice choices) (car+cdr choices))
                           (push! fail-stack cc)
                           choice))))))
              (? (lambda (condition)
                  (unless condition (✗)))))
        (recv ε ? ✗ ✓)))))

    ;; SAT-solving with amb.

(define (implies a b)
 (or (not a) b))

    ;; The is not the most efficient implementation,
    ;; because a continuation is captured for each
    ;; occurrence of the same variable, instead of
    ;; one for each variable.
    (define-syntax sat-solve
     (syntax-rules (and or implies not ∙ ▢)
      ((sat-solve vars formula)
       (sat-solve ∙ vars formula formula))
      ((sat-solve ∙ (var ...) formula assertion)
       (let ((var (void)) ...)
       (amb (lambda (ε ? ✗ ✓)
             (sat-solve ▢ (ε ? ✗ ✓) 
              formula 
              (begin (✓ (list var ...)) (✗)) 
              assertion)))))
      ((sat-solve ▢ selectors (not phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (and phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (and phi1 phi2 ...) body assertion)
       (sat-solve ▢ selectors phi1 
        (sat-solve ▢ selectors (and phi2 ...) body assertion)))
      ((sat-solve ▢ selectors (or phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (or phi1 phi2 ...) body assertion)
       (sat-solve ▢ selectors phi1 
        (sat-solve ▢ selectors (or phi2 ...) body assertion)))
      ((sat-solve ▢ selectors (implies phi1 phi2) body assertion)
       (sat-solve ▢ selectors phi1 
        (sat-solve ▢ selectors phi2 body assertion)))
      ((sat-solve ▢ selectors #t body assertion) body)
      ((sat-solve ▢ (ε ? ✗ ✓) #f body assertion) (✗))
      ((sat-solve ▢ (ε ? ✗ ✓) v body assertion)
       ;(let₁ (v (ε (list #t #f)))
       (begin 
        (set! v (ε (list #t #f)))
        (cond
         (assertion body)
         (else (✗)))))
      ((sat-solve ▢ selectors phi body) 
       (sat-solve ▢ selectors phi body phi))
      ))



    ) ; end of module `continuations`
