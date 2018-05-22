
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following content has been inspired by "The Seasoned Schemer" book.

    (module continuations *

     (import scheme chicken)
     (import matchable)

     (define eternity 
      (lambda args 
       (apply eternity args)))

     (define-syntax letcc
      (syntax-rules ()
       ((letcc cont sexp more ...) 
        (call/cc (lambda (cont) sexp more ...)))))

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

    (define-syntax dbind/car+cdr
     (syntax-rules (else)
      ((dbind/car+cdr sexp ((a d) pair-sexp) ... (else null-sexp))
       (match sexp 
        (() null-sexp)
        ((a . d) pair-sexp) ...))))

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


    (define values&thunk
     (lambda (f . args)
      (lambda ()
       (apply f args))))


) ; end of module `continuations`
