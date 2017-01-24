
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following content has been inspired by "The Seasoned Schemer" book.

    (module continuations *

     (import scheme chicken)

     (define eternity 
      (lambda args 
       (apply eternity args)))

     (define-syntax letcc
      (syntax-rules ()
       ((letcc cont sexp more ...) 
        (call/cc (lambda (cont) sexp more ...)))))

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
      ((dbind/car+cdr sexp ((a d) pair-sexp) (else null-sexp))
       (match sexp 
        (() null-sexp)
        ((a . d) pair-sexp)))))

     (define-syntax try
      (syntax-rules (else)
       ((try ((skip alpha) ... 
              (else beta)))
        (letcc success
         (letcc skip (success alpha)) ...
         beta))))

     (define-syntax try-cps
      (syntax-rules (else in =>)
       ((try 
         (receiver => consumer) ...
         (else cont in else-receiver => else-consumer))
        (letcc success
         (letcc skip 
          (let ((val (receiver skip)))
           (success (consumer val)))) ...
         (else-consumer (else-receiver cont))))
       ((try 
         (receiver => consumer) ...
         (else beta))
        (letcc success
         (letcc skip 
          (let ((val (receiver skip)))
           (success (consumer val)))) ...
         beta))
      ))

    #;(define-syntax values&thunk
     (syntax-rules ()
      ((values&thunk arg ...) 
       (lambda () (values arg ...)))))

    (define values&thunk
     (lambda (f . args)
      (lambda ()
       (apply f args))))

    (define-syntax λ
     (syntax-rules ()
      ((λ args sexp body ...) 
       (lambda args sexp body ...))))

) ; end of module `continuations`
