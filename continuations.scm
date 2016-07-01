
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following content has been inspired by "The Seasoned Schemer" book.

    (module continuations *

     (import scheme chicken)

     (define eternity (lambda args (apply eternity args)))

     (define-syntax letcc
      (syntax-rules ()
       ((letcc cont sexp more ...) 
        (call/cc (lambda (cont) sexp more ...)))))

     (define-syntax try-applicative
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

    (define-syntax λ
     (syntax-rules ()
      ((λ args sexp body ...) 
       (lambda args sexp body ...))))

) ; end of module `continuations`
