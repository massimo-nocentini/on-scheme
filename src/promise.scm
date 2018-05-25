
(module promise *

 (import chicken scheme)

  (use datatype data-structures srfi-69 test ports)
  (use commons)


    (define-datatype variable (V (s symbol?)))

    (define-record-printer variable
     (lambda (v out)
      (cases variable v
       (V (s) ((compose display symbol∼subscripts) s)))))

    (define fresh₁ ; functional abstraction for introducing _distinct_ `variable` objects
     (lambda (recv)
      (recv (V (gensym)))))

    (define-syntax fresh ; syntactic sugar on top of `fresh₁` to allow arbitrary arity
     (syntax-rules ()
      ((fresh (v) body ...) (fresh₁ (lambda (v) body ...)))
      ((fresh (v w ...) body ...) (fresh₁ (lambda (v) (fresh (w ...) body ...))))))

 

    )
