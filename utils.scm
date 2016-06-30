
    (define-syntax tester
     (syntax-rules ()
      ((_ expected ... sexp) 
       (let ((sexp-unevaluated 'sexp))
        (newline)
        (pretty-print sexp-unevaluated)
        (display "\texpected:\t")
        (display expected) ...
        (display "\n\tactual:\t\t")
        (eval sexp-unevaluated)
       ))))
