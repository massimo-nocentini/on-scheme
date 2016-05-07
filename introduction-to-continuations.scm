
;(declare (unit introduction-continuations))

    (define *escaper/thunk* "any continuation")

    (define escaper 
     (lambda (proc)
      (lambda args 
       (*escaper/thunk* (lambda () (apply proc args))))))

    (define set-escaper
     (lambda (continuation)
      (set! *escaper/thunk* continuation)
      (*escaper/thunk* (lambda () (display `(escaper defined as ,*escaper/thunk* ))))))

    (define make-escaper 
     (lambda ()
      (apply (call/cc set-escaper) '())))

    ; add one or more expected expressions
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




