
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

    (define new-escaper "any procedure")

    (define make-new-escaper 
     (lambda ()
      (let ((receiver (lambda (continuation)
                       (set! new-escaper (lambda (proc)
                                          (lambda args
                                           (continuation (lambda () (apply proc args))))))
                       (lambda () (display "`new-escaper` is defined")))))
       (apply (call/cc receiver) '()))))

    #;(define make-another-escaper 
     (lambda ()
      (let ((escaper "any procedure")
            (receiver (lambda (continuation)
                       (set! escaper (lambda (proc)
                                      (lambda args
                                       (continuation (lambda () (apply proc args))))))
                       (lambda () 
                        (display "hello") 
                        escaper))))
       (apply (call/cc receiver) '()))))

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

    ; the following syntax works only with interpreted code.
    #;(define-syntax λ
     (syntax-rules ()
      ((λ args . body) (lambda args . body))))


