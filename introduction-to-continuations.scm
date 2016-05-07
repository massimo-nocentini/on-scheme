
(declare (unit introduction-continuations))

(define *escaper/thunk* "any continuation")

    (define escaper 
     (lambda (proc)
      (lambda args 
       (*escaper/thunk* (lambda () (apply proc args))))))

    (define set-escaper
     (lambda (continuation)
      (set! *escaper/thunk* continuation)
      (*escaper/thunk* (lambda () 
                        (display '(escaper defined as *escaper/thunk* ))))))

    ; to actually instantiate `escaper`
    (apply (call/cc set-escaper) '())
