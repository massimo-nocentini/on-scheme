
;    (declare (unit continuations))

;;; The following content has been inspired by "Scheme and Art of Programming" book.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following content has been inspired by "The Seasoned Schemer" book.

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

; macro `try-applicative` can be used as follows: 
    #;(try-applicative 
     ((if-a-is-in-car (let ((new-car (rm a (car l) if-a-is-in-car))) 
                       (cons new-car (cdr l))))
      (else (cons (car l) (rm a (cdr l) oh)))))

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

; macro `try-applicative` can be used as follows: 
    #;(try-cps
     ((rm a (car l)) => (lambda (new-car) (cons new-car (cdr l))))
     (else oh in (rm a (cdr l)) => (lambda (new-cdr) (cons (car l) new-cdr))))
