
   (module macros *

    (import scheme chicken)    
    (use data-structures )

    (define-syntax len
     (syntax-rules ()
      ((len lat) (length lat))))

     (define-for-syntax length-syn 
      (lambda (lat)
       (display lat)
       (length lat)))

     (define-syntax t
      (syntax-rules ()
       ((t name) (define name 't))))

    (define-syntax with-pi-bad
     (syntax-rules (pi)
      ((with-pi <expr> ...)
       (let ((pi 3.14)) <expr> ...))))

    (define-syntax with-pi
     (syntax-rules ()
      ((with-pi <expr> ...)
       (eval (quote (let ((pi 3.14)) <expr> ...))))))

)
