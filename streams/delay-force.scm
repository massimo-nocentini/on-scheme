

(define (stream-filter p? s)
 (delay-force 
  (let ((s-mature (force s)))  
   (if (null? s-mature) 
    (delay '())
    (let ((h (car s-mature)) 
          (t (cdr s-mature)))
     (if (p? h)
      (delay (cons h (stream-filter p? t)))
      (stream-filter p? t)))))))

; very inefficient version that uses unbounded memory because of (delay (force ...))
#;(define (stream-filter p? s)
 (delay (force 
         (let ((s-mature (force s)))  
          (if (null? s-mature) 
           (delay '())
           (let ((h (car s-mature)) 
                 (t (cdr s-mature)))
            (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))))

(define (from n)
 (delay-force (cons n (from (+ n 1)))))

(define large-number 1000000000)
;(define large-number 1000)

(define-record var x)

(define var-1 (make-var 3))
(define var-2 (make-var '(3 2)))

(display (equal? var-1 var-2))

#;(display (car (force (stream-filter 
                      (lambda (n) (= n large-number)) 
                      (from 0)))))

