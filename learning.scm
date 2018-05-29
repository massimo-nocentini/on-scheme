
(load "introduction-to-continuations.so")


(define x 4)

(make-escaper)

(define escape-* (escaper *))

; the waiting + is abandoned
(+ (escape-* 5 2) 3)



(define ((+curry x) y)
    (+ x y))


