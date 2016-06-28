

(load "introduction-to-continuations.so")

(display "\n--- making `escaper` function ---\n")
(make-escaper)


(display "\n--- Contexts ---\n")

#|
    > A `context` is a procedure of one variable ▢ , pronounced "hole", to
    > distinguish contexts from other procedures; formally, if `e` is a subexpression
    > of `E`, we use the terminology that "the procedure `c` is a context of `e` in `E`".
    > In the absence of side effects, the procedure `c` applied to the value of `e` is
    > the value of `E`.
|#

(let ((sexp '(+ 3 (* 4 (+ 5 6))))
      (sub-sexp '(+ 5 6))
      (context '(lambda (▢ ) (+ 3 (* 4 ▢ ))))) 
 (display "\ncontext of '(+ 5 6) in '(+ 3 (* 4 (+ 5 6))): ")
 (display context))

#|
    > Let us next extend the mechanism for creating contexts. The second step,
    > namely lambda-abstraction, remains the same, but the first, namely identify
    > the subexpression `e` and replacing it with ▢ , does more. 
    > Now we extend the first step by *evaluating* the expression with the hole ▢ :
    > when evaluation cannot proceed because of the ▢ , we have finished the first step.
    > ** Thus, contexts are procedures created at the point in the computation where
    > we can no longer compute because of the existence of ▢ **
|#

    (let ((sexp '(letrec ((sum+n (lambda (n)
                                  (if (zero? n) 
                                   1
                                   (+ (add1 n) (sum+n (sub1 n)))))))
                  (* 10 (sum+n 5))))
          (sub-sexp '(add1 3))
          (context '(lambda (▢ ) (* 10 (+ 6 (+ 5 (+ ▢ (+ 3 (+ 2 1)))))))))
     (display "\ncontext of '(add1 3) in '(* 10 (sum+n 5)): ")
     (display context))

#|
    > A context might involve the use of `set!`. In the following snippet,
    > the free variable `n`, initially `1`, is taken from the `let` expression.
    > Each time the context is invoked, the variable `n` is incremented accordingly;
    > therefore, **it follows that contexts are procedures that may even maintain state**.
|#
    (let ((sexp '(begin
                  (display 0)
                  (let ((n 1))
                   (if (zero? n)
                    (display (+ 3 (* 4 (+ 5 6))))
                    (display (* (+ (* 3 4) 5) 2)))
                   (set! n (+ n 2))
                   n)))
          (sub-sexp '(* 3 4))
          (context '(lambda (▢ ) (begin
                                  (display 0)
                                  (display (* (+ ▢ 5) 2))
                                  (set! n (+ n 2))
                                  n))))
     (display "\ncontext of '(* 3 4) in\n")
     (pretty-print sexp)
     (display "\nis:\n")
     (pretty-print context))

(display "\n--- Escape procedures ---\n")

#|
    > An `escape` procedure upon invocation yields a value, but never passes
    > that value to others: when an escape procedure is invoked, its result *is*
    > the result of the entire computation, anything else awaiting the result 
    > is ignored.
|#

#|
    > At this time we don't have a mechanism for creating escape procedures,
    > so let us further assume there is a procedure `escaper` that takes any
    > procedure as an argument and returns a similarly defined, namely with
    > the same behavior, escape procedure.
|#

(tester 10 (+ ((escaper *) 5 2) 3))

(tester 8 (+ ((escaper (lambda (x)
                        (- (* x 3) 7))) 
              5) 
           4))

(tester 8 (+ ((escaper (lambda (x)
                        ((escaper -) (* x 3) 7))) 
              5) 
           4))

(tester 15 ((lambda (x) 
             (* x 3)) 
            5))

(tester 15 (+ ((escaper (lambda (x)
                         (- ((escaper *) x 3) 7))) 
               5) 
            4))

    (tester 15 (+ ((escaper (lambda (x)
                             ((escaper -) 
                              ((escaper *) x 3) 7))) 
                   5) 
                4))

#|
    > In the next example, the division awaits the addition and since the
    > addition has been abandoned by escape invocation, the division has also
    > been abandoned. This behavior can be characterized by an equation; if `e`
    > is an escape procedure and `f` is any procedure, then `(compose f e) = e`
    > -- that is, `(f (e expr))` is the same as `(e expr)`, forall expression `expr`.
    > Proof. The context of `(e expr)` in `(f (e expr))` is `(λ (▢ ) (f ▢ ))` which is
    > the same as `f`; since the result of `(f (e expr))` is the result of `(e expr)`,
    > we say that **an escape invocation abandons its context**.
|#

(tester 8 (/ (+ ((escaper (lambda (x)
                           (- (* x 3) 7))) 
                 5) 
              4) 
           2))

    ; 16.4.1
(tester -1 ((escaper add1) ((escaper sub1) 0)))

    ; 16.4.2
    (tester '(3)
     (let ((escape-cons (escaper cons)))
      (escape-cons 1 (escape-cons 2 (escape-cons 3 '())))))

    ; 16.6.1
    (tester "reset invoked" 
     (let ((reset (lambda ()
                   (let ((escape (escaper (lambda () (display "reset invoked")))))
                    (escape)))))
      (cons 1 (reset))))

    ; 16.6.2
    (tester '(2)
     (let ((reset (lambda ()
                   ((escaper (lambda () '(2)))))))
      (cons 1 (reset))))

#|
    We are about to discuss `call/cc`. It is a procedure of one argument, called a `receiver`:
    `receiver` is a procedure of one argument, called a `continuation`. The `continuation`
    argument is also a procedure of one argument, defined as follows:
    let `e` be the expression containing
    subexpr `(call/cc receiver)`, then `(call/cc receiver)` yields `(receiver continuation)`,
    where `continuation` is bound to procedure `(escaper c)`, where `c` is the context of 
    `(call/cc receiver)` in expression `e`.
|#

    ; manually building contexts and their escaping procedures.
    (tester 27 
     (let ((receiver (lambda (continuation) 6))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))

    (tester 27
     (let ((receiver (lambda (continuation) (continuation 6)))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))

    ; the following form is different from the previous two in the expression
    ; `(+ 2 (continuation 6))` in the definition of the `receiver` function.
    ; The dispatching mechanism is the same, the only difference is in *how
    ; the `receiver` uses the given `continuation`. Here a subtle but important
    ; observation has to be stated: observe that `receiver` escapes the
    ; context `(λ (▢ ) (+ 2 ▢ ))` -- namely the one that starts when function 
    ; `receiver` is applied -- not the context denoted by `context` bind, which
    ; is the context we *actually* escape toward to.
    (tester 27
     (let ((receiver (lambda (continuation) (+ 2 (continuation 6))))
           (context (lambda (▢ ) (+ 3 (* 4 ▢ )))))
      (+ 3 (* 4 (receiver (escaper context))))))

    ; contexts and their escaping procedures are provided by `call/cc` directly.
    ; In the following, the system forms the context of `(call/cc receiver)`; next,
    ; the system passes it as an *escape procedure* to `receiver`. Since it is now just 
    ; a simple invocation, *all the rules for procedure invocation apply*.
    (tester 27 
     (let ((receiver (lambda (continuation) 6)))
      (+ 3 (* 4 (call/cc receiver)))))

    (tester 27
     (let ((receiver (lambda (continuation) (continuation 6))))
      (+ 3 (* 4 (call/cc receiver)))))

    (tester 27
     (let ((receiver (lambda (continuation) (+ 2 (continuation 6)))))
      (+ 3 (* 4 (call/cc receiver)))))

    ; 16.9.1
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (continuation 5))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) (continuation 5))))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (continuation 5))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) (continuation 5))))
      (- 3 (* 5 (call/cc receiver)))))


    ; 16.9.2
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) 5)
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) 5)))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) 5)
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) 5)))
      (- 3 (* 5 (call/cc receiver)))))

    ; 16.9.3
    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (+ 1000 (continuation 5)))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (context (lambda (▢ ) (- 3 (* 5 ▢ ))))
           (receiver (lambda (continuation) (+ 1000 (continuation 5)))))
      (- 3 (* 5 (receiver (escaper context))))))

    (tester -22
     (let ((sexp '(- 3 (* 5 ((lambda (continuation) (+ 1000 (continuation 5)))
                             (escaper (lambda (▢ ) (- 3 (* 5 ▢ ))))))))
           (receiver (lambda (continuation) (+ 1000 (continuation 5)))))
      (- 3 (* 5 (call/cc receiver)))))

    ; Observe that if `receiver` is defined as `(λ (continuation) (continuation body))`,
    ; for some `body` expression, then `receiver` can be rewritten as `(λ (continuation) body)`,
    ; because the context that `receiver` would escape invoking `continuation` is `(λ (▢ ) ▢ )`, 
    ; namely the identity function, so no expression is pending on the result of `(continuation body)`.

    ; preparing for experiments
    (define result "any value")
    (define result/cc "any value")

    ; receivers
    (define receiver-1 
     (lambda (proc)
      (proc (list 1))))

    (define receiver-2 
     (lambda (proc)
      (proc (list (proc (list 2))))))

    (define receiver-3 
     (lambda (proc)
      (proc (list (proc (list 3 proc))))))

    ; helpers
    (define display/return 
     (lambda (x)
      (display x)
      x))

    (define answer-maker 
     (lambda (x)
      (cons 'answer-is (display/return x))))

    (define call 
     (lambda (receiver)
      (receiver display/return)))

    ; first experiment
    (tester "(1)(1)" (set! result (answer-maker (call receiver-1))))
    (tester '(answer-is 1) result)

    (tester "(1)" (set! result/cc (answer-maker (call/cc receiver-1))))
    (tester '(answer-is 1) result/cc)

    ; second experiment
    (tester "(2)((2))((2))" (set! result (answer-maker (call receiver-2))))
    (tester '(answer-is (2)) result)

    (tester "(2)" (set! result/cc (answer-maker (call/cc receiver-2))))
    (tester '(answer-is 2) result/cc)

    ; third experiment
    (tester 
     "(3 #<procedure (display/return x)>)((3 #<procedure (display/return x)>))((3 #<procedure (display/return x)>))" 
     (set! result (answer-maker (call receiver-3))))
    (tester `(answer-is (3 ,display/return)) result)
    (tester "(1000)" '(1000) ((cadr (cadr result)) (list 1000)))
    (tester `(answer-is (3 ,display/return)) result)

    (tester 
     "(3 #<procedure (continuation . results1901)>)" 
     (set! result/cc (answer-maker (call/cc receiver-3))))
    (tester `(answer-is 3 ,(caddr result/cc)) result/cc)
    (tester "(1000)" ((caddr result/cc) (list 1000)))
    (tester `(answer-is 1000) result/cc)

    ; fourth experiment (namely, as requested by exercise 16.15)
    (tester 
     "(3 #<procedure (display/return x)>)((3 #<procedure (display/return x)>))((3 #<procedure (display/return x)>))" 'done
     (begin 
      (set! result (answer-maker (call receiver-3)))
      'done))
    (tester `(answer-is (3 ,display/return)) result)
    (tester "(1000)" '(1000) ((cadr (cadr result)) (list 1000)))
    (tester `(answer-is (3 ,display/return)) result)

    (tester 
     "(3 #<procedure (continuation . results1901)>)" 'done
     (begin
      (set! result/cc (answer-maker (call/cc receiver-3)))
      'done))
    (tester `(answer-is 3 ,(caddr result/cc)) result/cc)
    (tester "(1000)" 'done ((caddr result/cc) (list 1000)))
    (tester `(answer-is 1000) result/cc)

    ; 16.14
    (tester "actual in {72, 8072}, with uniform probability"
     (let ((receiver (lambda (continuation)
                      (if (zero? (random 2))
                       (+ 1000 6)
                       (continuation 6)))))
      (* (+ (call/cc receiver) 3) 8)))

    (tester "actual in {144, 8144, 16144}, with probabilities 1/4, 1/2, 1/4, respectively"
     (let ((receiver (lambda (continuation)
                      (if (zero? (random 2))
                       (+ 1000 6)
                       (continuation 6)))))
      (+ 
       (* (+ (call/cc receiver) 3) 8)
       (* (+ (call/cc receiver) 3) 8))))

    (tester "actual in {48, 56, 64}, with probabilities 1/4, 1/2, 1/4, respectively"
     (let ((receiver (lambda (continuation)
                      (continuation ; useless, see comments above
                       (if (zero? (continuation (random 2)))
                        (+ 1000 6)
                        6)))))
      (+ 
       (* (+ (call/cc receiver) 3) 8)
       (* (+ (call/cc receiver) 3) 8))))

    ; 16.16
    (define *deep* "any continuation")

    (define map-sub1
     (lambda (ls)
      (if (null? ls)
       (call/cc (lambda (continuation) 
                 (set! *deep* continuation)
                 '()))
       (cons (sub1 (car ls)) (map-sub1 (cdr ls))))))

    (tester '(1000) (cons 1000 (map-sub1 '())))
    (tester '(1000 a b c) (cons 2000 (*deep* '(a b c))))
    (tester '(1000 -1) (cons 1000 (map-sub1 '(0))))
    (tester '(1000 -1 a b c) (cons 2000 (*deep* '(a b c))))
    (tester '(1000 0 -1) (cons 1000 (map-sub1 '(1 0))))
    (tester '(1000 0 -1 a b c) (cons 2000 (*deep* '(a b c))))
    (tester '(1000 4 3 2 1 0 -1) (cons 1000 (map-sub1 '(5 4 3 2 1 0))))
    (tester '(1000 4 3 2 1 0 -1 a b c) (cons 2000 (*deep* '(a b c))))

    (tester 7 (*escaper/thunk* (lambda () (add1 6))))
    (tester 7 (+ 5 (*escaper/thunk* (lambda () (add1 6)))))

    (tester '(a b d)
     (let ((receiver (escaper
                      (lambda (proc)
                       (cons 'c (proc (list 'd)))))))
      (cons 'a (cons 'b (call/cc receiver)))))

    (tester '(c d)
     (let ((receiver (escaper
                      (lambda (proc)
                       '(c d)))))
      (cons 'a (cons 'b (call/cc receiver)))))

    (make-new-escaper)

    (tester '(c d)
     (let ((receiver (new-escaper
                      (lambda (proc)
                       '(c d)))))
      (cons 'a (cons 'b (call/cc receiver)))))

    #;(tester '(c d)
     (let ((escaper (make-another-escaper))
           (receiver (escaper
                      (lambda (proc)
                       '(c d)))))
      (cons 'a (cons 'b (call/cc receiver)))))


    (define countdown
     (lambda (n attempt)
      (format #t "~%This string will appear only once~%")
      (let* ((message (lambda (direction value)
                       (format #t "\t~Aing `attempt` function with value ~A~%" direction value)
                       value))
             (pair (message 'exit (attempt (message 'enter n))))
             (v (car pair))
             (returner (cadr pair)))
       (format #t "\tnon-negative number: ~A~%" v)
       (if (positive? v)
        (returner (list (sub1 v) returner))
        (format #t "Blastoff")))))

    (tester "
This string will appear only once
        entering `attempt` function with value 3
        exiting `attempt` function with value (3 #<procedure (? x)>)
        non-negative number: 3
(2 #<procedure (? x)>)"
     (let ((attempt (lambda (n)
                     (let ((receiver (lambda (proc) (list n proc))))
                      (receiver (lambda (x) x))))))
      (countdown 3 attempt)))

    (tester "
This string will appear only once
        entering `attempt` function with value 3
        exiting `attempt` function with value (3 #<procedure (continuation . results1901)>)
        non-negative number: 3
        exiting `attempt` function with value (2 #<procedure (continuation . results1901)>)
        non-negative number: 2
        exiting `attempt` function with value (1 #<procedure (continuation . results1901)>)
        non-negative number: 1
        exiting `attempt` function with value (0 #<procedure (continuation . results1901)>)
        non-negative number: 0
Blastoff"
     (let ((attempt (lambda (n)
                     (let ((receiver (lambda (proc) (list n proc))))
                      (call/cc receiver)))))
      (countdown 3 attempt)))


    (tester "begin" "begin" "middle" "begin" "end"
     (let ((receiver
            (lambda (continuation)
             (continuation continuation))) ; equivalent to just returning `continuation`
           (three-phases
            (lambda (continuation)
             (display 'begin)
             (call/cc continuation)
             (display 'middle)
             (call/cc continuation)
             (display 'end))))
      (three-phases (call/cc receiver))))

























