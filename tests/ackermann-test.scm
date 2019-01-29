
(import scheme (chicken base))
(import matchable test)
(import continuations)

; implementation of a macro for the original Ackermann function φ, 
; taken from https://en.wikipedia.org/wiki/Ackermann_function
    (define-syntax φ
     (syntax-rules ()
      ((φ m n 0) (+ m n))
      ((φ m 0 1) 0)
      ((φ m 0 2) 1)
      ((φ m 0 p) m)
      ((φ m n p) (φ m (φ m (sub1 n) p) (sub1 p)))))
    
    (define-syntax AA
     (syntax-rules ()
      ((_ () ns) '(a . ns))
      ((_ (m . ms) '()) (A ms '(a)))
      ((_ (m . ms) (n . ns)) (A ms (A (m . ms) ns)))))

    (define A
     (lambda (α β)
      (match (list α β)
       ((() ns) (cons '● ns))
       (((m . ms) ()) (A ms '(●)))
       (((m . ms) (n . ns)) (A ms (A (cons m ms) ns))))))

    ;(display (φ 1 1 1))
    
    (test '(● ● ● ● ● ● ● ● ●) (A '(a a) '(a a a)))

    #;(let ((sexp '(A (a a) (a a a))))
     (printf "~A is the ackermann value of ~A"
      (let T ((sexp sexp))
       (let ((expanded (expand sexp)))
        (printf "~A is the expansion of ~A~N" expanded sexp)
        (cond 
         ((equal? sexp expanded) sexp)
         (else (match (cdr expanded) 
                ((A-macro (m . ms) '()) (T `(,A-macro ,ms (a))))
                ((A-macro ms ns)
                 (let* ((inner (cons 'A ns))
                        (inner-expanded (expand inner)))
                  ;(printf "~A is the inner expansion of ~A~N" inner-expanded inner)
                  (cons (car expanded) (T inner)))))))))
      sexp))

(newline)
(newline)

    (define-syntax tabling
     (syntax-rules (before ref store else)
      ((tabling <table> (before <command>) ... (ref <key>)) 
       (let ((memo (hash-table-ref <table> <key>)))
        <command> ...
        memo)) 
      ((tabling <table> (before <command>) ... (ref <key>) (else <comp>))
       (begin 
        <command> ... 
        (if (hash-table-exists? <table> <key>) 
         (hash-table-ref <table> <key>) 
         (tabling <table> (store <comp> at <key>)))))
      ((tabling <table> (before <command>) ... (store <val> at <key>)) 
       (begin
        <command> ...
        (hash-table-set! <table> <key> <val>)
        <val>))))

    (define restart (lambda () 'no-computation-to-restart))

    (define ackermann-expander
     (lambda (starting-sexp)
      (let ((table (make-hash-table)))
       (letcc hop
        (let E ((starting-sexp starting-sexp)
                (sexp starting-sexp) 
                (tabs "") 
                (sender "☻"))
         (printf "~A ~A → " sender sexp)
         (cond
          ((and (not (equal? sexp starting-sexp)) (equal? tabs "")) 
           (call/cc (lambda (cont)
                     (set! restart (lambda (injected-sexp)
                                    (let ((restarting-sexp (if (atom? injected-sexp) sexp injected-sexp)))
                                     (cont (E restarting-sexp restarting-sexp "" "☺")))))
                     (hop sexp))))
          ((hash-table-exists? table sexp) (tabling table (before (printf "★ ")) (ref sexp)))
          (else (match sexp
                 (('A 0 n)
                  (let* ((arith `(add1 ,n)))
                   (tabling table 
                    (before (printf "~A ⇒ " `(eval ',arith))) 
                    (ref sexp)
                    (else (eval arith)))))
                 (('A m 0)
                  (let ((forward `(A ,(sub1 m) 1)))
                   (tabling table 
                    (before (printf "■ ~A~N~A" forward tabs)) 
                    (ref forward) 
                    (else (E starting-sexp forward tabs "■")))))
                 (('A m n)
                  (let* ((inner `(A ,m ,(sub1 n)))
                         (expanded-inner (tabling table 
                                          (ref inner) 
                                          (else (E starting-sexp inner (string-append " " tabs) "○"))))
                         (whole-unexpanded `(A ,(sub1 m) ,inner))
                         (whole `(A ,(sub1 m) ,expanded-inner)))
                   (tabling table 
                    (before (printf "● ~A ≡ ~A~N~A" whole-unexpanded whole tabs)) 
                    (ref whole) 
                    (else (E starting-sexp whole (string-append "" tabs) "●")))))))))))))

; in order to see Ackermann reductions type the following in the REPL:
; (ackermann-expander '(A 3 6))
; if you want timing info too:
; (time (ackermann-expander '(A 3 6)))
; finally, to restart the natural unwind of recursion stack then type:
; (restart 'any-atom-should-work)
; moreover, if you want to restart the computation with a different Ackermann form <A-form> then:
; > (restart <A-form>) ; where <A-form> = '(A n m) for some integers n and m
