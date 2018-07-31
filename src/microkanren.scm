
(module microkanren *

 (import chicken scheme)

 (use datatype data-structures srfi-69 ports)

 (use commons streams unionfind)

 (define-record status ≡ depth)

 (define-record-printer status
  (lambda (s out)
   (display (status-≡ s) out)
   (display ((○ number->string status-depth) s) out)))

 (define status-copy
  (lambda (s)
   (make-status
    ((○ unionfind-copy status-≡) s)
    ((○ identity status-depth) s))))

 (define ε
  (lambda ()
   (make-status (unionfind-empty) 0)))

 (define-datatype variable variable?
  (V (s symbol?)) ; for *working* logic variables
  (R (r symbol?) (n number?)) ; for *reified* logic variables
 )

    (define-record-printer variable
     (lambda (v out)
      (let ((subscripts&display (○ (display-on-port out) symbol∼subscripts)))
       (cases variable v
        (V (s) (subscripts&display s))
        (R (r n) (subscripts&display (symbol-append r (number->symbol n))))))))

    (define variable->symbol
     (lambda (v)
      (cond
       ((variable? v)
        (string->symbol
         (with-output-to-string (lambda () (display v)))))
       ((pair? v) (cons
                   (variable->symbol (car v))
                   (variable->symbol (cdr v))))
       (else v))))

 (define ✓ stream:singleton)
 (define ✗ (Λ (s) stream:empty))

    (define ∧₂
     (lambda (g₁ g₂)
      (Λ (s)
       (stream:>>= (g₁ s) (stream:repeat g₂)))))

 (define-syntax ∧
  (syntax-rules ()
   ((∧) ✓)
   ((∧ g) g)
   ((∧ g₀ g ...) (∧₂ g₀ (∧ g ...)))))

    (define ∨
     (lambda (m+)
      (lambda goals
       (Λ (s)
        (let ((r (make-status (status-≡ s) ((○ add1 status-depth) s))))
         (apply m+ (map ($ r) goals)))))))

    (define V/gensym
     (lambda ()
      (V (gensym 'V))))

    (define fresh₁
     (lambda (recv)
      (Λ (s)
       ((recv (V/gensym)) s))))

 (define-syntax fresh
  (syntax-rules ()
   ((fresh () g) (Λ (s) (g s))) ; delaying `g` application
   ((fresh (v₀ v ...) g) (fresh₁ (lambda (v₀) (fresh (v ...) g))))))

 (define R/unionfind-size
  (lambda (U)
   (R '▢ (unionfind-#edges U))))

 (define reify/status
  (lambda (v s)
   (let ((U (status-≡ s)))
    (unionfind-accessors U
     (lambda (↑ ↑! ∪ →)
      (letrec ((R/S (lambda (v)
                     (let ((v₀ (↑ v)))
                      (cond
                       ((variable? v₀) (∪ v₀ (R/unionfind-size U)))
                       ((pair? v₀) (begin
                                    (R/S (car v₀))
                                    (R/S (cdr v₀))))
                       (else 'useless))))))
       (begin (R/S v) s)))))))

    (define walk*
     (lambda (↑)
      (letrec ((W* (lambda (v)
                    (let ((v₀ (↑ v)))
                     (cond
                      ((variable? v₀) v₀)
                      ((pair? v₀) (cons (W* (car v₀)) (W* (cdr v₀))))
                      (else v₀))))))
       W*)))

    (define reify/var
     (lambda (v)
      (lambda (s)
       (let* ((v₀ ((walk* (unionfind-↑ (status-≡ s))) v))
              (s₀ (reify/status v₀ (ε))))
        (values
         ((walk* (unionfind-↑! (status-≡ s₀))) v₀)
         (status-depth s))))))

    (define unify
     (lambda (∪ ↑)
      (letrec ((U (lambda (v w)
                   (let ((v₀ (↑ v))
                         (w₀ (↑ w)))
                    (cond
                     ((eqv? v₀ w₀) #t)
                     ((variable? v₀) (begin (∪ v₀ w₀) #t))
                     ((variable? w₀) (begin (∪ w₀ v₀) #t))
                     ((and (pair? v₀) (pair? w₀))
                      (and
                       (U (car v₀) (car w₀))
                       (U (cdr v₀) (cdr w₀))))
                     (else #f))))))
       U)))

    (define ≡
     (lambda (v w)
      (Λ (s)
       (let ((S (status-copy s)))
        (unionfind-accessors (status-≡ S)
         (lambda (↑ ↑! ∪ →)
          (cond
           (((unify ∪ ↑) v w) (stream:singleton S))
           (else stream:empty))))))))

 (define-syntax run
  (syntax-rules ()
   ((run taut) (run (q) (∧ taut (≡ q #t))))
   ((run (q) g) (run q g))
   ((run (v₀ v ...) g) (run q (fresh (v₀ v ...)
                               (∧ g (≡ q (list v₀ v ...))))))
   ((run q g) (let ((q (V/gensym)))
               ((stream:map (reify/var q)) (g (ε)))))
  ))

    (define get/variable->symbol
     (lambda (get)
      (○ get (stream:map variable->symbol))))

    (define-syntax run/with-symbols
     (syntax-rules (∞ ↓)

      ((run/with-symbols ↓ ∞ sexp ...) ((get/variable->symbol stream:->list) (run sexp ...)))
      ((run/with-symbols ↓ n sexp ...) ((get/variable->symbol (list○take n)) (run sexp ...)))

      ((run/with-symbols #t sexp ...)
       ((lambda (α) (if (stream:null? α) #f (stream:car α))) (run sexp ...)))

      ((run/with-symbols body ...)
       (map car (run/with-symbols ↓ body ...)))))

 (define deepening
  (lambda (depth)
   (lambda (g)
    (Λ (s)
     ((stream:iterative-deepening 0 depth status-depth) (g s))))))

    (define ifº/¦
     (lambda (question answer otherwise)
      ((∨ stream:append) (∧ question answer) otherwise)))

    (define ifº/§
     (lambda (question answer otherwise)
      (Λ (s) 
       (let ((g ((∨ stream:§) (∧ question answer) otherwise)))
        (g s)))))

    (define ifº/!
     (lambda (question answer otherwise)
      (Λ (s)
       (let ((α (question s)))
        (cond
         ((stream:null? α) (otherwise s))
         (else (stream:>>= α (stream:repeat answer))))))))

    (define ifº/!!
     (lambda (question answer otherwise)
      (Λ (s)
       (let ((α (question s)))
        (stream:dest/car+cdr α
         ((α₀ α₊) (answer α₀))
         (else (otherwise s)))))))

    (define-syntax condº
     (syntax-rules ()
      ((condº ifº (question answer ...))
       (ifº question (∧ answer ...) ✗))
      ((condº ifº (question answer ...) otherwise ...)
       (ifº question (∧ answer ...) (condº ifº otherwise ...)))))

    (define-syntax condº/¦
     (syntax-rules ()
      ((condº/∞ sexp ...) (condº ifº/¦ sexp ...))))

    (define-syntax condº/§
     (syntax-rules ()
      ((condº/§ sexp ...) (condº ifº/§ sexp ...))))

    (define-syntax condº/!
     (syntax-rules ()
      ((condº/! sexp ...) (condº ifº/! sexp ...))))

    (define-syntax condº/!!
     (syntax-rules ()
      ((condº/!! sexp ...) (condº ifº/!! sexp ...))))
)
