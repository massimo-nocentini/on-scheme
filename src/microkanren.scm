
(module microkanren *

 (import chicken scheme)

 (use datatype data-structures srfi-69 ports)

 (use commons streams unionfind)

 (define-record status ≡)

 (define status-copy (○ make-status unionfind-copy status-≡))

 (define ε
  (lambda ()
   (make-status (unionfind-empty))))

 (define-datatype variable variable?
  (V (s symbol?)) ; for *working* logic variables
  (R (r symbol?) (n number?)) ; for *reified* logic variables
 )

 (define-record-printer variable
  (lambda (v out)
   (cases variable v
    (V (s) ((○ (display-on-port out) symbol∼subscripts) s))
    (R (r n) ((○ (display-on-port out) symbol∼subscripts)
              (symbol-append r (number->symbol n)))))))

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

 (define-syntax ∨
  (syntax-rules ()
   ((∨) ✗)
   ((∨ g) g)
   ((∨ g₀ g ...) (∨₂ g₀ (∨ g ...)))))

 (define fresh₁
  (lambda (recv)
   (recv (V (gensym 'V)))))

 (define-syntax fresh
  (syntax-rules ()

   ((fresh () g ...) (∧ g ...))

   ((fresh (v₀ v ...) g ...)
    (fresh₁ (lambda (v₀) (fresh (v ...) g ...))))))

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
        ((walk* (unionfind-↑! (status-≡ s₀))) v₀)))))

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

   ((run taut)
    (run (q) (∧ taut (≡ q #t))))

   ((run (q) g ...) (run q g ...))

   ((run (v₀ v ...) g ...)
    (run q (fresh (v₀ v ...)
            g ...
            (≡ q (list v₀ v ...)))))

   ((run q g ...)
    (fresh₁ (lambda (q)
             ((stream:map (reify/var q)) ((∧ g ...) (ε))))))

  ))

    (define get/variable->symbol
     (lambda (get)
      (○ get (stream:map variable->symbol))))

)
