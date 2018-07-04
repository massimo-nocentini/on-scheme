
(module microkanren *

 (import chicken scheme)

 (use datatype data-structures srfi-69 ports)

 (use commons streams)

 (define-datatype variable
  (V (s symbol?)) ; for *working* logic variables
  (R (r symbol?)) ; for *reified* logic variables
 )

 (define-record-printer variable
  (lambda (v out)
   (cases variable v
    (V (s) ((○ (display-on-port out) symbol∼subscripts) s)))))

 (define variable->symbol
  (lambda (v)
   (string->symbol
    (with-output-to-string (lambda () (display v))))))

 (define ✓ stream:singleton)
 (define ✗ (Λ (s) stream:empty))

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

 (define-syntax run
  (syntax-rules ()

   ((run taut)
    (run (q) (∧ taut (≡ q #t))))

   ((run (v₀ v ...) g ...)
    (run q (fresh (v₀ v ...)
            g ...
            (≡ (list v₀ v ...) q))))

   ((run q g ...)
    (fresh₁ (lambda (q)
             ((stream:map (reify q)) (∧ g ...)))))

  ))

)
