

(import chicken scheme)

(use srfi-1 srfi-13)
(use test numbers)

(use commons streams microkanren)

(test-fail (equal? (V 'V0) (fresh₁ identity)))
(test-fail (equal? (V 'V₁) (fresh₁ identity)))
(test-fail (equal? (fresh₁ identity) (fresh₁ identity)))
(test 'V₄ (fresh₁ variable->symbol))
(test 'V₅ (fresh₁ variable->symbol))

(test '()   (stream:->list (run ✗)))
(test '(#t) (stream:->list (run ✓)))
(test '()   (stream:->list (run (≡ 2 3))))
(test '(#t) (stream:->list (run (≡ 3 3))))
(test '(#t) (stream:->list (run (fresh (v) ✓))))
(test '(#t) (stream:->list (run (fresh (v) (≡ v 3)))))
(test '() (stream:->list (run (fresh (v) 
                               (≡ v 3)
                               (≡ v 4)))))
(test '(3) (stream:->list (run (v) (≡ v 3))))
(test '(3) (stream:->list (run (v) (≡ 3 v))))
(test '(3) (stream:->list (run (v) (≡ '(3 4) `(,v 4)))))
(test (list (R '▢ 0)) (stream:->list (run (v) ✓)))
(test '(▢₀) ((get/variable->symbol stream:->list) (run (v) ✓)))
(test '((▢₀ ▢₁)) ((get/variable->symbol stream:->list) (run (v w) ✓)))
(test '((▢₀ ▢₀)) ((get/variable->symbol stream:->list) (run (v w) (≡ v w))))
(test '((▢₀ (((▢₀))))) ((get/variable->symbol stream:->list) (run (v w) (≡ `(((,v))) w))))

