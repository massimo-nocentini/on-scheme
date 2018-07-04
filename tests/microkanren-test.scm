

(import chicken scheme)

(use srfi-1 srfi-13)
(use test numbers)

(use commons streams microkanren)

(test-fail (equal? (V 'V0) (fresh₁ identity)))
(test-fail (equal? (V 'V₁) (fresh₁ identity)))
(test 'V₂ (fresh₁ variable->symbol))
(test 'V₃ (fresh₁ variable->symbol))

(test '(#t) (run ✓))
