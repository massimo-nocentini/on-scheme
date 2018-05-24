
(import chicken scheme)
(use test unionfind)

    (with-unionfind
     (lambda (↑ ≡)
      (test 'absent  (↑ 'absent))
      (test '(a b)   (↑ '(a b))) ; this example doesn't work in Python because lists are *not* hashable objects
      (≡ 3 4)
      (test 4 (↑ 3))
      (test 4 (↑ 4))))
