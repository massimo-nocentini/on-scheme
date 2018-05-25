
(import chicken scheme)
(use test unionfind ports)

    (with-unionfind
     (lambda (U ↑ ≡)
      (test 'absent  (↑ 'absent))
      (test '(a b)   (↑ '(a b))) ; this example doesn't work in Python because lists are *not* hashable objects
      (test "" (with-output-to-string (lambda () (display U))))
      (≡ 3 4)
      (test 4 (↑ 3))
      (test 4 (↑ 4))
      (test "(4 -> 4)\n(3 -> 4)\n" (with-output-to-string (lambda () (display U))))
      ))
