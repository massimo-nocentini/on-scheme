
    (import scheme (chicken base))
    (import test)
    (import collatz)

    (test-group "Collatz 3x+1 problem"

     (let ((expected '(51 154 77 232 116 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)))
      (test expected (collatz 51))
      (test expected (collatz&co 51))
      (test expected (collatz&co-abridged 51))
      (test expected (collatz&co-abridged2 51))
      (test expected (collatz&cc 51))
      (test expected (collatz&cc-abridged 51)))

    ) ; end of tests group

(test-exit)
