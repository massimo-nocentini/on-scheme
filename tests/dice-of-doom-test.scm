
(import chicken scheme)

(use srfi-1 srfi-13)
(use test numbers)

(use commons dice-of-doom)


    #;(define fibonacci
     (letrec ((F (lambda (i)
                  (cond
                   ((zero? i) 0)
                   (else (+ (F (- i 1)) (F (- i 2))))))))
      (memoize F)))

    (define F
     (lambda (i)
      (cond
       ((< i 2) i)
       (else (+ (F (- i 1)) (F (- i 2)))))))

(define-tabled F₀
 (lambda (i)
  (cond
   ((zero? i) 0)
   ((one? i) 1)
   (else (+ (F₀ (- i 1)) (F₀ (- i 2)))))))

(test 0 (F₀ 0))
(test 1 (F₀ 1))
(test '(0 1 1 2 3 5 8 13 21) (map F₀ (iota 9)))
(time (test 1346269 (F₀ 31)))

(define-tabled pascal
 (lambda (n k)
  (cond
   ((and (zero? n) (zero? k)) 1)
   ((zero? n) 0)
   ((zero? k) (pascal (sub1 n) 0))
   (else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))))))

(define-tabled catalan
 (lambda (n k)
  (cond
   ((and (zero? n) (zero? k)) 1)
   ((zero? n) 0)
   ((zero? k) (apply + (map (lambda (j) (catalan (sub1 n) j)) (iota n))))
   (else (apply + (map (lambda (j) (catalan (sub1 n) j)) (iota n (sub1 k))))))))

(test 1 (pascal 0 0))
(test 2 (pascal 2 1))
(test 1 (pascal 2 2))
(test 3 (pascal 3 2))
(test 100891344545564193334812497256 (pascal 100 50))

    (define Riordan-array
     (lambda (recurrence)
      (lambda (m)
       (map (lambda (n) 
             (append-map (lambda (k) (list (recurrence n k)))
              (iota (add1 n)))) 
        (iota m)))))

    (test '((1) 
            (1 1) 
            (1 2 1) 
            (1 3 3 1) 
            (1 4 6 4 1) 
            (1 5 10 10 5 1) 
            (1 6 15 20 15 6 1) 
            (1 7 21 35 35 21 7 1) 
            (1 8 28 56 70 56 28 8 1) 
            (1 9 36 84 126 126 84 36 9 1) 
            (1 10 45 120 210 252 210 120 45 10 1) 
            (1 11 55 165 330 462 462 330 165 55 11 1) 
            (1 12 66 220 495 792 924 792 495 220 66 12 1) 
            (1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1) 
            (1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1) 
            (1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1) 
            (1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1) 
            (1 17 136 680 2380 6188 12376 19448 24310 24310 19448 12376 6188 2380 680 136 17 1) (1 18 153 816 3060 8568 18564 31824 43758 48620 43758 31824 18564 8568 3060 816 153 18 1) (1 19 171 969 3876 11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876 969 171 19 1)) 
     ((Riordan-array pascal) 20))

    (test '((1) 
            (1 1) 
            (2 2 1) 
            (5 5 3 1) 
            (14 14 9 4 1) 
            (42 42 28 14 5 1)
            (132 132 90 48 20 6 1) 
            (429 429 297 165 75 27 7 1) 
            (1430 1430 1001 572 275 110 35 8 1) 
            (4862 4862 3432 2002 1001 429 154 44 9 1) 
            (16796 16796 11934 7072 3640 1638 637 208 54 10 1) 
            (58786 58786 41990 25194 13260 6188 2548 910 273 65 11 1) 
            (208012 208012 149226 90440 48450 23256 9996 3808 1260 350 77 12 1) 
            (742900 742900 534888 326876 177650 87210 38760 15504 5508 1700 440 90 13 1) 
            (2674440 2674440 1931540 1188640 653752 326876 149226 62016 23256 7752 2244 544 104 14 1) 
            (9694845 9694845 7020405 4345965 2414425 1225785 572033 245157 95931 33915 10659 2907 663 119 15 1) 
            (35357670 35357670 25662825 15967980 8947575 4601610 2187185 961400 389367 144210 48279 14364 3705 798 135 16 1) 
            (129644790 129644790 94287120 58929450 33266625 17298645 8351070 3749460 1562275 600875 211508 67298 19019 4655 950 152 17 1) 
            (477638700 477638700 347993910 218349120 124062000 65132550 31865925 14567280 6216210 2466750 904475 303600 92092 24794 5775 1120 170 18 1) 
            (1767263190 1767263190 1289624490 811985790 463991880 245642760 121580760 56448210 24582285 10015005 3798795 1332045 427570 123970 31878 7084 1309 189 19 1)) 
     ((Riordan-array catalan) 20))

(test 0 (F 0))
(test 1 (F 1))
(test '(0 1 1 2 3 5 8 13 21) (map F (iota 9)))
(time (test 1346269 (F 31)))

(define fibonacci
 (letrec ((F (lambda (i)
              (cond
               ((< i 2) i)
               (else (+ (F (- i 1)) (F (- i 2))))))))
  (memoize F)))

(define F₁
 (memoize (lambda (i)
              (cond
               ((< i 2) i)
               (else (+ (F₁ (- i 1)) (F₁ (- i 2))))))))

(time (test 354224848179261915075 (fibonacci 100)))
(time (test 1346269 (F₁ 31))) 

(randomize 1024)

(test-group "initial"
 (test
  "\n    A-1 B-1 \n  B-3 A-3 "
  (to-string (gen-board 2 '(A B) 4))) ; 'A and 'B play on 2x2 board starting with at most 4 dices on each cell.
 (test
  #((B 2) (B 2) (B 4) (B 1))
  (board-cells (gen-board 2 '(A B) 4))) ; 'A and 'B play on 2x2 board starting with at most 4 dices on each cell.

 (test '(2 1 3) (neighbors 0 (gen-board 2 '(A B) 4)))
 (test '(3 0)   (neighbors 1 (gen-board 2 '(A B) 4)))
 (test '(0 3)   (neighbors 2 (gen-board 2 '(A B) 4)))
 (test '(1 0 2) (neighbors 3 (gen-board 2 '(A B) 4)))

 (test #((A 3) (A 1) (B 3) (A 2))
  (board-cells
   (board-attack
    (make-board #((A 3) (A 3) (B 3) (B 1)) 2)
    'A 1 3 3)))

 (test #((A 2) (B 3) (A 3) (B 1))
  (board-cells (add-new-dice (make-board #((A 1) (B 3) (A 2) (B 1)) 2) 'A 2)))

    (test '(A "\n    A-1 B-1 \n  A-2 B-1 "
            (((cell 2 attacks 3) (A "\n    A-1 B-1 \n  A-1 A-1 "
                                  ((pass (B "\n    A-1 B-1 \n  A-1 A-1 "
                                          ())))))))
     ((map/tree (lambda (sexp)
                 (cond
                  ((board? sexp) (to-string sexp))
                  (else sexp))))
      (game-tree
       (make-board #((A 1) (B 1) (A 2) (B 1)) 2)
       (apply circular-list '(A B)) 0 #t)))

    (test '(A) (winners (make-board #((A 1) (B 1) (A 1) (A 1)) 2)))

    #;(test '()
     (with-output-to-string
      (lambda ()
       (let ((board (gen-board 3 '(A B) 3))
             (players (apply circular-list '(A B))))
        (computer-vs-computer (game-tree board players 0 #t))))))
)


(display (gen-board 5 '(A B) 4))

(newline)
    (let ((board (gen-board 3 '(A B) 3))
          (players (apply circular-list '(A B))))
     (computer-vs-computer (game-tree board players 0 #t)))
(newline)

;------------------------------------------------------------------------
(test-exit)
