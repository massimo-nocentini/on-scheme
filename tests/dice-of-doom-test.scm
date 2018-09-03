
(import chicken scheme)

(use srfi-1 srfi-13)
(use test numbers)

(use commons streams dice-of-doom)

(randomize 1024)

(test-group "initial"
 (test
  "\n    A-1 B-1 \n  B-3 A-3 "
  (to-string (gen-board 2 '(A B) 4))) ; 'A and 'B play on 2x2 board starting with at most 4 dices on each cell.
 (test
  #((B 2) (B 2) (B 4) (B 1))
  (board-cells (gen-board 2 '(A B) 4))) ; 'A and 'B play on 2x2 board starting with at most 4 dices on each cell.

 (test '(2 1 3) (stream:->list (neighbors 0 (gen-board 2 '(A B) 4))))
 (test '(3 0)   (stream:->list (neighbors 1 (gen-board 2 '(A B) 4))))
 (test '(0 3)   (stream:->list (neighbors 2 (gen-board 2 '(A B) 4))))
 (test '(1 0 2) (stream:->list (neighbors 3 (gen-board 2 '(A B) 4))))

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
    (let ((board (gen-board 5 '(A B) 3))
          (players (apply circular-list '(A B))))
     (computer-vs-computer (game-tree board players 0 #t)))
(newline)

;------------------------------------------------------------------------
(test-exit)
