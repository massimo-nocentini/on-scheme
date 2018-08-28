
(module dice-of-doom *

 (import chicken scheme)

 (use srfi-1)
 (use numbers extras loop)
 (use commons)

#;(defparameter *num-players* 2)
#;(defparameter *max-dice* 3)
#;(defparameter *board-size* 2)
#;(defparameter *board-hexnum* (* *board-size* *board-size*))
#;(defun player-letter (n)
  (code-char (+ 97 n)))

 (define-record board cells size)
 (define-record game board players)

 (define num-players (○ length game-players))
 (define board-cell@ (lambda (b) (fvector-ref (board-cells b))))
 (define board-player@cell (lambda (b) (○ first (board-cell@ b))))
 (define board-dices@cell (lambda (b) (○ second (board-cell@ b))))
 (define board-hexnum (○ ² board-size))

#;(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

#;(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

 (define gen-board
  (lambda (size players dices)
   (let* ((select-player (flist-ref players))
          (num-players (length players))
          (choose-player (○ select-player random (K num-players)))
          (assign-dices (○ add1 random (K dices)))
          (ctors (list choose-player assign-dices))
          (R (lambda (i) (map ($ i) ctors))))
    (make-board (list->vector (list-tabulate (² size) R)) size))))

#;(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                              (second hex))))))

    (define-record-printer board
     (lambda (b out)
      (let ((size (board-size b)))
       (loop for y below size
        do (begin
            (newline)
            (loop repeat (- size y) do (display "  "))
            (loop
             for x below size
             for hex = ((board-cell@ b) (+ x (* size y)))
             do (format out "~a-~a " (first hex) (second hex))))))))

#;(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

    (define-tabled game-tree
     (lambda (board players spare-dice first-move)
      (let* ((attack-moves (attacking-moves board players spare-dice))
             (moves (add-passing-move board players spare-dice first-move attack-moves)))
       (list (car players) board moves))))

#;(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

    (define add-passing-move
     (lambda (board players spare-dice first-move moves)
      (cond
       (first-move moves)
       (else (let* ((new-board (add-new-dice board (car players) (sub1 spare-dice)))
                    (tree (game-tree new-board (cdr players) 0 #t)))
              (cons (list 'pass tree) moves))))))

#;(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                (> (dice src) (dice dst)))
                          (list
     (list (list src dst)
           (game-tree (board-attack board cur-player src dst (dice src))
                      cur-player
                      (+ spare-dice (dice dst))
                      nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

    (define attacking-moves
     (lambda (board players spare-dice)
      (let ((S (lambda (src)
                (let ((player (car players)))
                 (cond
                  ((equal? ((board-player@cell board) src) player)
                   (let ((D (lambda (dst)
                             (let ((dices@src-cell ((board-dices@cell board) src))
                                   (dices@dst-cell ((board-dices@cell board) dst)))
                              (cond
                               ((and
                                 (≠ ((board-player@cell board) dst) player)
                                 (> dices@src-cell dices@dst-cell))
                                (list
                                 (list
                                  `(cell ,src attacks ,dst) ; description
                                  (game-tree
                                   (board-attack board player src dst dices@src-cell)
                                   players (+ spare-dice dices@dst-cell) #f))))
                               (else '()))))))
                    (append-map D (neighbors src board))))
                  (else '()))))))
       (append-map S (iota (board-hexnum board))))))

#;(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))

    (define-tabled neighbors
     (lambda (pos board)
      (let* ((size (board-size board))
             (up (- pos size))
             (down (+ pos size))
             (divisible-by-size? (lambda (v) (zero? (modulo v size))))
             (l₁ (cond
                  ((not (divisible-by-size? pos)) (list (sub1 up) (sub1 pos)))
                  (else '())))
             (l₂ (cond
                  ((not (divisible-by-size? (add1 pos))) (list (add1 pos) (add1 down)))
                  (else '())))
             (l (append (list up down) l₁ l₂)))
       (loop for p in l
        when (and (>= p 0) (< p (board-hexnum board)))
        collect p))))

#;(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

    (define board-attack
     (lambda (board player src dst dice)
      (let ((cells (loop for pos from 0
                         for hex across (board-cells board)
                         collect (cond
                                  ((equal? pos src) (list player 1))
                                  ((equal? pos dst) (list player (sub1 dice)))
                                  (else hex)))))
       (make-board (list->vector cells) (board-size board)))))

#;(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
               (t (let ((cur-player (caar lst))
                        (cur-dice (cadar lst)))
                    (if (and (eq cur-player player) (< cur-dice *max-dice*))
                        (cons (list cur-player (1+ cur-dice))
                              (f (cdr lst) (1- n)))
                        (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

    (define add-new-dice₀ ; not a tail call definition.
     (lambda (board player spare-dice)
      (letrec ((F (lambda (lst n)
                   (cond
                    ((null? lst) '())
                    ((zero? n) lst)
                    (else (let ((cur-player (caar lst))
                                (cur-dice (cadar lst)))
                           (cond
                            ((equal? cur-player player) (cons (list cur-player (add1 cur-dice)) (F (cdr lst) (sub1 n))))
                            (else (cons (car lst) (F (cdr lst) n))))))))))
       (let ((cells (F ((○ vector->list board-cells) board) spare-dice)))
        (make-board (list->vector cells) (board-size board))))))

#;(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

#;(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(define-record gametree player board moves)

    (define-record-printer gametree
     (lambda (tree out)
      (format out "\nCurrent player: ~a\nBoard:~a"
       (gametree-player tree) (gametree-board tree))))

#;(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a -> ~a" (car action) (cadr action))
                   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

#;(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply max (mapcar cdr totals))))
    (mapcar car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

    (define winners
     (lambda (board)
      (let* ((tally (loop for hex across (board-cells board) collect (car hex)))
             (C (lambda (player) (cons player (count (equals-to? player) tally))))
             (totals (map C (remove-duplicates tally)))
             (best (apply max (map cdr totals))))
       (map car (remove (lambda (x) (≠ (cdr x) best)) totals)))))

#;(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar player-letter w))
      (format t "The winner is ~a" (player-letter (car w))))))

    (define announce-winner
     (lambda (board)
      (newline)
      (let ((w (winners board)))
       ((K w)
        (cond
         ((> (length w) 1) (format #t "The game is a tie between ~a" w))
         (else (format #t "The winner is ~a" (car w))))))))

;To play against a human:
;
;(play-vs-human (game-tree (gen-board) 0 0 t))

;The code below adds the AI player

#;(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
      (apply (if (eq (car tree) player)
               max
             min)
             (get-ratings tree player))
      (let ((w (winners (cadr tree))))
      (if (member player w)
          (/ 1 (length w))
        0)))))

    (define-tabled rate-position
     (lambda (player)
      (letrec-tabled ((R (lambda (tree)
                          (let ((moves (caddr tree)))
                           (cond
                            ((pair? moves) (let* ((other (car tree))
                                                  (opt (cond
                                                        ((equal? other player) max)
                                                        (else min))))
                                            (apply opt (get-ratings tree player))))
                            (else (let ((w (winners (cadr tree))))
                                   (cond
                                    ((member player w) (⁻¹ (length w)))
                                    (else 0)))))))))
      R)))

#;(defun get-ratings (tree player)
  (mapcar (lambda (move)
          (rate-position (cadr move) player))
        (caddr tree)))

    (define get-ratings
     (lambda (tree player)
      (map (lambda (move)
            ((rate-position player) (cadr move)))
       (caddr tree))))

#;(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply max ratings) ratings) (caddr tree)))))

    (define handle-computer
     (lambda (tree)
      (let* ((ratings (get-ratings tree (car tree)))
             (n (list-index (equals-to? (apply max ratings)) ratings))) ; here we can add non-determinism when there is more than one maximum.
       (cadr (list-ref (caddr tree) n)))))

#;(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
      ((zerop (car tree)) (play-vs-computer (handle-human tree)))
      (t (play-vs-computer (handle-computer tree)))))

(define computer-vs-computer
 (lambda (tree)
  (display (make-gametree (car tree) (cadr tree) (caddr tree)))
  (cond
   ((null? (caddr tree)) (announce-winner (cadr tree)))
   (else (computer-vs-computer (handle-computer tree))))))

;To play against the computer:
;
;(play-vs-computer (game-tree (gen-board) 0 0 t))

;The code below optimizes the game and allows play on a 3x3 board

#;(defparameter *board-size* 3)
#;(defparameter *board-hexnum* (* *board-size* *board-size*))

#;(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

#;(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
      (setf (gethash rest previous) (apply old-game-tree rest)))))

#;(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))

#;(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice)) acc))
                          (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

    (define add-new-dice
     (lambda (board player spare-dice)
      (letrec ((F (lambda (lst n acc)
                   (cond
                    ((null? lst) (reverse acc))
                    ((zero? n) (append (reverse acc) lst))
                    (else (let ((cur-player (caar lst))
                                (cur-dice (cadar lst)))
                           (cond
                            ((equal? cur-player player) (F (cdr lst) (sub1 n) (cons (list cur-player (add1 cur-dice)) acc)))
                            (else (F (cdr lst) n (cons (car lst) acc))))))))))
       (let ((cells (F ((○ vector->list board-cells) board) spare-dice '())))
        (make-board (list->vector cells) (board-size board))))))

)
