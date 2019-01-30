
(module dice-of-doom *

 (import scheme (chicken base) (chicken format) (chicken fixnum) (chicken random))
 (import srfi-1)
 (import loop)
 (import commons streams series)

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
                      collect (list (pseudo-random-integer *num-players*)
                          (1+ (pseudo-random-integer *max-dice*))))))

 (define gen-board
  (lambda (size players dices)
   (let* ((select-player (flist-ref players))
          (num-players (length players))
          (choose-player (○ select-player pseudo-random-integer (K num-players)))
          (assign-dices (○ add1 pseudo-random-integer (K dices)))
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
         (lazy-cons (list nil
                     (game-tree (add-new-dice board player
                                 (1- spare-dice))
                      (mod (1+ player) *num-players*)
                      0
                      t))
          moves)))

    (define add-passing-move
     (Λ (board players spare-dice first-move moves)
      (cond
       (first-move moves)
       (else (let* ((new-board (add-new-dice board (car players) (sub1 spare-dice)))
                    (tree (game-tree new-board (cdr players) 0 #t)))
              (stream:cons (list 'pass tree) moves))))))

#;(defun attacking-moves (board cur-player spare-dice)
        (labels ((player (pos) (car (aref board pos)))
                 (dice (pos) (cadr (aref board pos))))
         (lazy-mapcan
          (lambda (src)
           (if (eq (player src) cur-player)
            (lazy-mapcan
             (lambda (dst)
              (if (and (not (eq (player dst)
                             cur-player))
                   (> (dice src) (dice dst)))
               (make-lazy
                (list (list (list src dst)
                       (game-tree (board-attack board
                                   cur-player
                                   src
                                   dst
                                   (dice src))
                        cur-player
                        (+ spare-dice (dice dst))
                        nil))))
               (lazy-nil)))
             (make-lazy (neighbors src)))
            (lazy-nil)))
    (make-lazy (loop for n below *board-hexnum*
                collect n)))))

    (define attacking-moves
     (Λ (board players spare-dice)
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
                                 (stream:singleton ; replaces: (stream:->list (list ...))
                                  (list
                                   `(cell ,src attacks ,dst) ; description
                                   (game-tree
                                    (board-attack board player src dst dices@src-cell)
                                    players (+ spare-dice dices@dst-cell) #f))))
                               (else stream:empty))))))
                    ((stream:append-map D) (neighbors src board))))
                  (else stream:empty))))))
       ((stream:append-map S) (series:range 0 (board-hexnum board))))))

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
       (list->stream
        (loop for p in l
         when (and (>= p 0) (< p (board-hexnum board)))
         collect p)))))

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
        (if (not (lazy-null (caddr tree)))
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
         (labels ((print-moves (moves n)
                   (unless (lazy-null moves)
                    (let* ((move (lazy-car moves))
                           (action (car move)))
                     (fresh-line)
                     (format t "~a. " n)
                     (if action
                      (format t "~a -> ~a" (car action) (cadr action))
                      (princ "end turn")))
                    (print-moves (lazy-cdr moves) (1+ n)))))
          (print-moves moves 1))
         (fresh-line)
         (cadr (lazy-nth (1- (read)) moves))))

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

#;(defun rate-position/for-tabling (tree player)
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

    (define-tabled rate-position/tabled
     (lambda (player)
      (letrec-tabled ((R (lambda (tree)
                          (let ((moves (caddr tree)))
                           (cond
                            (((○ not stream:null?) moves) (let* ((other (car tree))
                                                                 (opt (if (equal? other player) max min)))
                                                           (apply opt (get-ratings tree player))))
                            (else (let ((w (winners (cadr tree))))
                                   (cond
                                    ((member player w) (⁻¹ (length w)))
                                    (else 0)))))))))
      R)))

#;(defun rate-position/for-streams (tree player)
        (let ((moves (caddr tree)))
         (if (not (lazy-null moves))
          (apply (if (eq (car tree) player) max min)
           (get-ratings tree player))
          (score-board (cadr tree) player))))

    (define rate-position
     (lambda (player)
      (lambda (tree)
       (let ((moves (caddr tree)))
        (cond
         ((not (stream:null? moves)) (let ((opt (if (equal? (car tree) player) max min)))
                                      (apply opt (get-ratings tree player))))
         (else (score-board (cadr tree) player)))))))

#;(defun get-ratings (tree player)
        (take-all (lazy-mapcar (lambda (move)
                                (rate-position (cadr move) player))
                   (caddr tree))))

    (define get-ratings
     (lambda (tree player)
      (let ((R (○ (rate-position player) cadr))
            (moves (caddr tree)))
       ((○ stream:->list (stream:map R)) moves))))

#;(defparameter *ai-level* 4)

#;(defun handle-computer (tree)
        (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                        (car tree)
                        most-positive-fixnum
                        most-negative-fixnum)))
         (cadr (lazy-nth (position (apply max ratings) ratings) (caddr tree)))))

    (define handle-computer
     (lambda (tree #!key (handler identity))
      (let* ((tree (handler tree))
             (ratings (get-ratings/αβ-max tree (car tree) most-positive-fixnum most-negative-fixnum))
             (maximum (apply max ratings))
             (n (list-index (equals-to? maximum) ratings)) ; here we can add non-determinism when there is more than one maximum.
             (moves (caddr tree)))
       (cadr ((stream:ref n) moves)))))

#;(defun play-vs-computer (tree)
        (print-info tree)
        (cond ((null (caddr tree)) (announce-winner (cadr tree)))
         ((zerop (car tree)) (play-vs-computer (handle-human tree)))
         (t (play-vs-computer (handle-computer tree)))))

    (define computer-vs-computer
     (lambda (AI-level)
      (let ((pruning (limit-tree-depth AI-level)))
       (letrec ((P (lambda (tree)
                    (display (make-gametree (car tree) (cadr tree) (caddr tree)))
                    (cond
                     ((stream:null? (caddr tree)) (announce-winner (cadr tree)))
                     (else (P (handle-computer tree handler: pruning)))))))
        P))))

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

;Now we start writing improvements for the AI...

#;(defun limit-tree-depth (tree depth)
        (list (car tree)
         (cadr tree)
         (if (zerop depth)
          (lazy-nil)
          (lazy-mapcar (lambda (move)
                        (list (car move)
                         (limit-tree-depth (cadr move) (1- depth))))
           (caddr tree)))))

    (define limit-tree-depth
     (lambda (depth)
      (lambda (tree)
       (letrec ((L (lambda (tree depth)
                    (list
                     (car tree)
                     (cadr tree)
                     (cond
                      ((zero? depth) stream:empty)
                      (else (let ((L₀ (lambda (move)
                                       (list
                                        (car move)
                                        (L (cadr move) (sub1 depth))))))
                             ((stream:map L₀) (caddr tree)))))))))
        (L tree depth)))))

#;(defun score-board (board player)
        (loop for hex across board
         for pos from 0
         sum (if (eq (car hex) player) (if (threatened pos board) 1 2) -1)))

    (define score-board
     (lambda (board player)
      (loop for hex across (board-cells board)
       for pos from 0
       sum (cond
           ((equal? (car hex) player) (if (threatened pos board) 1 2))
           (else -1)))))

#;(defun threatened (pos board)
        (let* ((hex (aref board pos))
               (player (car hex))
               (dice (cadr hex)))
         (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                  (nplayer (car nhex))
                  (ndice (cadr nhex)))
              (when (and (not (eq player nplayer)) (> ndice dice))
               (return t))))))

    (define threatened
     (lambda (pos board)
      (let* ((cell@ (board-cell@ board))
             (hex (cell@ pos))
             (player (car hex))
             (dice (cadr hex))
             (T (stream:foldr
                 (lambda (n α)
                  (let* ((nhex (cell@ n))
                         (nplayer (car nhex))
                         (ndice (cadr nhex)))
                   (or (and (not (equal? player nplayer)) (> ndice dice)) (force α))))
                 (lambda () #f))))
       (T (neighbors pos board)))))

;The rest of this file implements α-β pruning.

#;(defun ab-get-ratings-max (tree player upper-limit lower-limit)
        (labels ((f (moves lower-limit)
                  (unless (lazy-null moves)
                   (let ((x (ab-rate-position (cadr (lazy-car moves))
                             player
                             upper-limit
                             lower-limit)))
                    (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
         (f (caddr tree) lower-limit)))

    (define get-ratings/αβ-max
     (lambda (tree player upper-limit lower-limit)
      (letrec ((F (lambda (moves lower-limit)
                   (cond
                    ((stream:null? moves) '())
                    (else (let ((x (rate-position/αβ (cadr (stream:car moves)) player upper-limit lower-limit)))
                           (cond
                            ((>= x upper-limit) (list x))
                            (else (cons x (F (stream:cdr moves) (max x lower-limit)))))))))))
       (F (caddr tree) lower-limit))))

#;(defun ab-get-ratings-min (tree player upper-limit lower-limit)
        (labels ((f (moves upper-limit)
                  (unless (lazy-null moves)
                   (let ((x (ab-rate-position (cadr (lazy-car moves))
                             player
                             upper-limit
                             lower-limit)))
                    (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
         (f (caddr tree) upper-limit)))

    (define get-ratings/αβ-min
     (lambda (tree player upper-limit lower-limit)
      (letrec ((F (lambda (moves upper-limit)
                   (cond
                    ((stream:null? moves) '())
                    (else (let ((x (rate-position/αβ (cadr (stream:car moves)) player upper-limit lower-limit)))
                           (cond
                            ((<= x lower-limit) (list x))
                            (else (cons x (F (stream:cdr moves) (min x upper-limit)))))))))))
       (F (caddr tree) upper-limit))))

#;(defun ab-rate-position (tree player upper-limit lower-limit)
        (let ((moves (caddr tree)))
         (if (not (lazy-null moves))
          (if (eq (car tree) player)
           (apply max (ab-get-ratings-max tree
                       player
                       upper-limit
                       lower-limit))
           (apply min (ab-get-ratings-min tree
                       player
                       upper-limit
                       lower-limit)))
          (score-board (cadr tree) player))))

    (define rate-position/αβ
     (lambda (tree player upper-limit lower-limit)
      (let ((moves (caddr tree)))
       (if (not (stream:null? moves))
        (if (equal? (car tree) player)
         (apply max (get-ratings/αβ-max tree player upper-limit lower-limit))
         (apply min (get-ratings/αβ-min tree player upper-limit lower-limit)))
        (score-board (cadr tree) player)))))


)
