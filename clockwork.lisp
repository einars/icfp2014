(defun guess-direction (old-pos pos state)
  (if (and (= (car old-pos) (car pos))
	   (= (cdr old-pos) (cdr pos)))
      (mod4 (+ state 1))
      state))

(defun look-at-direction (old-pos pos state i)
  (if (= 2 (map-elt (move pos i) *map*))
      i
      (advance-state old-pos pos state (+ i 1))))

(defun advance-state (old-pos pos state i)
  (if (= i 4)
      (guess-direction old-pos pos state)
      (look-at-direction old-pos pos state i)))

(defun current-board ()
  (list (lambda-man-pos) (lambda-man-dir)))

(defun get-board-pos (board)
  (first board))

(defun get-board-dir (board)
  (second board))

(defun move-board (board dir)
  (list (move (get-board-pos board) dir) dir))

(defun is-legal-move (board dir)
  (and (can-move (get-board-pos board) dir)
       (not (= dir (opposite (get-board-dir board))))))

(defun possible-forward-dirs (board)
  (remove-if (lambda (dir) (not (is-legal-move board dir))) *all-dirs*))

(defun possible-dirs (board)
  (or-if (possible-forward-dirs board) (opposite (get-board-dir board))))

(defun boards-from (board)
  (map (lambda (dir) (move-board board dir)) (possible-dirs board)))

(defun boards-from-board-list (board-lst)
  (append (boards-from (car board-lst))
	  (boards-from-boards (cdr board-lst))))

(defun boards-from-boards (board-lst)
  (if (null board-lst) nil (boards-from-board-list board-lst)))

(defun boards-at-depth (board-lst depth)
  (if (= depth 0)
      board-lst
      (boards-at-depth (boards-from-boards board-lst) (- depth 1))))

(defun main ()
  (init-globals)
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (init-world world)
	  (boards-at-depth (list (current-board)) 3)
	  (cons (lambda-man-pos)
		(advance-state old-pos
			       (lambda-man-pos)
			       (lambda-man-dir)
			       0)))))
