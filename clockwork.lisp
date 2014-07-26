(defvar *closest-pill*)

(defun player-distance (pos)
  (manhattan (lambda-man-pos) pos))

(defun update-closest-pill (pos new-distance)
  (set *closest-pill* pos)
  new-distance)

(defun pill-at (val pos distance)
  (let ((new-distance (player-distance pos)))
    (if (and (is-pill val) (> distance new-distance))
	(update-closest-pill pos new-distance)
	distance)))

(defun pill-row (row x y distance)
  (if (null row)
      distance
      (pill-row (cdr row) (+ x 1) y (pill-at (car row) (cons x y) distance))))

(defun pill-column (map y distance)
  (if (null map)
      distance
      (pill-column (cdr map) (+ y 1) (pill-row (car map) 0 y distance))))

(defun current-board ()
  (list (lambda-man-pos) (lambda-man-dir) -1 0))

(defun get-board-pos (board)
  (first board))

(defun get-board-dir (board)
  (second board))

(defun get-board-parent-dir (board)
  (third board))

(defun get-board-pills (board)
  (fourth board))

(defun get-parent-dir (board dir)
  (if (>= (get-board-parent-dir board) 0)
      (get-board-parent-dir board)
      dir))

(defun update-pills (board pos)
  (+ (get-board-pills board)
     (is-consumable pos)))

(defun move-board (board dir)
  (let ((pos (move (get-board-pos board) dir)))
    (list pos dir
	  (get-parent-dir board dir)
	  (update-pills board pos))))

(defun is-legal-move (board dir)
  (and (can-move (get-board-pos board) dir)
       (not (= dir (opposite (get-board-dir board))))))

(defun possible-forward-dirs (board)
  (remove-if (lambda (dir) (not (is-legal-move board dir))) *all-dirs*))

(defun possible-dirs (board)
  (or-if (possible-forward-dirs board)
	 (list (opposite (get-board-dir board)))))

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

(defun get-all-boards-at-depth (depth)
  (boards-at-depth (list (current-board)) depth))

(defun goodness (board)
  (get-board-pills board))

(defun filter-best (board-lst score best)
  (cond ((null board-lst) best)
	(t (let ((new-score (goodness (car board-lst))))
	     (if (> new-score score)
		 (filter-best (cdr board-lst) new-score (car board-lst))
		 (filter-best (cdr board-lst) score best))))))

(defun pick-best (board-lst)
  (filter-best board-lst -1 nil))

(defun get-desired-direction ()
  (get-board-parent-dir (pick-best (get-all-boards-at-depth 7))))

(defun main ()
  (init-globals)
  (cons 0 (lambda (old-pos world)
	    (init-world world)
	    (pill-column *map* 0 512)
	    (cons 0 (get-desired-direction)))))
