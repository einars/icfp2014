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

(defvar *new-states*)
(defvar *old-states*)

(defun distance (pos)
  (manhattan pos *closest-pill*))

(defun initial-state ()
  (list (lambda-man-pos) (distance (lambda-man-pos)) -1))

(defun state-pos (state)
  (first state))

(defun state-distance (state)
  (second state))

(defun state-from (state)
  (third state))

(defun possible-dirs (pos)
  (remove-if (lambda (dir) (not (can-move pos dir))) *all-dirs*))

(defun state-dir (from dir)
  (if (> 0 from) dir from))

(defun create-state (state dir)
  (let ((pos (move (state-pos state) dir)))
    (list pos (distance pos) (state-dir (state-from state) dir))))

(defun possible-neighbors (state)
  (map (lambda (dir) (create-state state dir))
       (possible-dirs (state-pos state))))

(defun pick-state ()
  (let ((state (first *new-states*)))
    (set *new-states* (cdr *new-states*))
    (set *old-states* (cons state *old-states*))
    state))

(defun hit (state)
  (pos-eq *closest-pill* (state-pos state)))

(defun insert (state list)
  (cond ((null list) (list state))
	((> (state-distance (car list))
	    (state-distance state))
	 (cons state list))
	(t (cons (car list) (insert state (cdr list))))))

(defun push-state (state)
  (set *new-states* (insert state *new-states*)))

(defun is-not-element (a list)
  (null (member-if (lambda (b) (pos-eq (state-pos a) (state-pos b))) list)))

(defun is-new-state (state)
  (and (is-not-element state *old-states*)
       (is-not-element state *new-states*)))

(defun dispatch-neighbor (state rest-states)
  (if (is-new-state state) (push-state state) nil)
  (sort-neighbors rest-states))

(defun sort-neighbors (states)
  (cond ((null states) nil)
	((hit (car states)) (car states))
	(t (dispatch-neighbor (car states) (cdr states)))))

(defun inspect-state ()
  (sort-neighbors (possible-neighbors (pick-state))))

(defun get-a-star-direction ()
  (let ((result (inspect-state)))
    (if (consp result)
	(state-from result)
	(get-a-star-direction))))

(defun a-star ()
  (set *old-states* nil)
  (set *new-states* (list (initial-state)))
  (get-a-star-direction))

(defun main ()
  (init-globals)
  (cons 0 (lambda (old-pos world)
	    (init-world world)
	    (pill-column *map* 0 512)
	    (cons 0 (a-star)))))
