(defvar *closest-pill*)

(defun player-distance (pos)
  (manhattan (lambda-man-pos) pos))

(defun update-closest-pill (pos new-distance)
  (set *closest-pill* pos)
  new-distance)

(defun is-fruit (val)
  (= val 4))

(defun pill-at (val pos distance)
  (let ((new-distance (player-distance pos)))
    (if (> (cdr (cdr (cdr *world*))) 0)
	(if (is-fruit val)
	    (update-closest-pill pos new-distance)
	    nil)
	(if (and (is-pill val) (> distance new-distance))
	    (update-closest-pill pos new-distance)
	    distance))))

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
  (pill-column *map* 0 512)
  (set *old-states* nil)
  (set *new-states* (list (initial-state)))
  (get-a-star-direction))

(defun ghost-distance (ghost)
  (manhattan (lambda-man-pos) (second ghost)))

(defun is-threat (ghost)
  (and (= 0 (first ghost)) (>= 3 (ghost-distance ghost))))

(defun threats (ghosts)
  (cond ((null ghosts) nil)
	((is-threat (car ghosts))
	 (cons (car ghosts) (threats (cdr ghosts))))
	(t (threats (cdr ghosts)))))

(defun total-distance (ghosts pos)
  (if (null ghosts)
      0
      (+ (manhattan pos (second (car ghosts)))
	 (total-distance (cdr ghosts) pos))))

(defun total-distance-from (ghosts dir)
  (total-distance ghosts (move (lambda-man-pos) dir)))

(defun furthest-from (ghosts best-dir distance dirs)
  (cond ((null dirs) best-dir)
	(t (let ((new-distance (total-distance-from ghosts (car dirs))))
	     (if (> new-distance distance)
		 (furthest-from ghosts (car dirs) new-distance (cdr dirs))
		 (furthest-from ghosts best-dir distance (cdr dirs)))))))

(defun evade (ghosts)
  (furthest-from ghosts +dir-N+ 0 (possible-dirs (lambda-man-pos))))

(defun evasive-a-star ()
  (let ((ghosts (threats (ghost-state))))
    (if (consp ghosts)
	(evade ghosts)
	(a-star))))

(defun main ()
  (init-globals)
  (cons 0 (lambda (old-pos world)
	    (init-world world)
	    (cons 0 (evasive-a-star)))))
