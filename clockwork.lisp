(defun lambda-man-state (world)
  (second world))

(defun lambda-man-pos (world)
  (second (lambda-man-state world)))

(defun lambda-man-direction (world)
  (third (lambda-man-state world)))

(defun map-elt (pos map)
  (nth (nth map (cdr pos)) (car pos)))

(defvar dir-list)

(defun next-pos (pos dir)
  (add-pos pos (nth dir-list dir)))

(defun guess-direction (old-pos pos state)
  (if (and (= (car old-pos) (car pos))
	   (= (cdr old-pos) (cdr pos)))
      (mod4 (+ state 1))
      state))

(defvar *map*)

(defun look-at-direction (old-pos pos state i)
  (if (= 2 (map-elt (next-pos pos i) *map*))
      i
      (advance-state old-pos pos state (+ i 1))))

(defun advance-state (old-pos pos state i)
  (if (= i 4)
      (guess-direction old-pos pos state)
      (look-at-direction old-pos pos state i)))

(defun init-globals ()
  (set dir-N 0)
  (set dir-E 1)
  (set dir-S 2)
  (set dir-W 3)
  (set dir-list (list (cons  0 -1)
		      (cons  1  0)
		      (cons  0  1)
		      (cons -1  0))))

(defun opposite (direction)
  (mod4 (+ direction 2)))

(defvar dir-N)
(defvar dir-S)
(defvar dir-E)
(defvar dir-W)

(defun move (pos dir)
  (add-pos pos (nth dir-list dir)))

(defun pos-contents (pos)
  (map-elt pos *map*))

(defun check-wall (pos)
  (= (pos-contents pos) 0))

(defun main ()
  (init-globals)
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (set *map* (car world))
	  (cons (lambda-man-pos world)
		(advance-state old-pos
			       (lambda-man-pos world)
			       (lambda-man-direction world)
			       0)))))
