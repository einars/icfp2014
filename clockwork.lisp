(defun nth (list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(defun second (list)
  (nth list 1))

(defun third (list)
  (nth list 2))

(defun lambda-man-state (world)
  (second world))

(defun lambda-man-pos (world)
  (second (lambda-man-state world)))

(defun lambda-man-direction (world)
  (third (lambda-man-state world)))

(defun map-elt (pos map)
  (nth (nth map (cdr pos)) (car pos)))

(defun add-pos (pos1 pos2)
  (cons (+ (car pos1) (car pos2))
	(+ (cdr pos1) (cdr pos2))))

(defvar dir-list)

(defun next-pos (pos dir)
  (add-pos pos (nth dir-list dir)))

(defun mod4 (val)
  (if (>= val 4) (mod4 (- val 4)) val))

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
  (set dir-list (list (cons  0 -1)
		      (cons  1  0)
		      (cons  0  1)
		      (cons -1  0)))
  (set dir-N 0)
  (set dir-E 1)
  (set dir-S 2)
  (set dir-W 3))

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

(defun pos-eq (pos1 pos2)
  (and (= (car pos1) (car pos2))
       (= (cdr pos1) (cdr pos2))))

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
