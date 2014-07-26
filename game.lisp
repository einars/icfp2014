(defvar +dir-N+)
(defvar +dir-S+)
(defvar +dir-E+)
(defvar +dir-W+)

(defvar *map*)
(defvar *dir-list*)

(defun lambda-man-state (world)
  (second world))

(defun lambda-man-pos (world)
  (second (lambda-man-state world)))

(defun lambda-man-direction (world)
  (third (lambda-man-state world)))

(defun map-elt (pos map)
  (nth (nth map (cdr pos)) (car pos)))

(defun opposite (direction)
  (mod4 (+ direction 2)))

(defun move (pos dir)
  (add-pos pos (nth *dir-list* dir)))

(defun pos-contents (pos)
  (map-elt pos *map*))

(defun check-wall (pos)
  (= (pos-contents pos) 0))

(defun init-world (world)
  (set *map* (car world)))

(defun init-globals ()
  (set +dir-N+ 0)
  (set +dir-E+ 1)
  (set +dir-S+ 2)
  (set +dir-W+ 3)
  (set *dir-list* (list (cons  0 -1)
			(cons  1  0)
			(cons  0  1)
			(cons -1  0))))

