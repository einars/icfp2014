(defvar *point-list*)
(defvar *stored-world*)
(defvar *pill-list*)
(defvar *pill-count*)
(defvar +max-depth+ 5)

(defun try-move-at-point (pos direction)
  (let ((new-pos (move (car pos) direction))
	(excluded-move (second pos))
	(original-direction (third pos))
	(contents (fourth pos))
	(depth (nth pos 4)))
    (if (or (is-obstacle new-pos)
	    (pos-eq new-pos excluded-move)
	    (> depth +max-depth+))
	0
	(list (list new-pos
		    (car pos)
		    (if (> original-direction 3)
			direction
			original-direction)
		    (pos-contents new-pos)
		    (+ 1 depth))))))

(defun moves-at-point (pos)
  (flatten
   (list
    (try-move-at-point pos +dir-N+)
    (try-move-at-point pos +dir-E+)
    (try-move-at-point pos +dir-S+)
    (try-move-at-point pos +dir-W+))))

(defun check-point-pos (pos)
  (let ((pos-point (car pos)))
    (if (= (pos-contents pos-point) 2)
	(list (car pos))
	0)))

(defun find-closest-point-rec (pos-list)
  (let ((new-list (mappend moves-at-point pos-list)))
    (if (and (not (null new-list))
	     (null (mappend check-point-pos new-list)))
	(find-closest-point-rec new-list)
	(if (null new-list)
	    0
	    (car (mappend check-point-pos new-list))))))

(defun find-point-near (pos)
  (find-closest-point-rec (list (list pos pos 99 99 0))))
