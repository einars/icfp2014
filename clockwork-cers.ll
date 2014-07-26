
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
    (if (or (= (pos-contents pos-point) 2)
	    (= (pos-contents pos-point) 3))
	(list (third pos))
	0)))

(defun find-closest-point-direction-rec (pos-list)
  (let ((new-list (mappend moves-at-point pos-list)))
    (if (> (third (car pos-list)) 3)
	(set new-list (remove-if (lambda (x) (is-ghost (first x))) new-list))
	0)
    (if (and (not (null new-list))
	     (null (mappend check-point-pos new-list)))
	(find-closest-point-direction-rec new-list)
	(if (null new-list)
	    (advance-state (lambda-man-pos))
	    (car (mappend check-point-pos new-list))))))

(defun find-closest-point-direction (pos)
  (find-closest-point-direction-rec (list (list pos pos 99 99 0))))

(defun main ()
  (init-globals)
  (cons 0
	(lambda (old-pos world)
	  (init-world world)
	  (cons 0 (find-closest-point-direction (lambda-man-pos))))))
