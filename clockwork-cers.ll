(defun try-move-at-point (pos direction)
  (let ((new-pos (move (car pos) direction))
	(excluded-move (second pos))
	(original-direction (third pos))
	(contents (fourth pos)))
    (if (or (is-obstacle new-pos)
	    (pos-eq new-pos excluded-move))
	0
	(list (list new-pos
		    (car pos)
		    (if (> original-direction 3)
			direction
			original-direction)
		    (pos-contents new-pos))))))

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
  (set pos-list (mappend moves-at-point pos-list))
  (if (and (not (null pos-list))
	   (null (mappend check-point-pos pos-list)))
      (find-closest-point-direction-rec pos-list)
      (if (null pos-list)
	  +dir-N+
	  (car (mappend check-point-pos pos-list)))))

(defun find-closest-point-direction (pos)
  (find-closest-point-direction-rec (list (list pos pos 99 99))))

(defun main ()
  (init-globals)
  (cons 0
	(lambda (old-pos world)
	  (init-world world)
	  (cons 0 (find-closest-point-direction (lambda-man-pos))))))
