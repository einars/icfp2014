(defvar *state*)
(defvar *running*)
(defvar +max-depth+ 5)

(defun try-move-at-point (pos direction)
  (let ((new-pos (move pos direction)))
    (if (is-obstacle new-pos)
	0
	(if (is-ghost new-pos)
	    (progn
	      (set *running* 1)
	      0)
	    (list direction)))))

(defun moves-at-point (pos)
  (flatten
   (list
    (try-move-at-point pos +dir-N+)
    (try-move-at-point pos +dir-E+)
    (try-move-at-point pos +dir-S+)
    (try-move-at-point pos +dir-W+))))

(defun compare-moves (pos move1 move2)
  (let ((pos1 (move pos move1))
	(pos2 (move pos move2)))
    (if (= (cdr (annotated-get *state* (cdr pos1) (car pos1)))
	   (cdr (annotated-get *state* (cdr pos2) (car pos2))))
	(if (> (abs (- move1 (lambda-man-dir)))
	       (abs (- move2 (lambda-man-dir))))
	    move2
	    move1)
	(if (> (cdr (annotated-get *state* (cdr pos1) (car pos1)))
	       (cdr (annotated-get *state* (cdr pos2) (car pos2))))
	    move2
	    move1))))

(defun choose-move (pos)
  (if (not (null (moves-at-point pos)))
      (reduce (lambda (move1 move2)
		(if (= move1 99)
		    move2
		    (compare-moves pos move1 move2)))
	      99
	      (moves-at-point pos))
      0))

(defun main ()
  (init-globals)
  (cons (create-annotated-map)
	(lambda (state world)
	  (set *state* state)
	  (set *running* 0)
	  (init-world world)
	  (cons
	   (annotated-set *state*
			  (cdr (lambda-man-pos))
			  (car (lambda-man-pos))
			  (+ (if (= 0 (cdr (annotated-get *state* (cdr (lambda-man-pos)) (car (lambda-man-pos)))))
				 2
				 1) 
			     (cdr (annotated-get *state* (cdr (lambda-man-pos)) (car (lambda-man-pos))))))
	   (choose-move (lambda-man-pos))))))
