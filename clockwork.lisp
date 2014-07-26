(defun guess-direction (old-pos pos state)
  (if (and (= (car old-pos) (car pos))
	   (= (cdr old-pos) (cdr pos)))
      (mod4 (+ state 1))
      state))

(defun look-at-direction (old-pos pos state i)
  (if (= 2 (map-elt (move pos i) *map*))
      i
      (advance-state old-pos pos state (+ i 1))))

(defun advance-state (old-pos pos state i)
  (if (= i 4)
      (guess-direction old-pos pos state)
      (look-at-direction old-pos pos state i)))

(defun main ()
  (init-globals)
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (init-world world)
	  (cons (lambda-man-pos)
		(advance-state old-pos
			       (lambda-man-pos)
			       (lambda-man-dir)
			       0)))))
