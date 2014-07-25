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

(defun next-pos (pos dir)
  (if (= dir 0)
      (cons (car pos) (- (cdr pos) 1))
      (if (= dir 1)
	  (cons (+ 1 (car pos)) (cdr pos))
	  (if (= dir 2)
	      (cons (car pos) (+ (cdr pos) 1))
	      (cons (- (car pos) 1) (cdr pos))))))

(defun advance-state (pos map state)
  (if (= 2 (map-elt (next-pos pos 0) map))
      0
      (if (= 2 (map-elt (next-pos pos 1) map))
	  1
	  (if (= 2 (map-elt (next-pos pos 2) map))
	      2
	      (if (= 2 (map-elt (next-pos pos 3) map))
		  3
		  (if (= state 3) 0 (+ state 1)))))))

(defun and (a b)
  (* a b))

(defun new-state (old-pos new-pos map state)
  (if (and (= (car old-pos) (car new-pos))
	   (= (cdr old-pos) (cdr new-pos)))
      (advance-state new-pos map state)
      state))

(defun main ()
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (cons (lambda-man-pos world)
		(new-state old-pos
			   (lambda-man-pos world)
			   (car world)
			   (lambda-man-direction world))))))
