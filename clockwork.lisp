(defun second (list)
  (car (cdr list)))

(defun third (list)
  (car (cdr (cdr list))))

(defun lambda-man-state (world)
  (second world))

(defun lambda-man-pos (world)
  (second (lambda-man-state world)))

(defun lambda-man-direction (world)
  (third (lambda-man-state world)))

(defun advance-state (state)
  (if (= state 3) 0 (+ state 1))) 

(defun new-state (old-pos new-pos state)
  (if (= (car old-pos) (car new-pos))
      (if (= (cdr old-pos) (cdr new-pos))
	  (advance-state state)
	  state)
      state))

(defun main ()
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (cons (lambda-man-pos world)
		(new-state old-pos
			   (lambda-man-pos world)
			   (lambda-man-direction world))))))
