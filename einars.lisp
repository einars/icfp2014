; bēg no spociņiem

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

(defun has-ghost? (ghost_elem pos)
  (same-pos? pos (nth ghost_elem 1)))

(defun abs (n)
  (if (> n 0) n (- 0 n)))

(defun manhattan-dist (p1 p2)
  (+ (abs (- (car p1) (car p2)))
  (abs (- (cdr p1) (cdr p2)))))

(defun close-ghost? (ghost_elem pos)
  (> 3 (manhattan-dist pos (nth ghost_elem 1))))

(defun has-any-ghost? (ghosts pos)
  (if (close-ghost? (nth ghosts 0) pos) 1
    (if (close-ghost? (nth ghosts 1) pos) 1
      (if (close-ghost? (nth ghosts 2) pos) 1
        (close-ghost? (nth ghosts 3) pos)))))

(defun move-score (elem pos ghosts frontfacing? backfacing?)
  (if (has-any-ghost? ghosts pos) 0
    (if (= 0 elem)  0
        (if (= 2 elem) 10
            (if (= 3 elem) 20
            (if backfacing? 1 (if frontfacing? 3 2)))))))

(defun choose-move (s0 s1 s2 s3)
  (if (and3 (>= s0 s1) (>= s0 s2) (>= s0 s3)) 0
    (if (and3 (>= s1 s0) (>= s1 s2) (>= s1 s3)) 1
      (if (and3 (>= s2 s0) (>= s2 s1) (>= s2 s3)) 2
        (if (and3 (>= s3 s0) (>= s3 s1) (>= s3 s2)) 3 0)))))
        
(defun advance-state (pos map ghosts state stuck?)
  (choose-move 
    (move-score (map-elt (next-pos pos 0) map) (next-pos pos 0) ghosts (= state 0) (= state 2))
    (move-score (map-elt (next-pos pos 1) map) (next-pos pos 1) ghosts (= state 1) (= state 3))
    (move-score (map-elt (next-pos pos 2) map) (next-pos pos 2) ghosts (= state 2) (= state 0))
    (move-score (map-elt (next-pos pos 3) map) (next-pos pos 3) ghosts (= state 3) (= state 1))))

(defun and (a b)
  (* a b))

(defun and3 (a b c)
  (* a (* b c)))

(defun >= (a b)
  (if (> a b) 1 (= a b)))

(defun same-pos? (p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

(defun new-state (old-pos new-pos map ghosts state)
      (advance-state new-pos map ghosts state (same-pos? old-pos new-pos)))

(defun main ()
  (cons (cons 0 0)
	(lambda (old-pos world)
	  (cons (lambda-man-pos world)
		(new-state old-pos
			   (lambda-man-pos world)
			   (car world)
			   (nth world 2)
			   (lambda-man-direction world))))))


