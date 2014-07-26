(defun nth (list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(defun second (list)
  (nth list 1))

(defun third (list)
  (nth list 2))

(defun fourth (list)
  (nth list 3))

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

(defun not (x)
  (if x 0 1))

(defun null (x)
  (and (atom x) (= x 0)))

(defun reduce (fn acc lst)
  (if (atom lst)
      acc
      (reduce fn (funcall fn acc (car lst)) (cdr lst))))

(defun map (fn lst)
  (if (null lst)
      0
      (cons (funcall fn (car lst)) (map fn (cdr lst)))))

(defun mappend (fn lst)
  (if (null lst)
      0
      (append-2 (funcall fn (car lst)) (mappend fn (cdr lst)))))

(defun append-2 (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append-2 (cdr list1) list2))))

(defun append (lists)
  (reduce (lambda (x y)
	    (append-2 x y))
	  0
	  lists))

(defun try-move-at-point (pos direction)
  (let ((new-pos (move (car pos) direction))
	(excluded-move (second pos))
	(original-direction (third pos))
	(contents (fourth pos)))
    (if (or (check-wall new-pos)
	    (pos-eq new-pos excluded-move))
	0
	(list (list new-pos
		    (car pos)
		    (if (> original-direction 3)
			direction
			original-direction)
		    (pos-contents new-pos))))))

(defun moves-at-point (pos)
  (append
   (list
    (try-move-at-point pos dir-N)
    (try-move-at-point pos dir-E)
    (try-move-at-point pos dir-S)
    (try-move-at-point pos dir-W))))

(defun check-point-pos (pos)
  (let ((pos-point (car pos)))
    (if (or (= (pos-contents pos-point) 2)
	    (= (pos-contents pos-point) 3)
	    (= (pos-contents pos-point) 4))
	(list (third pos))
	0)))

(defun find-closest-point-direction-rec (pos-list)
  (set pos-list (mappend moves-at-point pos-list))
  (if (null (mappend check-point-pos pos-list))
      (find-closest-point-direction-rec pos-list)
      (car (mappend check-point-pos pos-list))))

(defun find-closest-point-direction (pos)
  (find-closest-point-direction-rec (list (list pos pos 99 99))))

(defun main ()
  (init-globals)
  (cons 0
	(lambda (old-pos world)
	  (set *map* (car world))
	  (cons 0 (find-closest-point-direction (lambda-man-pos world))))))
