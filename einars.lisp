; bēg no spociņiem


(defun nth (list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(defun second (list)
  (nth list 1))

(defun third (list)
  (nth list 2))

(defun map-elt (pos)
  (nth (nth *map* (cdr pos)) (car pos)))

(defun next-pos (pos dir)
  (if (= dir 0)
      (cons (car pos) (- (cdr pos) 1))
      (if (= dir 1)
	  (cons (+ 1 (car pos)) (cdr pos))
	  (if (= dir 2)
	      (cons (car pos) (+ (cdr pos) 1))
	      (cons (- (car pos) 1) (cdr pos))))))

(defun abs (n)
  (if (> n 0) n (- 0 n)))

(defun manhattan-dist (p1 p2)
  (+ (abs (- (car p1) (car p2)))
  (abs (- (cdr p1) (cdr p2)))))

(defun close-ghost? (ghost-pos pos)
  (> 3 (manhattan-dist pos ghost-pos)))

(defun has-any-ghost? (ghost-poss pos)
   (or-if (close-ghost? (nth ghost-poss 0) pos)
     (or-if (close-ghost? (nth ghost-poss 1) pos)
       (or-if (close-ghost? (nth ghost-poss 2) pos)
         (close-ghost? (nth ghost-poss 3) pos)))))

(defun moving-closer? (pos1 pos2 target)
  (< (manhattan-dist pos2 target)
     (manhattan-dist pos1 target)))

(defun move-score (elem old-pos pos target ghost-poss backwards?)
  (if (has-any-ghost? ghost-poss pos) 0
    (if (= 0 elem) 0 (+ (+ (+ (if (= 2 elem) 0 0)
                       (if (= 3 elem) 20 0))
                       (if (moving-closer? old-pos pos target) 10 0))
                        (if backwards? -10 1)
                        ))))

(defun choose-move (s0 s1 s2 s3)
  ; (print (cons s0 (cons s1 (cons s2 s3))))
  (if (and (>= s0 s1) (>= s0 s2) (>= s0 s3)) 0
    (if (and (>= s1 s0) (>= s1 s2) (>= s1 s3)) 1
      (if (and (>= s2 s0) (>= s2 s1) (>= s2 s3)) 2
        (if (and (>= s3 s0) (>= s3 s1) (>= s3 s2)) 3 0)))))


(defun collect-c3 (elem x y pred accum)
  (if (pred elem)
    (cons (cons x y) accum)
    accum))

(defun collect-c2 (row x y pred accum)
  (if (atom row)
    accum
    (collect-c2 (cdr row) (+ 1 x) y pred (collect-c3 (car row) x y pred accum))))

(defun collect-c1 (map y pred accum)
    (if (atom map) 
      accum
      (collect-c1 (cdr map) (+ 1 y) pred (collect-c2 (car map) 0 y pred accum))))

(defun collect-map (pred)
  (collect-c1 *map* 0  pred 0))

(defun pred-collector-dots (elem)
  (= 2 elem))


(defvar *best-pos*)
(defvar *best-dist*)

(defun find-closest (pos points)
  (set *best-pos* 0)
  (set *best-dist* 999999)
  (map (lambda (some-pos)
         (let ((dist (manhattan-dist some-pos pos)))
           (if (< dist *best-dist*)
             (progn (set *best-dist* dist)
                    (set *best-pos* some-pos))
             0)))
       points)
  *best-pos*)


(defun advance-state-2 (pos dots ghost-poss direction)
  (let ((target (find-closest pos dots)))
    (choose-move 
      (move-score (pos-contents (move pos 0)) pos (move pos 0) target ghost-poss (= 2 direction))
      (move-score (pos-contents (move pos 1)) pos (move pos 1) target ghost-poss (= 3 direction))
      (move-score (pos-contents (move pos 2)) pos (move pos 2) target ghost-poss (= 0 direction))
      (move-score (pos-contents (move pos 3)) pos (move pos 3) target ghost-poss (= 1 direction)))))


(defun advance-state (pos)
  (advance-state-2 pos (collect-map pred-collector-dots) (ghost-positions) (lambda-man-dir)))

(defun >= (a b)
  (if (> a b) 1 (= a b)))

(defun < (a b)
  (> b a))

(defun same-pos? (p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

(defun main ()
  (init-globals)
  (cons 0 (lambda (old-pos world)
            (init-world world)
            (cons 0 (advance-state (lambda-man-pos))))))


