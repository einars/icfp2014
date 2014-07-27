(defvar +dir-N+ 0)
(defvar +dir-S+ 1)
(defvar +dir-E+ 2)
(defvar +dir-W+ 3)

(defvar +wall+ 0)
(defvar +pill+ 2)
(defvar +power-pill+ 3)

(defvar *map*)
(defvar *world*)
(defvar *all-dirs*)
(defvar *dir-list*)

(defun lambda-man-state ()
  (second *world*))

(defun lambda-man-pos ()
  (second (lambda-man-state)))

(defun lambda-man-dir ()
  (third (lambda-man-state)))

(defun map-elt (pos map)
  (nth (nth map (cdr pos)) (car pos)))

(defun opposite (direction)
  (mod4 (+ direction 2)))

(defun move (pos dir)
  (add-pos pos (nth *dir-list* dir)))

(defun pos-contents (pos)
  (map-elt pos *map*))

(defun ghost-state ()
  (third *world*))

(defun ghost-positions ()
  (map second (third *world*)))

(defun bad-ghost (pos ghost)
  (and (= 0 (first ghost))
       (> 2 (manhattan pos (second ghost)))))

(defun matching-ghost (pos fn)
  (member-if (lambda (ghost) (funcall fn pos ghost)) (ghost-state)))

(defun is-pill (val)
  (or (= val +pill+) (= val +power-pill+)))

(defun is-fruit (val)
  (= val 4))

(defun fruit-on-board ()
  (> (cdr (cdr (cdr *world*))) 0))

(defun is-consumable (pos)
  (is-pill (pos-contents pos)))

(defun is-obstacle (pos)
  (= (pos-contents pos) +wall+))

(defun is-ghost (pos)
  (not (null (matching-ghost pos bad-ghost))))

(defun init-world (world)
  (set *world* world)
  (set *map* (car world)))

(defun init-globals ()
  (set *all-dirs* (list 0 1 2 3))
  (set *dir-list* (list (cons  0 -1)
			(cons  1  0)
			(cons  0  1)
			(cons -1  0))))

(defun create-annotated-map ()
  (map (lambda (row)
	 (map (lambda (cell)
		(cons cell 0))
	      row))
       (car arg1)))

(defun annotated-col-get (row col)
  (if (> col 0)
      (annotated-col-get (cdr row) (- col 1))
      (car row)))

(defun annotated-get (map row col)
  (if (> row 0)
      (annotated-get (cdr map) (- row 1) col)
      (annotated-col-get (car map) col)))

(defun annotated-row-set (row col val)
  (if (not (= col 0))
      (cons (car row) (annotated-row-set (cdr row) (- col 1) val))
      (cons (cons (car (car row)) val) (cdr row))))

(defun annotated-set (map row col val)
  (if (not (= row 0))
      (cons (car map) (annotated-set (cdr map) (- row 1) col val))
      (cons (annotated-row-set (car map) col val) (cdr map))))
