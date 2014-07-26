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

(defun bad-ghost (pos ghost)
  (and (= 0 (first ghost)) ; vitality == standard
       (pos-eq pos (second ghost))))

(defun matching-ghost (pos)
  (member-if (lambda (ghost) (bad-ghost pos ghost)) (ghost-state)))

(defun is-consumable (pos)
  (or (= (pos-contents pos) +pill+)
      (= (pos-contents pos) +power-pill+)))

(defun is-obstacle (pos)
  (or (= (pos-contents pos) +wall+)
      (not (null (matching-ghost pos)))))

(defun can-move (pos dir)
  (null (is-obstacle (move pos dir))))

(defun init-world (world)
  (set *world* world)
  (set *map* (car world)))

(defun init-globals ()
  (set *all-dirs* (list 0 1 2 3))
  (set *dir-list* (list (cons  0 -1)
			(cons  1  0)
			(cons  0  1)
			(cons -1  0))))

