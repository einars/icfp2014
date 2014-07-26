(defvar nil)
(defvar t 1)

(defun nth (list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(defun first (list)
  (car list))

(defun second (list)
  (car (cdr list)))

(defun third (list)
  (nth list 2))

(defun fourth (list)
  (nth list 3))

(defun not (a)
  (if (null a) t 0))

(defun or-if (a b)
  (if (null a) b a))

(defun add-pos (pos1 pos2)
  (cons (+ (car pos1) (car pos2))
	(+ (cdr pos1) (cdr pos2))))

(defun mod4 (val)
  (if (>= val 4) (mod4 (- val 4)) val))

(defun pos-eq (pos1 pos2)
  (and (= (car pos1) (car pos2))
       (= (cdr pos1) (cdr pos2))))

(defun null (item)
  (and (atom item) (= item nil)))

(defun length (lst)
  (if (null lst)
      nil
      (+ 1 (length (cdr lst)))))

(defun reduce (fn acc lst)
  (if (atom lst)
      acc
      (reduce fn (funcall fn acc (car lst)) (cdr lst))))

(defun append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defun map (fn lst)
  (if (null lst)
      nil
      (cons (funcall fn (car lst)) (map fn (cdr lst)))))

(defun mappend (fn lst)
  (if (null lst)
      0
      (append (funcall fn (car lst)) (mappend fn (cdr lst)))))

(defun flatten (lists)
  (reduce (lambda (x y)
	    (append x y))
	  0
	  lists))

(defun member-if (predicate lst)
  (cond ((null lst) nil)
	((funcall predicate (car lst)) lst)
	(t (member-if predicate (cdr lst)))))

(defun remove-if (predicate lst)
  (cond ((null lst) nil)
	((funcall predicate (car lst))
	 (remove-if predicate (cdr lst)))
	(t (cons (car lst) (remove-if predicate (cdr lst))))))

(defun abs (val)
  (if (> val 0) val (- 0 val)))

(defun min (a b)
  (if (> a b) b a))

(defun max (a b)
  (if (> a b) a b))

(defun manhattan (pos1 pos2)
  (+ (abs (- (car pos1) (car pos2)))
     (abs (- (cdr pos1) (cdr pos2)))))
