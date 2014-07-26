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

(defun not (a)
  (if a 0 t))

(defun or-if (a b)
  (if a a b))

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

(defun append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defun map (fn lst)
  (if (null lst)
      nil
      (cons (funcall fn (car lst)) (map fn (cdr lst)))))

(defun member-if (predicate lst)
  (cond ((null lst) nil)
	((funcall predicate (car lst)) lst)
	(t (member-if predicate (cdr lst)))))

(defun remove-if (predicate lst)
  (cond ((null lst) nil)
	((funcall predicate (car lst))
	 (remove-if predicate (cdr lst)))
	(t (cons (car lst) (remove-if predicate (cdr lst))))))
