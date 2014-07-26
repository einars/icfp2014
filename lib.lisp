(defvar nil)

(defun nth (list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(defun second (list)
  (nth list 1))

(defun third (list)
  (nth list 2))

(defun add-pos (pos1 pos2)
  (cons (+ (car pos1) (car pos2))
	(+ (cdr pos1) (cdr pos2))))

(defun mod4 (val)
  (if (>= val 4) (mod4 (- val 4)) val))

(defun pos-eq (pos1 pos2)
  (and (= (car pos1) (car pos2))
       (= (cdr pos1) (cdr pos2))))

(defun null (item)
  (and (atom item) (= item 0)))

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
