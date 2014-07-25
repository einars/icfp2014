(defun el-num (var frame)
  (- (length frame) (length (member var frame))))

(defun find-symbol-lm (var env &optional (depth 0))
  (cond ((null env) (error "unknown variable ~A" var))
	((member var (first env)) `(ld ,depth ,(el-num var (first env))))
	(t (find-symbol-lm var (rest env) (1+ depth)))))

(defun compile-lm (exp env)
  (cond ((numberp exp) `((ldc ,exp)))
	((symbolp exp) (list (find-symbol-lm exp env)))
	(t (error "bad expression"))))
