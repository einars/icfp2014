(defvar *linker-symbols* nil)

(defun el-num (var frame)
  (- (length frame) (length (member var frame))))

(defun find-symbol-lm (var env &optional (depth 0))
  (cond ((null env) (error "unknown variable ~A" var))
	((member var (first env)) `(ld ,depth ,(el-num var (first env))))
	(t (find-symbol-lm var (rest env) (1+ depth)))))

(defun is-arithmetic (exp)
  (and (consp exp) (member (first exp) '(+ - / *))))

(defun get-arithmetic-opcode (op)
  (list (list (cdr (assoc op '((+ . add) (- . sub) (* . mul) (/ . div)))))))

(defun compile-arithmetic (exp env)
  (append (compile-lm (second exp) env)
	  (compile-lm (third exp) env)
	  (get-arithmetic-opcode (first exp))))

(defun is-application (exp)
  (and (consp exp) (symbolp (first exp))))

(defun compile-list (exp-list env)
  (cond ((null exp-list) nil)
	(t (append (compile-lm (first exp-list) env)
		   (compile-list (rest exp-list) env)))))

(defun apply-defined (exp env)
  (append (compile-list (rest exp) env)
	  `((ldf ,(first exp))
	    (ap ,(length (rest exp))))))

(defun is-special (exp sym)
  (and (consp exp) (eq (first exp) sym)))

(defun compile-lambda-body (def env)
  (compile-lm (second def) (cons (first def) env)))

(defun compile-function (sym def env)
  (cond ((not (listp (first def))) (error "invalid lambda list ~A" def))
	((not (= 2 (length def))) (error "malformed lambda body ~A" def))
	(t (push (cons sym (append (compile-lambda-body def env) '((rtn))))
		 *linker-symbols*))))

(defun compile-lambda (exp env)
  (let ((lambda-sym (gensym)))
    (compile-function lambda-sym (rest exp) env)
    `((ldf ,lambda-sym))))

(defun compile-lm (exp env)
  (cond ((numberp exp) `((ldc ,exp)))
	((symbolp exp) (list (find-symbol-lm exp env)))
	((is-arithmetic exp) (compile-arithmetic exp env))
	((is-special exp 'lambda) (compile-lambda exp env))
	((is-application exp) (apply-defined exp env))
	(t (error "bad expression ~A" exp))))
