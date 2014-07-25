(defparameter *linker-symbols* nil)

(defun el-num (var frame)
  (- (length frame) (length (member var frame))))

(defun find-symbol-lm (var env &optional (depth 0))
  (cond ((null env) (error "unknown variable ~A" var))
	((member var (first env)) `(ld ,depth ,(el-num var (first env))))
	(t (find-symbol-lm var (rest env) (1+ depth)))))

(defparameter *builtin-ops*
  '((+ . add)
    (- . sub)
    (* . mul)
    (/ . div)
    (= . ceq)
    (> . cgt)
    (>= . cgte)
    (car . car)
    (cdr . cdr)
    (atom . atom)
    (cons . cons)))

(defun is-builtin (exp)
  (and (consp exp) (assoc (first exp) *builtin-ops*)))

(defun get-builtin-opcode (op)
  (list (list (cdr (assoc op *builtin-ops*)))))

(defun compile-builtin (exp env)
  (append (compile-list (rest exp) env)
	  (get-builtin-opcode (first exp))))

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

(defun compile-block (sym body env linkage)
  (push (cons sym (append (compile-lm body env) linkage)) *linker-symbols*))

(defun compile-lambda (exp env)
  (let ((lambda-sym (gensym)))
    (unless (= 3 (length exp)) (error "malformed lambda body ~A" exp))
    (unless (listp (second exp)) (error "invalid lambda list ~A" (second exp)))
    (compile-block lambda-sym (third exp) (cons (second exp) env) '((rtn)))
    `((ldf ,lambda-sym))))

(defun compile-if (exp env)
  (let ((true-sym (gensym))
	(false-sym (gensym)))
    (compile-block true-sym (third exp) env '((join)))
    (compile-block false-sym (fourth exp) env '((join)))
    (append (compile-lm (second exp) env)
	    `((sel ,true-sym ,false-sym)))))

(defun compile-lm (exp env)
  (cond ((numberp exp) `((ldc ,exp)))
	((symbolp exp) (list (find-symbol-lm exp env)))
	((is-builtin exp) (compile-builtin exp env))
	((is-special exp 'if) (compile-if exp env))
	((is-special exp 'lambda) (compile-lambda exp env))
	((is-application exp) (apply-defined exp env))
	(t (error "bad expression ~A" exp))))
