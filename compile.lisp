(defparameter *linker-symbols* nil)
(defparameter *macro-symbols* nil)

(defun el-num (var frame)
  (- (length frame) (length (member var frame))))

(defun find-symbol-lm (var env &optional (op 'ld) (depth 0))
  (cond ((null env) (error "unknown variable ~A" var))
	((member var (first env)) `(,op ,depth ,(el-num var (first env))))
	(t (find-symbol-lm var (rest env) op (1+ depth)))))

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
    (cons . cons)
    (break . brk)))

(defun is-builtin (exp)
  (and (consp exp) (assoc (first exp) *builtin-ops*)))

(defun get-builtin-opcode (op)
  (list (list (cdr (assoc op *builtin-ops*)))))

(defun compile-builtin (exp env)
  (append (compile-list (rest exp) env)
	  (get-builtin-opcode (first exp))))

(defun is-application (exp)
  (and (consp exp) (symbolp (first exp))))

(defun compile-list (exp-list env &optional padding)
  (cond ((null exp-list) nil)
	(t (append (compile-lm (first exp-list) env)
		   (when (> (length exp-list) 1) padding)
		   (compile-list (rest exp-list) env padding)))))

(defun apply-defined (exp env)
  (append (compile-list (rest exp) env)
	  `(,(find-symbol-lm (first exp) env)
	    (ap ,(length (rest exp))))))

(defun is-special (exp sym)
  (and (consp exp) (eq (first exp) sym)))

(defun compile-block (sym body env linkage)
  (push (cons sym (append (compile-lm body env) linkage)) *linker-symbols*))

(defun lambda-env (exp env)
  (if (not (eq 'main (first exp)))
      (cons (second exp) env)
      env))

(defun prognize (exp)
  `(progn ,@(cddr exp)))

(defun compile-lambda (exp env &optional (lambda-sym (gensym)))
  (unless (listp (second exp)) (error "invalid lambda list ~A" (second exp)))
  (compile-block lambda-sym (prognize exp) (lambda-env exp env) '((rtn)))
  `((ldf ,lambda-sym)))

(defun compile-if (exp env)
  (let ((true-sym (gensym))
	(false-sym (gensym)))
    (compile-block true-sym (third exp) env '((join)))
    (compile-block false-sym (fourth exp) env '((join)))
    (append (compile-lm (second exp) env)
	    `((sel ,true-sym ,false-sym)))))

(defun compile-funcall (exp env)
  (append (compile-list (cddr exp) env)
	  (compile-lm (second exp) env)
	  `((ap ,(length (cddr exp))))))

(defun compile-define (exp env)
  (compile-lambda (rest exp) env (second exp)))

(defun compile-defasm (exp)
  (push (cons (second exp) (third exp)) *linker-symbols*)
  `(ldf ,(second exp)))

(defun is-macro (exp)
  (assoc (first exp) *macro-symbols*))

(defun compile-macro (exp env)
  (compile-lm (apply (cdr (assoc (first exp) *macro-symbols*))
		     (rest exp))
	      env))

(defun compile-set (exp env)
  `(,@(compile-lm (third exp) env)
    ,(find-symbol-lm (second exp) env 'st)
      (ldc 0)))

(defun compile-print (exp env)
  `(,@(compile-lm (second exp) env)
      (dbug) (ldc 0)))

(defun compile-progn (exp env)
  (let ((discard-op (find-symbol-lm '*** env 'st)))
    (compile-list (rest exp) env (list discard-op))))

(defun compile-lm (exp env)
  (cond ((numberp exp) `((ldc ,exp)))
	((symbolp exp) (list (find-symbol-lm exp env)))
	((is-builtin exp) (compile-builtin exp env))
	((is-special exp 'if) (compile-if exp env))
	((is-special exp 'set) (compile-set exp env))
	((is-special exp 'print) (compile-print exp env))
	((is-special exp 'progn) (compile-progn exp env))
	((is-special exp 'defun) (compile-define exp env))
	((is-special exp 'defasm) (compile-defasm exp))
	((is-special exp 'lambda) (compile-lambda exp env))
	((is-special exp 'funcall) (compile-funcall exp env))
	((is-special exp 'defvar) nil)
	((is-macro exp) (compile-macro exp env))
	((is-application exp) (apply-defined exp env))
	(t (error "bad expression ~A" exp))))

(defmacro define-lm-macro (name lambda-list &body body)
  `(push (cons ',name (lambda ,lambda-list
		       ,@body))
	 *macro-symbols*))

(define-lm-macro cond (&rest expressions)
  (if expressions
      (let ((expr (copy-tree (first expressions))))
	`(if ,(car expr)
	     ,(cadr expr)
	     (cond ,@(copy-tree (rest expressions)))))
      0))

(define-lm-macro list (&rest elements)
  (if elements
      `(cons ,(car elements) (list ,@(cdr elements)))
      0))

(define-lm-macro and (&rest clauses)
  (if clauses
      `(if ,(car clauses) (and ,@(cdr clauses)) 0)
      1))

(define-lm-macro or (&rest clauses)
  (if clauses
      `(if ,(car clauses) 1 (or ,@(cdr clauses)))
      0))

(define-lm-macro let (var-list &rest body)
  `(funcall (lambda ,(mapcar #'car var-list)
	      ,@body)
	    ,@(mapcar #'cadr var-list)))

(defun get-top-level (program)
  (mapcar #'second program))

(defun read-program-defines (stream &optional accumulator)
  (let ((define (read stream nil)))
    (if (null define)
	accumulator
	(read-program-defines stream (cons define accumulator)))))

(defun read-program (filename)
  (with-open-file (stream filename :direction :input)
    (read-program-defines stream)))

(defun init-globals (top-level-name)
  (if (member (first top-level-name) '(defun defasm))
      `(ldf ,(second top-level-name))
      `(ldc ,(or (third top-level-name) 0))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defparameter *symbol-offsets* nil)

(defun calculate-offsets (offset &optional (seq *linker-symbols*))
  (when (consp seq)
    (cons (cons (car (first seq)) offset)
	  (calculate-offsets (+ offset (length (cdr (first seq))))
			     (rest seq)))))

(defun flatten-chunks (entry)
  (setf *symbol-offsets* (calculate-offsets (length entry)))
  (append entry (mappend #'cdr *linker-symbols*)))

(defun flatten-program (top-level)
  (flatten-chunks
   `((dum ,(length top-level))
     ,@(mapcar #'init-globals top-level)
     (ldf main)
     (rap ,(length top-level))
     (stop))))

(defun resolve-symbol (sym)
  (if (not (symbolp sym))
      sym
      (let ((offset (assoc sym *symbol-offsets*)))
	(if offset
	    (cdr offset)
	    (error "unknown symbol ~A" sym)))))

(defun resolve-symbols (instruction-body)
  (when instruction-body
    (cons (resolve-symbol (first instruction-body))
	  (resolve-symbols (rest instruction-body)))))

(defun resolve-instruction (instruction)
  (cons (first instruction) (resolve-symbols (rest instruction))))

(defun resolve-links (program)
  (mapcar #'resolve-instruction program))

(defun print-program (program)
  (format t "~{~{~A ~}~%~}" program))

(defun compile-program (filename)
  (let* ((*linker-symbols* nil)
	 (*symbol-offsets* nil)
	 (program (append (list '(defvar ***)) (read-program filename))))
    (compile-list program (list (get-top-level program) '(arg1 arg2)))
    (print-program (resolve-links (flatten-program program)))))
