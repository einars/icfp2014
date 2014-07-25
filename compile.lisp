(defun compile-lm (exp env)
  (cond ((numberp exp) `((ldc ,exp)))))
