(in-package :easy-match)

(defun match-clause (expr pattern)
  "Match PATTERN against EXPR.  Return two lists, the first containing
  conditions that EXPR must satisfy, and the second containing
  bindings for values in EXPR."
  (labels ((f (e p cs bs k)
	     (cond
	       ((not p) (funcall k (nconc cs `((not ,e))) bs))
	       ((keywordp p) (funcall k (nconc cs `((eq ,p ,e))) bs))
	       ((symbolp p) (if (equal (symbol-name p) "_") (funcall k cs bs)
				(funcall k cs (nconc bs `((,p ,e))))))
	       ((listp p) (flet ((this-k (car-conds car-bindings)
				   (f `(cdr ,e) (cdr p)
				      car-conds car-bindings k)))
			    (f `(car ,e) (car p)
			       (nconc cs `((listp ,e))) bs #'this-k)))
	       (t (funcall k (nconc cs `((eq ,p ,e))) bs)))))
    (f expr pattern nil nil #'values)))

(defmacro match (expr &rest clauses)
  "Do simple pattern matching on EXPR, binding any symbols in the
  patterns in CLAUSES for their respective bodies.  Each clause has
  the form (pattern &rest body).

  MATCH works similarly to COND: clauses are tried in sequential
  order, and the first matching pattern decides which clause executes
  and returns a value.  If no patterns are matched, NIL is returned.

  The symbol _ (a single underscore) may be used in patterns to
  indicate that the value in that position is ignored.  This may be
  used multiple times in a pattern.

  Currently only elements of lists may be matched for binding.  All
  other data is compared with EQ."
  (let ((esym (gensym)))
    `(let ((,esym ,expr))
       (declare (ignorable ,esym))
       (cond
	 ,@(mapcar #'(lambda (clause)
		       (let ((pattern (car clause))
			     (body (cdr clause)))
			 (multiple-value-bind (conds bindings)
			     (match-clause esym pattern)
			   `((and ,@conds) (let ,bindings
					     ,@body)))))
		   clauses)))))
