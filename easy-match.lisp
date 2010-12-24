(in-package :easy-match)

(defun match-clause (expr pattern)
  "Match PATTERN against EXPR.  Return two lists, the first containing
  conditions that EXPR must satisfy, and the second containing
  bindings for values in EXPR."
  (labels ((f (e p cs bs k)
	     (cond
	       ((not p) (funcall k (nconc cs `((not ,e))) bs))
	       ((keywordp p) (funcall k (nconc cs `((eq ,p ,e))) bs))
	       ((symbolp p) (funcall k cs (nconc bs `((,p ,e)))))
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

  Example: a pattern matching any expression.  This may be useful as a
  default clause.  Any clauses after this one will never execute.

    (match foo
      (x (format t \"~A\" x))) ; x is bound to the value of foo

  Example: required values.  In this example, FOO must be a list
  containing 0, 1, 2, some value, and the keyword :BAR.  X is bound to
  the unspecified value.

    (match foo
      ((0 1 2 x :bar) x))

  Currently only elements of lists may be matched for binding.  All
  other data is compared with EQ."
  (let ((esym (gensym)))
    `(let ((,esym ,expr))
       (cond
	 ,@(mapcar #'(lambda (clause)
		       (let ((pattern (car clause))
			     (body (cdr clause)))
			 (multiple-value-bind (conds bindings)
			     (match-clause esym pattern)
			   `((and ,@conds) (let ,bindings
					     ,@body)))))
		   clauses)))))
