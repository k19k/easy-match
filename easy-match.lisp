(in-package :easy-match)

(defun match-cond-clause (expr pattern guard-expr &rest body)
  "Return a COND clause to match EXPR against PATTERN.  If GUARD-EXPR
  is not empty, it is included in the test form of the clause.  BODY
  and GUARD-EXPR may use symbols bound by PATTERN."
  (labels ((f (e p cs bs k)
	     (cond
	       ((null p) (funcall k (nconc cs `((null ,e))) bs))
	       ((keywordp p) (funcall k (nconc cs `((eq ,p ,e))) bs))
	       ((symbolp p) (if (equal (symbol-name p) "_") (funcall k cs bs)
				(funcall k cs (nconc bs `((,p ,e))))))
	       ((listp p) (flet ((this-k (car-conds car-bindings)
				   (f `(cdr ,e) (cdr p)
				      car-conds car-bindings k)))
			    (f `(car ,e) (car p)
			       (nconc cs `(,e (listp ,e))) bs #'this-k)))
	       (t (funcall k (nconc cs `((eq ,p ,e))) bs)))))
    (f expr pattern nil nil
       #'(lambda (conds bindings)
	   `((and ,@conds
		  ,@(cond
		     ((and guard-expr bindings)
		      `((let ,bindings
			  (declare (ignorable ,@(mapcar #'car bindings)))
			  ,guard-expr)))
		     (guard-expr `(,guard-expr))))
	     (let ,bindings
	       (declare (ignorable ,@(mapcar #'car bindings)))
	       ,@body))))))

(defmacro match (expr &rest clauses)
  "Do simple pattern matching on EXPR, binding any symbols in the
  patterns in CLAUSES for their respective bodies.  Each clause has
  the form (pattern guard-expr &rest body) or (pattern expr).

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
		       (destructuring-bind (pattern guard-expr &rest body)
			   clause
			 (if (not body)
			     (match-cond-clause esym pattern nil guard-expr)
			     (apply #'match-cond-clause
				    esym pattern guard-expr body))))
		   clauses)))))
