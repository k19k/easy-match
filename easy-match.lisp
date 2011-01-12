(in-package :easy-match)

(defgeneric match-term (pattern expression conds bindings k))

(defmethod match-term ((pattern t) expression conds bindings k)
  (funcall k (nconc conds `((eq ,pattern ,expression))) bindings))

(defmethod match-term ((pattern null) expression conds bindings k)
  (funcall k (nconc conds `((null ,expression))) bindings))

(defmethod match-term ((pattern symbol) expression conds bindings k)
  (cond
    ((keywordp pattern) (funcall k (nconc conds `((eq ,pattern ,expression)))
				 bindings))
    ((equal (symbol-name pattern) "_") (funcall k conds bindings))
    (t (funcall k conds (nconc bindings `((,pattern ,expression)))))))

(defmethod match-term ((pattern cons) expression conds bindings k)
  (match-term (car pattern) `(car ,expression)
	      (nconc conds `(,expression (listp ,expression)))
	      bindings #'(lambda (conds* bindings*)
			   (match-term (cdr pattern) `(cdr ,expression)
				       conds* bindings* k))))

(defun match-cond-clause (expr pattern guard-expr &rest body)
  "Return a COND clause to match EXPR against PATTERN.  If GUARD-EXPR
  is not empty, it is included in the test form of the clause.  BODY
  and GUARD-EXPR may use symbols bound by PATTERN."
  (match-term pattern expr nil nil
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
		      ,@body)))))

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
