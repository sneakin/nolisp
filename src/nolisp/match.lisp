(in-package :nolisp)

(defun question-sym? (sym)
  "Returns true for symbols that begin with a question mark."
  (if (symbolp sym)
      (eq 0 (position #\? (symbol-name sym)))))

(defun match-var? (sym &optional (allow-keywords t))
  "The default variable predicate for ~match~ that returns true of question marked symbols and keywords when ~allow-keywords~ is true."
  (or (question-sym? sym)
      (and allow-keywords (keywordp sym))))

(defun match-atom (pattern
		   atom
		   &key
		   syms
		   (allow-keywords t)
		   (varp (partial-after #'match-var? allow-keywords))
		   (test #'equal))
  "When ~match~ enrounters an atom, this is called to determine if the atom from the pattern and input pass the ~varp~ predicate. The value is stored into the ~syms~ alist. Finally the updated list is returned with a second value that is true when the value equaled the prior stored value."
  (cond
   ;; The pattern is a variable identifier:
   ((funcall varp pattern)
    ;; Compare the input value with any existing stored value.
     (let ((old-value (assoc pattern syms)))
       (if old-value
	   ;; Return the existing alist and if the stored and input
	   ;; values pass the test.
           (if (funcall test atom (cdr old-value))
               (values syms T)
             (values syms nil))
	 ;; New value, so add to the alist and return that.
         (values (acons pattern atom syms) T))))
   ;; Pattern and input atom matches
   ((funcall test pattern atom) (values syms T))
   ;; Neither so return the alist.
   (t (values syms nil))))

(defun match (pattern
	      input
	      &key
	      syms
	      (allow-keywords t)
	      (varp (partial-after #'match-var? allow-keywords))
	      (test #'equal))
  "Takes a list of structured data to compare and extract variable data. ~pattern~ is the structured list containing identifiers that cause ~varp~ to return true. By default these are symbols that start with a question mark or a keyword when ~allowed-keywords~ is true. Match variables may appear multiple times, but every match in the input must have the same value. ~test~ specifies an equality function to match the non-variable elements of the pattern. ~syms~ is the accumulator of results.

The results are returned as an alist of (variable . value) pairs, or null of ~pattern~ and ~input~ have a different form. Ajsecond true value is returned when the pattern is a variable identifier.

Example: (match '(pair ( ?name ?value )) '(pair (data 123)))
          => '((?name . data) (?value . 123))
         (match '(:x :y :x) '(1 2 1)) => '((:x . 1) (:y . 2))
         (match '(:x :y :x) '(1 2 3)) => nil"
  (cond
   ;; Both the pattern and input are atoms, so check against
   ;; and update the syms list with the value
   ((or (atom pattern) (atom input))
    (multiple-value-bind (syms matched)
       (match-atom pattern input
                   :syms syms
                   :allow-keywords allow-keywords
		   :varp varp
                   :test test)
       (if matched (values syms t))))
   ;; The pattern and input are lists that need to have each element matched.
   (t (multiple-value-bind (syms matched)
         (match (first pattern) (first input)
                :syms syms
                :allow-keywords allow-keywords
		:varp varp
                :test test)
	 ;; Keep matching if the recursive match did match.
         (if matched
             (match (rest pattern) (rest input)
                    :syms syms
                    :allow-keywords allow-keywords
		    :varp varp
                    :test test))))))

(defun match-vars (pattern &optional acc &key (varp #'match-var?))
  (reduce #'(lambda (acc el)
	      (cond
		((funcall varp el) (cons el acc))
		((listp el) (match-vars el acc :varp varp))
		(t acc)))
	  (fix-improper-list pattern)
	  :initial-value acc))

(defun match-var-name (n)
  (cond
    ((question-sym? n) (intern (subseq (symbol-name n) 1)))
    ((keywordp n) (intern (symbol-name n)))
    (t n)))

(defmacro match-list-bind (keys lst &rest body)
  (assoc-bind-gen keys lst :key-namer #'match-var-name :body body))

(defmacro when-match-bind (matched pattern input &rest body)
  (let ((msym (gensym "match")))
    `(multiple-value-bind (,msym ,matched)
	 (match ',pattern ,input)
       (match-list-bind ,(match-vars pattern) ,msym ,@body))))

(defmacro match-bind (pattern input yes &optional no)
  (let ((msym (gensym "match"))
	(yessym (gensym "matched")))
    `(multiple-value-bind (,msym ,yessym)
	 (match ',pattern ,input)
       (if ,yessym
	   (match-list-bind ,(match-vars pattern) ,msym ,yes)
	   ,no))))

(define-condition match-error (simple-error)
  ((pattern :initarg :pattern :reader match-error-pattern)
   (input :initarg :input :reader match-error-input))
  (:report (lambda (condition stream)
	     (format stream "~a did not match ~a~%"
		     (match-error-input condition)
		     (match-error-pattern condition)))))

(defmacro match-bind! (pattern input &rest body)
  `(match-bind ,pattern ,input
	       ,(maybe-wrap-with-progn body)
	       (error 'match-error :pattern ',pattern :input ,input)))

(defun match-case-cases (input cases)
  (cond
    ((eq nil cases) nil)
    ((eq t (first (first cases))) (maybe-wrap-with-progn (rest (first cases))))
    (t `(match-bind ,(first (first cases)) ,input
		    ,(maybe-wrap-with-progn (rest (first cases)))
		    ,(match-case-cases input (rest cases))))))

(defmacro match-case (input &rest cases)
  (match-case-cases input cases))
