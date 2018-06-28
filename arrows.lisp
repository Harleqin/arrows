(in-package #:cl-user)

(defpackage #:arrows
  (:use #:common-lisp)
  (:export #:->
           #:->>
           #:-<>
           #:-<>>
           #:as->
           #:some->
           #:some->>
           #:cond->
           #:cond->>
           #:->*
           #:as->*))

(in-package #:arrows)

(defun simple-inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
        (list next acc))))

(defun expand-arrow (initial-form forms insert-fun)
  (reduce (simple-inserter insert-fun)
          forms
          :initial-value initial-form))

(defmacro -> (initial-form &rest forms)
  "Inserts INITIAL-FORM as first argument into the first of FORMS, the result
into the next, etc., before evaluation.  FORMS are treated as list designators."
  (expand-arrow initial-form forms #'insert-first))

(defmacro ->> (initial-form &rest forms)
  "Like ->, but the forms are inserted as last argument instead of first."
  (expand-arrow initial-form forms #'insert-last))

(defun diamond-inserter (insert-fun)
  (simple-inserter (lambda (acc next)
                     (case (count-if #'<>p next)
                       (0 (funcall insert-fun acc next))
                       (1 (substitute-if acc #'<>p next))
                       (t (let ((r (gensym "R")))
                            `(let ((,r ,acc))
                               ,(substitute-if r #'<>p next))))))))

(defmacro -<> (initial-form &rest forms)
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level
element, each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (reduce (diamond-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro -<>> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (reduce (diamond-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun <>p (form)
  "Predicate identifying the placeholders for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(defun insert-first (arg surround)
  "Inserts ARG into the list form SURROUND as its first argument, after the
operator."
  (list* (car surround)
         arg
         (cdr surround)))

(defun insert-last (arg surround)
  "Inserts ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

(defmacro as-> (initial-form var &rest forms)
  "Binds INITIAL-FORM to VAR, then successively each of FORMS to VAR, finally
returns the last value of VAR."
  `(let* ,(mapcar (lambda (form)
                    (list var form))
                  (cons initial-form forms))
     ,var))

(defun some-inserter (insert-fun)
  (lambda (acc next)
    (destructuring-bind (let* bindings var) acc
      `(,let* (,@bindings
               (,var (when ,var
                       ,(funcall insert-fun var next))))
         ,var))))

(defun expand-some (initial-form forms insert-fun)
  (let ((var (gensym "some")))
    (reduce (some-inserter (simple-inserter insert-fun))
            forms
            :initial-value `(let* ((,var ,initial-form))
                              ,var))))

(defmacro some-> (initial-form &rest forms)
  "Like ->, but short-circuits to nil as soon as either INITIAL-FORM or any of
FORMS return nil.  This is like all these forms are lifted to the maybe monad."
  (expand-some initial-form forms #'insert-first))

(defmacro some->> (initial-form &rest forms)
  "Like some->, but with insertion behaviour as in ->>."
  (expand-some initial-form forms #'insert-last))

(defun cond-inserter (insert-fun)
  (lambda (acc next)
    (destructuring-bind (let* bindings var) acc
      (destructuring-bind (test . forms) next
        `(,let* (,@bindings
                 (,var (if ,test
                           ,(expand-arrow var forms insert-fun)
                           ,var)))
           ,var)))))

(defun expand-cond (initial-form clauses insert-fun)
  (let ((var (gensym "cond")))
    (reduce (cond-inserter (simple-inserter insert-fun))
            clauses
            :initial-value `(let* ((,var ,initial-form))
                              ,var))))

(defmacro cond-> (initial-form &rest clauses)
  "CLAUSES is a list of clauses similar to COND clauses, each clause comprising
first a test form, then a body of further forms.  Cond-> evaluates INITIAL-FORM
to a value, then for each clause whose test evaluates to true, pipes (as in ->)
the value through each form in the body of the clause.  Note that unlike in
COND, there is no short-circuiting: each clause gets tested regardless of the
outcome of the clauses before."
  (expand-cond initial-form clauses #'insert-first))

(defmacro cond->> (initial-form &rest clauses)
  "Like cond->, but with insertion behaviour as in ->>."
  (expand-cond initial-form clauses #'insert-last))

(defmacro ->* (&rest forms)
  "Like ->, but the last form is used as initial form, then the remaining forms
used as in ->.  This is intended for inversing the default in a ->> form.

Example:

    (->> 3
         (/ 12)
         (->* (/ 2)))
    => 2"
  `(-> ,@(append (last forms) (butlast forms))))

(defmacro as->* (var &rest forms)
  "Shorthand for the combination of ->* and as->: the last form is used for
initial binding, then the remaining forms used as in as->.  This is intended for
overriding the default in a ->> form."
  `(as-> ,@(last forms) ,var
         ,@(butlast forms)))
