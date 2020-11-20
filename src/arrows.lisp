(defpackage #:arrows
  (:use #:common-lisp)
  (:export #:-> #:->> #:-<> #:-<>> #:->*
           #:some-> #:some->> #:some-<> #:some-<>>
           #:cond-> #:cond->> #:cond-<> #:cond-<>>
           #:as-> #:as->*))

(in-package #:arrows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrow expansion function

(defun ensure-cons (thing)
  (if (consp thing) thing (list thing)))

(defun make-value-form (value-fn prev-symbol form)
  (if (null prev-symbol)
      form
      (funcall value-fn prev-symbol (ensure-cons form))))

(defun expand-arrow
    (forms &key (symbol-fn (lambda () (gensym "VAR"))) (value-fn #'value-first))
  (let ((length (length forms)))
    (case length
      (0 'nil)
      (1 (first forms))
      (t (loop with symbols = (loop repeat length collect (funcall symbol-fn))
               for form in forms
               for prev-symbol = nil then next-symbol
               for next-symbol in symbols
               for value-form = (make-value-form value-fn prev-symbol form)
               collect `(,next-symbol ,value-form) into bindings
               finally (return `(let* ,bindings ,next-symbol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value functions

(defun value-first (symbol form)
  (destructuring-bind (head . tail) form
    (list* head symbol tail)))

(defun value-last (symbol form)
  (append form (list symbol)))

(defun as-value (symbol form)
  (declare (ignore symbol))
  form)

(defun some-value (value-fn symbol form)
  `(and ,symbol ,(funcall value-fn symbol form)))

(defun some-value-first (symbol form)
  (some-value #'value-first symbol form))

(defun some-value-last (symbol form)
  (some-value #'value-last symbol form))

(defun cond-value (arrow symbol form)
  (destructuring-bind (test . forms) form
    `(if ,test (,arrow ,symbol ,@forms) ,symbol)))

(defun cond-value-first (symbol form)
  (cond-value '-> symbol form))

(defun cond-value-last (symbol form)
  (cond-value '->> symbol form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diamond value functions

(defun diamond-value (value-fn symbol form)
  (flet ((diamondp (form) (and (symbolp form) (string= form "<>"))))
    (if (= 0 (count-if #'diamondp form))
        (funcall value-fn symbol form)
        (substitute-if symbol #'diamondp form))))

(defun diamond-value-first (symbol form)
  (diamond-value #'value-first symbol form))

(defun diamond-value-last (symbol form)
  (diamond-value #'value-last symbol form))

(defun some-diamond-value-first (symbol form)
  (some-value #'diamond-value-first symbol form))

(defun some-diamond-value-last (symbol form)
  (some-value #'diamond-value-last symbol form))

(defun cond-diamond-value-first (symbol form)
  (cond-value '-<> symbol form))

(defun cond-diamond-value-last (symbol form)
  (cond-value '-<>> symbol form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrow implementations

(defmacro -> (&rest forms)
  (expand-arrow forms))

(defmacro ->> (&rest forms)
  (expand-arrow forms :value-fn #'value-last))

(defmacro -<> (&rest forms)
  (expand-arrow forms :value-fn #'diamond-value-first))

(defmacro -<>> (&rest forms)
  (expand-arrow forms :value-fn #'diamond-value-last))

(defmacro some-> (&rest forms)
  (expand-arrow forms :value-fn #'some-value-first))

(defmacro some->> (&rest forms)
  (expand-arrow forms :value-fn #'some-value-last))

(defmacro some-<> (&rest forms)
  (expand-arrow forms :value-fn #'some-diamond-value-first))

(defmacro some-<>> (&rest forms)
  (expand-arrow forms :value-fn #'some-diamond-value-last))

(defmacro cond-> (&rest forms)
  (expand-arrow forms :value-fn #'cond-value-first))

(defmacro cond->> (&rest forms)
  (expand-arrow forms :value-fn #'cond-value-last))

(defmacro cond-<> (&rest forms)
  (expand-arrow forms :value-fn #'cond-diamond-value-first))

(defmacro cond-<>> (&rest forms)
  (expand-arrow forms :value-fn #'cond-diamond-value-last))

(defmacro ->* (&rest forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand-arrow forms)))

(defmacro as-> (initial-form var &rest forms)
  (let ((forms (cons initial-form forms)))
    (expand-arrow forms :symbol-fn (constantly var) :value-fn #'as-value)))

(defmacro as->* (var &rest forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand-arrow forms :symbol-fn (constantly var) :value-fn #'as-value)))
