(in-package #:arrows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation

(setf (documentation '-> 'function)
      "The thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms as their first
arguments.

For example, the following form:

  (-> foo
      bar
      (baz)
      (quux 1 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (bar temp1))
        (temp3 (baz temp2))
        (temp4 (quux temp3 1 2 3)))
    temp4)")

(setf (documentation '->> 'function)
      "The thread-last threading macro.

Binds anonymous variables and threads them into subsequent forms as their last
arguments.

For example, the following form:

  (->> foo
       bar
       (baz)
       (quux 1 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (bar temp1))
        (temp3 (baz temp2))
        (temp4 (quux 1 2 3 temp3)))
    temp4)")

(setf (documentation '-<> 'function)
      "The diamond wand threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their first arguments (like the -> macro) otherwise.

The diamond symbols are tested by name, not by identity, like LOOP keywords.

The diamond wand does not descend into subforms in its search for diamonds;
see the ARROW-MACROS system for an implementation that performs code walking.

For example, the following form:

  (-<> foo
       bar
       (baz)
       (quux 1 2 3)
       (fred 4 5 6 <>)
       (frob 7 <> 8 <> 9))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (bar temp1))
        (temp3 (baz temp2))
        (temp4 (quux temp3 1 2 3))
        (temp5 (fred 4 5 6 temp4))
        (temp6 (quux 7 temp5 8 temp5 9)))
    temp6)")

(setf (documentation '-<>> 'function)
      "The diamond spear threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their last arguments (like the -> macro) otherwise.

The diamond symbols are tested by name, not by identity, like LOOP keywords.

The diamond wand does not descend into subforms in its search for diamonds;
see the ARROW-MACROS system for an implementation that performs code walking.

For example, the following form:

  (-<>> foo
        bar
        (baz)
        (quux 1 2 3)
        (fred 4 5 6 <>)
        (frob 7 <> 8 <> 9))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (bar temp1))
        (temp3 (baz temp2))
        (temp4 (quux 1 2 3 temp3))
        (temp5 (fred 4 5 6 temp4))
        (temp6 (quux 7 temp5 8 temp5 9)))
    temp6)")

(setf (documentation 'some-> 'function)
      "The short-cicruiting thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms as their first
arguments. If any form returns NIL, the subsequent forms are not evaluated, and
NIL is returned.

For example, the following form:

  (some-> foo
          bar
          (baz)
          (quux 1 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (and temp1 (bar temp1)))
        (temp3 (and temp2 (baz temp2)))
        (temp4 (and temp3 (quux temp3 1 2 3))))
    temp4)")

(setf (documentation 'some->> 'function)
      "The short-cicruiting thread-last threading macro.

Binds anonymous variables and threads them into subsequent forms as their last
arguments. If any form returns NIL, the subsequent forms are not evaluated, and
NIL is returned.

For example, the following form:

  (some->> foo
           bar
           (baz)
           (quux 1 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (and temp1 (bar temp1)))
        (temp3 (and temp2 (baz temp2)))
        (temp4 (and temp3 (quux 1 2 3 temp3))))
    temp4)")

(setf (documentation 'some-<> 'function)
      "The short-cicruiting diamond thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their first arguments (like the -> macro) otherwise.

If any form returns NIL, the subsequent forms are not evaluated, and NIL is
returned.

For example, the following form:

  (some-<> foo
           bar
           (baz :baz)
           (quux 1 <> 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (and temp1 (bar temp1)))
        (temp3 (and temp2 (baz temp2 :baz)))
        (temp4 (and temp3 (quux 1 temp3 2 3))))
    temp4)")

(setf (documentation 'some-<>> 'function)
      "The short-cicruiting diamond thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their last arguments (like the ->> macro) otherwise.

If any form returns NIL, the subsequent forms are not evaluated, and NIL is
returned.

For example, the following form:

  (some-<> foo
           bar
           (baz :baz)
           (quux 1 <> 2 3))

Is equivalent to:

  (let ((temp1 foo)
        (temp2 (and temp1 (bar temp1)))
        (temp3 (and temp2 (baz :baz temp2)))
        (temp4 (and temp3 (quux 1 temp3 2 3))))
    temp4)")

(setf (documentation 'cond-> 'function)
      "The conditional thread-first threading macro.

Binds anonymous variables and threads them into the subsequent forms as their
first arguments, but only when the test of a given form returns true.

For example, the following form:

  (cond-> foo
          (barp x y z)
          (bazp (baz))
          ((quuxp thing) (quux 1 2 3)))

Is equivalent to:

  (let* ((temp1 foo)
         (temp2 (if barp
                    (-> temp1 x y z)
                    temp1))
         (temp3 (if bazp
                    (-> temp2 (baz))
                    temp2))
         (temp4 (if (quuxp thing)
                    (-> temp3 (quux 1 2 3))
                    temp3)))
    temp4)")

(setf (documentation 'cond->> 'function)
      "The conditional thread-last threading macro.

Binds anonymous variables and threads them into the subsequent forms as their
last arguments, but only when the test of a given form returns true.

For example, the following form:

  (cond->> foo
           (barp x y z)
           (bazp (baz))
           ((quuxp thing) (quux 1 2 3)))

Is equivalent to:

  (let* ((temp1 foo)
         (temp2 (if barp
                    (->> temp1 x y z)
                    temp1))
         (temp3 (if bazp
                    (->> temp2 (baz))
                    temp2))
         (temp4 (if (quuxp thing)
                    (->> temp3 (quux 1 2 3))
                    temp3)))
    temp4)")

(setf (documentation 'cond-<> 'function)
      "The short-cicruiting diamond thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their first arguments (like the -> macro) otherwise.

The binding is only effective when the test of a given form returns true.

If any form returns NIL, the subsequent forms are not evaluated, and NIL is
returned.

For example, the following form:

  (cond-<> foo
           (barp x y z)
           (bazp (baz))
           ((quuxp thing) (quux 1 <> 2 3)))

Is equivalent to:

  (let* ((temp1 foo)
         (temp2 (if barp
                    (-<> temp1 x y z)
                    temp1))
         (temp3 (if bazp
                    (-<> temp2 (baz))
                    temp2))
         (temp4 (if (quuxp thing)
                    (-<> temp3 (quux 1 <> 2 3))
                    temp3)))
    temp4)")

(setf (documentation 'cond-<>> 'function)
      "The short-cicruiting diamond thread-last threading macro.

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (<>) if they are present in the form,
  * as their last arguments (like the ->> macro) otherwise.

The binding is only effective when the test of a given form returns true.

If any form returns NIL, the subsequent forms are not evaluated, and NIL is
returned.

For example, the following form:

  (cond-<>> foo
            (barp x y z)
            (bazp (baz))
            ((quuxp thing) (quux 1 <> 2 3)))

Is equivalent to:

  (let* ((temp1 foo)
         (temp2 (if barp
                    (-<>> temp1 x y z)
                    temp1))
         (temp3 (if bazp
                    (-<>> temp2 (baz))
                    temp2))
         (temp4 (if (quuxp thing)
                    (-<>> temp3 (quux 1 <> 2 3))
                    temp3)))
    temp4)")

(setf (documentation '->* 'function)
      "The inverted thread-first threading macro.

Binds anonymous variables and threads them into subsequent forms as their first
arguments. The order of the forms is altered, so that the last form is used as
the initialization form.

For example, the following form:

  (->* bar
       (baz)
       (quux 1 2 3)
       foo)

Is equivalent to:

  (-> foo
      bar
      (baz)
      (quux 1 2 3))

And therefore to:

  (let ((temp1 foo)
        (temp2 (bar temp1))
        (temp3 (baz temp2))
        (temp4 (quux temp3 1 2 3)))
    temp4)")

(setf (documentation 'as-> 'function)
      "The named threading macro.

Binds the provided variable to subsequent forms, which may make use of the
bound variable.

For example, the following form:

  (as-> foo var
        (bar var)
        (baz var)
        (quux 1 2 3))

Is equivalent to:

  (let* ((var foo)
         (var (bar var))
         (var (baz var))
         (var (quux 1 2 3)))
    var)")

(setf (documentation 'as->* 'function)
      "The inverted named threading macro.

Binds the provided variable to subsequent forms, which may make use of the
bound variable. The order of the forms is altered, so that the last form is used
as the initialization form for the variable.

For example, the following form:

  (as->* var
         (bar var)
         (baz var)
         (quux 1 2 3)
         foo)

Is equivalent to:

  (as-> foo var
        (bar var)
        (baz var)
        (quux 1 2 3))

And therefore to:

  (let* ((var foo)
         (var (bar var))
         (var (baz var))
         (var (quux 1 2 3)))
    var)")
