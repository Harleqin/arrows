# Arrows

Implements threading macros, inspired by Clojure (both core and
the [swiss-arrows](https://github.com/rplevy/swiss-arrows) library).

This is an ASDF system providing the package `arrows`.  Its home is at
https://gitlab.com/Harleqin/arrows, with a mirror at
https://github.com/Harleqin/arrows.

## Overview

You get:

- the basic “thrushing” arrows `->` and `->>`,
- “diamond” arrows `-<>` and `-<>>`,
- binding arrow `as->`,
- “maybe” arrows `some->` and `some->>`,
- conditional arrows `cond->` and `cond->>`, and
- “double arrow cancellers” `->*` and `as->*`.

As far as I see, `->*` and `as->*` are new.  Their purpose is to be nested in
other threading forms to temporarily supplant their behaviour (see “Nesting”
below).

## Other arrow libraries

- [`arrow-macros`](https://github.com/hipeta/arrow-macros)
- [`cl-arrows`](https://github.com/nightfly19/cl-arrows) is superseded by `arrows`.

## Notable differences to Clojure and swiss-arrows

- `Cond->` and `cond->>` use one additional paren nesting for the clauses, so
  that each clause can contain multiple forms to thread/execute.

- `-<>` and `-<>>` do not support literals to insert the `<>` placeholder.  The
  placeholder really only works at the outermost level of the threaded forms.
  The reason for this is mostly that Common Lisp does not have so many literal
  syntax elements (by default) where it would make sense to do this kind of
  insertion.  If you do need anything fancy, use `as->` or `as->*` for a real
  lexical binding.

## Notable differences to arrow-macros

- `Cond->` and `cond->>` use one additional paren nesting for the clauses, so
  that each clause can contain multiple forms to thread/execute.

- `-<>` and `-<>>` do not use a code walker to find out whether a placeholder is
  present in the next threaded form.  The placeholder only works at the
  outermost level of the threaded forms.  This reduces the dependencies of
  `arrows` (there are none at present).  Instead, the recommendation is to use
  binding arrows `as->` or `as->*`, possibly nested (see below).

- There is no `some-<>` nor `some-<>>` yet.  Instead, you can use nested `as->`
  or `as->*` forms (see below).

## Nesting

One useful idiom is to nest these arrows.  The basic example is to use `->>`
inside `->`:

    (-> deeply-nested-plist
        (getf :foo)
        (getf :bar)
        (->> (mapcar #'reverse)))

This inspired the discovery of `->*`, which enables the inverse nesting:

    (->> deeply-nested-alist
         (assoc :foo)
         cdr
         (assoc :bar)
         cdr
         (->* (mod 3))
         (expt 2))

Generally useful for overriding defaults are `as->` and `as->*`:

    (-> 3
        (as-> $
              (< x $ y))
        not)

    (some->> 15
             (as->* $
                    (progn
                      (format t debug-formatter $)
                      $))
             (/ 75))

However, don't overdo it!  This quickly leads to an unreadable mess.  You may
well be better off with a few explicit `let` bindings.

## Documentation

#### `->` initial-form _&rest_ forms => results
_[macro]_ Inserts INITIAL-FORM as first argument into the first of FORMS, the
result into the next, etc., before evaluation.  FORMS are treated as list
designators.

#### `->>` initial-form _&rest_ forms => results
_[macro]_ Like `->`, but the forms are inserted as last argument instead of
first.

#### `->*` _&rest_ forms => results
_[macro]_ Like `->`, but the last form is used as initial form, then the
remainung forms as in `->`.  This is intended for inversing the default in a
`->>` form.

#### `-<>` initial-form _&rest_ forms => results
_[macro]_ Like `->`, but if a form in FORMS has one or more symbols named `<>`
as top-level element, each such symbol is substituted by the primary result of
the form accumulated so far, instead of it being inserted as first argument.
Also known as diamond wand.

#### `-<>>` initial-form _&rest_ forms => results
_[macro]_ Like `-<>`, but if a form in FORMS has no symbols named `<>` as
top-level element, insertion is done like in `->>`.  Also known as diamond
spear.

#### `as->` initial-form var _&rest_ forms => results
_[macro]_ Binds INITIAL-FORM to VAR, then successively each of FORMS to VAR,
finally returns the last value of VAR.

#### `as->*` var _&rest_ forms => results
_[macro]_ Shorthand for the combination of `->*` and `as->`: the last form is
used for initial binding, then the remaining forms used as in `as->`.  This is
intended for overriding the default in a `->>` form.

#### `some->` initial-form _&rest_ forms => results
_[macro]_ Like `->`, but short-circuits to nil as soon as either INITIAL-FORM or
any of FORMS return nil.  This is like all these forms are lifted to the maybe
monad.

#### `some->>` initial-form _&rest_ forms => results
_[macro]_ Like `some->`, but with insertion behaviour as in `->>`.

#### `cond->` initial-form _&rest_ clauses => results
_[macro]_ CLAUSES is a list of clauses similar to COND clauses, each clause
comprising first a test form, then a body of further forms.  `Cond->` evaluates
INITIAL-FORM to a value, then for each clause whose test evaluates to true,
pipes (as in `->`) the value through each form in the body of the clause.  Note
that unlike in COND, there is no short-circuiting: each clause gets tested
regardless of the outcome of the clauses before.

#### `cond->>` initial-form _&rest_ clauses => results
_[macro]_ Like `cond->`, but with insertion behaviour as in `->>`.

## Examples

    (-> 3
        /)  ; insert into designated list (/)
    => 1/3

    (-> 3
        (expt 2))  ; insert as first argument
    => 9

    (->> 3
         (expt 2))  ; insert as last argument
    => 8

    (-<>> (list 1 2 3)
          (remove-if #'oddp <> :count 1 :from-end t) ; substitute <>
          (reduce #'+)                               ; insert last
          /)                                         ; list designator
    => 1/3

    (let ((x 3))
      (-<> (incf x)     ; (let ((r (incf x)))
           (+ <> <>)))  ;   (+ r r))
    => 8

    (->> 3
         (/ 12)         ; (/ 12 3) => 4
         (->* (/ 2)))   ; (/ 4 2)  => 2
    => 2

    (flet ((say (n)
             (cond->> nil
                      ((zerop (mod n 3)) (cons "Fizz"))
                      ((zerop (mod n 5)) (cons "Buzz"))
                      (t (->* (or (list (princ-to-string n))))
                         reverse
                         (apply #'concatenate 'string)))))
      (mapcar #'say '(9 10 11 12 13 14 15)))
    => ("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz")
