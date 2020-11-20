# Arrows - Documentation

## Macro `->`
* The thread-first threading macro.
* Lambda list: `(-> &rest forms)`

Binds anonymous variables and threads them into subsequent forms as their first
arguments.

For example, the following form:

```lisp
(-> foo
    bar
    (baz)
    (quux 1 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux temp3 1 2 3)))
  temp4)
```

## Macro `->>`
* The thread-last threading macro.
* Lambda list: `(->> &rest forms)`.

Binds anonymous variables and threads them into subsequent forms as their last
arguments.

For example, the following form:

```lisp
(->> foo
     bar
     (baz)
     (quux 1 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux 1 2 3 temp3)))
  temp4)
```

## Macro `-<>`
* The diamond wand threading macro.
* Lambda list: `(-<> &rest forms)`

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their first arguments (like the `->` macro) otherwise.

The diamond symbols are tested by name, not by identity, like LOOP keywords.

The diamond wand does not descend into subforms in its search for diamonds;
see the `arrow-macros` system for an implementation that performs code walking.

For example, the following form:

```lisp
(-<> foo
     bar
     (baz)
     (quux 1 2 3)
     (fred 4 5 6 <>)
     (frob 7 <> 8 <> 9))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux temp3 1 2 3))
      (temp5 (fred 4 5 6 temp4))
      (temp6 (quux 7 temp5 8 temp5 9)))
  temp6)
```

## Macro `-<>>`
* The diamond spear threading macro.
* Lambda list: `(-<>> &rest forms)`

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their last arguments (like the `->` macro) otherwise.

The diamond symbols are tested by name, not by identity, like LOOP keywords.

The diamond wand does not descend into subforms in its search for diamonds;
see the `arrow-macros` system for an implementation that performs code walking.

For example, the following form:

```lisp
(-<>> foo
      bar
      (baz)
      (quux 1 2 3)
      (fred 4 5 6 <>)
      (frob 7 <> 8 <> 9))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux 1 2 3 temp3))
      (temp5 (fred 4 5 6 temp4))
      (temp6 (quux 7 temp5 8 temp5 9)))
  temp6)
```

## Macro `some->`
* The short-cicruiting thread-first threading macro.
* Lambda list: `(some-> &rest forms)`

Binds anonymous variables and threads them into subsequent forms as their first
arguments. If any form returns `nil`, the subsequent forms are not evaluated,
and `nil` is returned.

For example, the following form:

```lisp
(some-> foo
        bar
        (baz)
        (quux 1 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (and temp1 (bar temp1)))
      (temp3 (and temp2 (baz temp2)))
      (temp4 (and temp3 (quux temp3 1 2 3))))
  temp4)
```

## Macro `some->>`
* The short-cicruiting thread-last threading macro.
* Lambda list: `(sone->> &rest forms)`

Binds anonymous variables and threads them into subsequent forms as their last
arguments. If any form returns `nil`, the subsequent forms are not evaluated,
and `nil` is returned.

For example, the following form:

```lisp
(some->> foo
         bar
         (baz)
         (quux 1 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (and temp1 (bar temp1)))
      (temp3 (and temp2 (baz temp2)))
      (temp4 (and temp3 (quux 1 2 3 temp3))))
  temp4)
```

## Macro `some-<>`
* The short-cicruiting diamond thread-first threading macro.
* Lambda list: `(some-<> &rest forms)`

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their first arguments (like the `->` macro) otherwise.

If any form returns `nil`, the subsequent forms are not evaluated, and `nil` is
returned.

For example, the following form:

```lisp
(some-<> foo
         bar
         (baz :baz)
         (quux 1 <> 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (and temp1 (bar temp1)))
      (temp3 (and temp2 (baz temp2 :baz)))
      (temp4 (and temp3 (quux 1 temp3 2 3))))
  temp4)
```

## Macro `some-<>>`
* The short-cicruiting diamond thread-last threading macro.
* Lambda list: `(some-<> &rest forms)`

Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their last arguments (like the `->>` macro) otherwise.

If any form returns `nil`, the subsequent forms are not evaluated, and `nil` is
returned.

For example, the following form:

```lisp
(some-<>> foo
          bar
          (baz :baz)
          (quux 1 <> 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (and temp1 (bar temp1)))
      (temp3 (and temp2 (baz :baz temp2)))
      (temp4 (and temp3 (quux 1 temp3 2 3))))
  temp4)
```

## Macro `cond->`
* The conditional thread-first threading macro.
* Lambda list: `(cond-> &rest forms)`
  * Each form must have the structure of `(test . forms)`.

Binds a single anonymous variable and threads it into the subsequent forms as
their first arguments, but only when the test of a given form returns true.

For example, the following form:

```lisp
(cond-> foo
        (barp x y z)
        (bazp (baz))
        ((quuxp thing) (quux 1 2 3)))
```

Is equivalent to:

```lisp
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
  temp4)
```

## Macro `cond->>`
* The conditional thread-last threading macro.
* Lambda list: `(cond-> &rest forms)`
  * Each form must have the structure of `(test . forms)`.

Binds a single anonymous variable and threads it into the subsequent forms as
their last arguments, but only when the test of a given form returns true.

For example, the following form:

```lisp
(cond->> foo
         (barp x y z)
         (bazp (baz))
         ((quuxp thing) (quux 1 2 3)))
```

Is equivalent to:

```lisp
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
  temp4)
```

## Macro `cond-<>`
* The conditional diamond thread-first threading macro.
* Lambda list: `(cond-> &rest forms)`
  * Each form must have the structure of `(test . forms)`.
 
Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their first arguments (like the `->` macro) otherwise.

The binding is only effective when the test of a given form returns true.

If any form returns `nil`, the subsequent forms are not evaluated, and `nil` is
returned.

For example, the following form:

```lisp
(cond-<> foo
         (barp x y z)
         (bazp (baz))
         ((quuxp thing) (quux 1 <> 2 3)))
```

Is equivalent to:

```lisp
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
  temp4)
```

## Macro `cond-<>>`
* The conditional diamond thread-last threading macro.
* Lambda list: `(cond-> &rest forms)`
  * Each form must have the structure of `(test . forms)`.
 
Binds anonymous variables and threads them into subsequent forms:
  * by substituting diamond symbols (`<>`) if they are present in the form,
  * as their last arguments (like the `->` macro) otherwise.

The binding is only effective when the test of a given form returns true.

If any form returns `nil`, the subsequent forms are not evaluated, and `nil` is
returned.

For example, the following form:

```lisp
(cond-<> foo
         (barp x y z)
         (bazp (baz))
         ((quuxp thing) (quux 1 <> 2 3)))
```

Is equivalent to:

```lisp
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
  temp4)
```

## Macro `->*`
* The inverted thread-first threading macro.
* Lambda list: `(->* &rest forms)`

Binds anonymous variables and threads them into subsequent forms as their first
arguments. The order of the forms is altered, so that the last form is used as
the initialization form.

For example, the following form:

```lisp
(->* bar
     (baz)
     (quux 1 2 3)
     foo)
```

Is equivalent to:

```lisp
(-> foo
    bar
    (baz)
    (quux 1 2 3))
```

And therefore to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux temp3 1 2 3)))
  temp4)
```

## Macro `as->`
* The named threading macro.
* Lambda list: `(as-> initial-form variable &rest forms)`

Binds the provided variable to subsequent forms, which may make use of the
bound variable.

For example, the following form:

```lisp
(as-> foo var
      (bar var)
      (baz var)
      (quux 1 2 3))
```

Is equivalent to:

```lisp
(let* ((var foo)
       (var (bar var))
       (var (baz var))
       (var (quux 1 2 3)))
  var)
```

## Macro `as->*`
* The inverted named threading macro.
* Lambda list: `(as->* &rest forms)`

Binds the provided variable to subsequent forms, which may make use of the
bound variable. The order of the forms is altered, so that the last form is used
as the initialization form for the variable.

For example, the following form:

```lisp
(as->* var
       (bar var)
       (baz var)
       (quux 1 2 3)
       foo)
```

Is equivalent to:

```lisp
(as-> foo var
      (bar var)
      (baz var)
      (quux 1 2 3))
```

And therefore to:

```lisp
(let* ((var foo)
       (var (bar var))
       (var (baz var))
       (var (quux 1 2 3)))
  var)
```
