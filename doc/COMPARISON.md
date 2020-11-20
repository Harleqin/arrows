# Arrows - Comparison

## Overview

* [`arrow-macros`](https://github.com/hipeta/arrow-macros)
  * Supports diamonds in nested code forms
  * Requires `hu.dwim.walker` as a dependency
  * Does not provide `cond-<>` or `cond-<>>`
* [`cl-arrows`](https://github.com/nightfly19/cl-arrows)
  * **Do not use** - the library is unmaintained and has licensing issues.

## Notable differences to Clojure threading macros

* The conditional threading macros `cond->` and `cond->>` use one additional 
  level of parentheses, so that each clause can contain multiple forms to 
  thread/execute.

* `-<>` and `-<>>` do not support literals to insert the `<>` placeholder. The
  placeholder really only works at the outermost level of the threaded forms.
  The reason for this is mostly that Common Lisp does not have so many literal
  syntax elements (by default) where it would make sense to do this kind of
  insertion. If you do need anything fancy, use `as->` or `as->*` for a real
  lexical binding.
  
## Notable differences to arrow-macros

* The conditional threading macros `cond->` and `cond->>` use one additional 
  level of parentheses, so that each clause can contain multiple forms to 
  thread/execute.

* The diamond threading macros provided by `arrow-macros` use a code walker to
  find out whether a placeholder is present in the next threaded form. The
  placeholder therefore works not only at the outermost level of the threaded
  forms, but it is replaced anywhere it is used as a lexical variable.
  * This adds the heavy-weight dependency on `hu.dwim.walker`; if such a
    dependency is not viable, the recommendation is to use named threading
    macros `as->` and `as->*`, possibly nested (see [tutorial](TUTORIAL.md)).

* `arrow-macros` do not provide the conditional diamond threading macros 
  `cond-<>` and `cond-<>>`.
