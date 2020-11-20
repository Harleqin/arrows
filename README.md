# Arrows

An implementation of Clojure-inspired threading macros.

## Overview

This is an ASDF system providing the package `arrows`. Its home is at
https://gitlab.com/Harleqin/arrows, with a mirror at
https://github.com/Harleqin/arrows.

This system contains:

* threading macros `->` and `->>`,
* diamond threading macros `-<>` and `-<>>`,
* short-circuiting threading macros `some->` and `some->>`,
* short-circuiting diamond threading macros `some-<>` and `some-<>>`,
* conditional threading macros `cond->` and `cond->>`,
* conditional diamond threading macros `cond-<>` and `cond-<>>`,
* inverted threading macro `->*`,
* named threading macro `as->`,
* inverted named threading macro `as->*`.

## Manual pages

* [Tutorial](doc/TUTORIAL.md)
* [Documentation](doc/DOCUMENTATION.md)
* [Examples](doc/EXAMPLES.md)
* [Comparison with similar libraries](doc/COMPARISON.md)

## TODO

* Finish the tutorial
* Add more examples
