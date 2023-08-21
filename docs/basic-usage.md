# FMCS User Manual > Basic Usage

## Installation

FMCS uses ASDF3. To install FMCS, clone the repository into an ASDF local
projects directory, such as `~/common-lisp/` or `~/quicklisp/local-projects/`.

Then FMCS may be loaded directly by ASDF with `(asdf:load-system :fmcs)` or
using a package manager such as Quicklisp with `(ql:quickload :fmcs)`,
and used as a dependency for other ASDF3 systems.

## Planned Feature: FMCS Named Readtable

In all your source-files where you use FMCS, switch to the `:FMCS` named readtable:

```lisp
(in-package :<my-project>)

(named-readtables:in-readtable :fmcs)
```

## Defining Flavor Classes

```lisp
(def$flavor ...)
```

## Defining Flavor Metaclasses

```lisp
(def$flavor ...)
```

## Defining Flavor Mixins

```lisp
(def$flavor ...)
```

## Defining Demon Methods

```lisp
(def$method (<flavor> :<method-name>) ...)
```

## Defining Frames

```lisp
(def$frame ...)
```

## Defining Behaviors

```lisp
(def$behavior ...)
```

## Traceing Demon Methods

```lisp
(trace$method ...)
```

## Untraceing Demon Methods

```lisp
(untrace$method ...)
```

## Defining Whoppers

```lisp
(defwhopper ...)
```

## SBCL Users

> :warning: Note:<br>
> This will be deprecated in favor of an `:FMCS` named readtable in a future release.

For SBCL, FMCS relies on `FARE-QUASIQUOTE` to macroexpand backquote syntax in keeping with idiomatic conventions followed by all other Common Lisp implementations.

When using FMCS for your own projects, you will need to conditionally depend on `FARE-QUASIQUOTE-EXTRAS` for SBCL in your ASDF systems, and use its named-readtable in every source-file where FMCS forms are used to ensure correct macroexpansion at every level.

For example, in your ASDF system definition:

```lisp
(defsystem my-project
  ...
  :depends-on ((:feature :sbcl fare-quasiquote-extras)
               fmcs)
  :components ((:file "package")
               (:file "my-project")))
```

And in your source-files:

```lisp
(in-package :my-project)

#+sbcl
(named-readtables:in-readtable :fare-quasiquote)

...

#+sbcl
(named-readtables:in-readtable :standard)

;; eof
```

Alternatively, as the documentation for `FARE-QUASIQUOTE` suggests, you can use ASDF's `:around-compile` hook to automatically wrap all source-files in the appropriate `NAMED-READTABLES:IN-READTABLE` forms.
