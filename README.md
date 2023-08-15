# FMCS

This is the Flavors Meta-Class System (FMCS) for _Demonic Metaprogramming_ in Common Lisp, an alternative to CLOS+MOP. It has been restored from the [CMU AI Repository][CMUAIREPO], alongside Jürgen Walther's [BABYLON][] AI Workbench system, from which the sources were extracted.

## TODO

The following tasks are required to complete the restoration of FMCS:

- [x] Extract FMCS from BABYLON
- [x] Clean up original source-code
- [x] Translate German comments to English
- [ ] Restore and update original documentation
- [ ] Generate API Documentation in Markdown format
- [ ] Generate unit tests

While the following tasks are to support extended features:

- [ ] Integrate with [BAPHOMET][]
- [ ] Replace hand-coded STANDARD-CLASS object with Hyperlattice implementation
- [ ] Rewrite low-level definitions as Hyperlattice operations

## Demonic Metaprogramming

TODO: give a brief overview of Demonic Metaprogramming, and how it differs from the MOP, covering the following topics:

- Modelling software by Control Flow and Data Flow
- SSA-Form and Phi-Functions as Nondeterminstic Choice
- Angelic and Demonic Semantics of Nondeterministic Choice
- Favoring Demonic Semantics for Metaprogramming
- Demonic Nondeterminism over Unified Control/Data Flow Graph
- Demon Methods versus Generic Functions
- The Flavors System versus CLOS
- The Flavors Meta-Class System (FMCS) versus the MOP
- Demonic Semantics of Backquote Syntax in Macroexpansion

## Basic Usage

- Defining Flavor Classes
- Defining Flavor Metaclasses
- Defining Mixins
- Defining Demon Methods

## SBCL Users

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

## Authors

- ["the Phoeron" Colin J.E. Lupton][@thephoeron]
- Jürgen Walther

Including contributions by, and code based on the work of:

- Pierre Cointe
- Thomas Christaller
- Harry Bretthauer
- Eckehard Gross
- Jürgen Kopp

## License

Copyright &copy; 1987&ndash;2023, the Authors. Restrored from the CMU AI Repository and released under the MIT License. Please see the [LICENSE](LICENSE) file for details.

> **Restoration Note:** as explicitly noted in the original source-code, FMCS
> was released by Jürgen Walthers under similar terms as the X Windows System,
> X11, and the MIT License is the closest modern, standardized FOSS equivalent.

[BABYLON]: https://github.com/thephoeron/babylon
[BAPHOMET]: https://github.com/thephoeron/baphomet
[CMUAIREPO]: https://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/0.html
[@thephoeron]: https://github.com/thephoeron
