# Flavors Meta-Class System (FMCS): User Manual

> :warning: **This is a work in progress.** The documentation is being restored
> and updated from the original sources. Please check back later for updates.

This manual describes the Flavors Meta-Class System (FMCS) for
_Demonic Metaprogramming_ in Common Lisp, an alternative to CLOS+MOP. It has
been restored from the [CMU AI Repository][CMUAIREPO] alongside Jürgen
Walther's [BABYLON][BABYLON] AI Workbench system, from which the sources of
FMCS were extracted as a standalone library.

- [Overview](./overview.md)
- [Basic Usage](./basic-usage.md)
    - [Installation](./basic-usage.md#installation)
    - [Defining Flavor Classes](./basic-usage.md#defining-flavor-classes)
    - [Defining Flavor Metaclasses](./basic-usage.md#defining-flavor-metaclasses)
    - [Defining Flavor Mixins](./basic-usage.md#defining-flavor-mixins)
    - [Defining Demon Methods](./basic-usage.md#defining-demon-methods)
    - [Defining Behaviours](./basic-usage.md#defining-behaviours)
    - [Defining Traces](./basic-usage.md#defining-traces)
    - [Defining Whoppers](./basic-usage.md#defining-whoppers)
    - [Defining Frames](./basic-usage.md#defining-frames)
    - [SBCL Users](./basic-usage.md#sbcl-users)
- [What Is Demonic Metaprogramming?](./demonic-metaprogramming.md)
    - [Modelling software by Control Flow and Data Flow](./demonic-metaprogramming.md#modelling-software-by-control-flow-and-data-flow)
    - [SSA-Form and Phi-Functions as Nondeterminstic Choice](./demonic-metaprogramming.md#ssa-form-and-phi-functions-as-nondeterminstic-choice)
    - [Angelic and Demonic Semantics of Nondeterministic Choice](./demonic-metaprogramming.md#angelic-and-demonic-semantics-of-nondeterministic-choice)
    - [Favoring Demonic Semantics for Metaprogramming](./demonic-metaprogramming.md#favoring-demonic-semantics-for-metaprogramming)
    - [Demonic Nondeterminism over Unified Control/Data Flow Graph](./demonic-metaprogramming.md#demonic-nondeterminism-over-unified-controldata-flow-graph)
    - [Demonic Semantics of Method Combination](./demonic-metaprogramming.md#demonic-semantics-of-method-combination)
    - [Demonic Semantics of Introspection](./demonic-metaprogramming.md#demonic-semantics-of-introspection)
    - [Demonic Semantics of Backquote Syntax in Macroexpansion](./demonic-metaprogramming.md#demonic-semantics-of-backquote-syntax-in-macroexpansion)
- [FMCS vs. CLOS+MOP](./fmcs-vs-clos-mop.md)
    - [FFMCS Flavors vs. CLOS Objects](./fmcs-vs-clos-mop.md#fmcs-flavors-vs-clos-objects)
    - [FMCS Flavor Classes vs. CLOS Classes](./fmcs-vs-clos-mop.md#fmcs-flavor-classes-vs-clos-classes)
    - [FMCS Flavor Metaclasses vs. MOP Metaclasses](./fmcs-vs-clos-mop.md#fmcs-flavor-metaclasses-vs-mop-metaclasses)
    - [FMCS Flavor Mixins vs. CLOS Multiple Inheritance](./fmcs-vs-clos-mop.md#fmcs-flavor-mixins-vs-clos-multiple-inheritance)
    - [FMCS Demon Methods vs. CLOS Generic Functions](./fmcs-vs-clos-mop.md#fmcs-demon-methods-vs-clos-generic-functions)
    - [FMCS Frames](./fmcs-vs-clos-mop.md#fmcs-frames)
    - [FMCS Behaviors](./fmcs-vs-clos-mop.md#fmcs-behaviors)
    - [FMCS Traces](./fmcs-vs-clos-mop.md#fmcs-traces)
    - [FMCS Whoppers](./fmcs-vs-clos-mop.md#fmcs-whoppers)
- [Concepts](./concepts.md)
    - [Flavors](./concepts.md#flavors)
    - [Flavor Classes](./concepts.md#flavor-classes)
    - [Flavor Metaclasses](./concepts.md#flavor-metaclasses)
    - [Flavor Mixins](./concepts.md#flavor-mixins)
    - [Demon Methods](./concepts.md#demon-methods)
    - [Frames](./concepts.md#frames)
    - [Behaviours](./concepts.md#behaviours)
    - [Traces](./concepts.md#traces)
    - [Whoppers](./concepts.md#whoppers)
- [API Reference](./api-reference.md)
    - [Flavor Classes](./api-reference.md#flavor-classes)
    - [Flavor Metaclasses](./api-reference.md#flavor-metaclasses)
    - [Flavor Mixins](./api-reference.md#flavor-mixins)
    - [Demon Methods](./api-reference.md#demon-methods)
    - [Frames](./api-reference.md#frames)
    - [Behaviors](./api-reference.md#behaviors)
    - [Traces](./api-reference.md#traces)
    - [Whoppers](./api-reference.md#whoppers)
    - [Macros](./api-reference.md#macros)
    - [Functions](./api-reference.md#functions)
    - [Special Variables](./api-reference.md#special-variables)

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

Copyright &copy; 1984&ndash;2023, the Authors. Restrored from the CMU AI Repository and released under the MIT License. Please see the [LICENSE](LICENSE) file for details.

> **Restoration Note:** as explicitly noted in the original source-code, FMCS
> was released by Jürgen Walthers under similar terms as the X Windows System,
> X11, and the MIT License is the closest modern, standardized FOSS equivalent.

[BABYLON]: https://github.com/thephoeron/babylon
[BAPHOMET]: https://github.com/thephoeron/baphomet
[CMUAIREPO]: https://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/0.html
[@thephoeron]: https://github.com/thephoeron

