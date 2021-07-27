## Propositional calculus

The current repository is a work-in-progress playground for
experimentation with intuitionistic propositional calculus.
*Caveat emptor!*

Installation:
-------------

To compile from sources, you can use Haskell's `stack`.
The project depends on Paul Tarau's intuitionistic provers:

* https://github.com/ptarau/TypesAndProofs

In order to make use of it through the current project,
make sure that:

* The `scripts/int-solver` script in available in your `PATH`.
* Make sure that the `script/int-solver` points to the directory with the `TypesAndProofs` project.
