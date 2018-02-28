cli
===

Usage
-----

To build, first obtain `cabal-install`.

Then, run

```
cabal sandbox init
cabal install --only-dependencies
cabal test # should pass the test cases
cabal run  # should start the program
```

Architecture
------------

The architecture is bottom-up.

The major modules are:
  * `Command` -- evaluation of commands that are already fully parsed;
  * `Environment` -- the type definition of the lookup table;
  * `Parse` -- parsing a string into a structure representind a pipeline,
    with no variable substitutions being performed at this point;
  * `Sentence` -- substituting the variables everywhere where applicable
    in the pipeline;
  * `String` -- the representation of a string token for which the variable
    substitutions haven't yet happened, and a function to substitute
    variables;
  * `Main` -- the main loop.
