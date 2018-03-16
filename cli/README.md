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

Libraries used
--------------

  * `directory` -- for `pwd`;
  * `process` -- for calling subprocesses;
  * `parsec` -- for parsing the input;
  * `mtl` -- for using monad transformers;
  * `regex-tdfa` -- DFA for regular expressions in `grep`.

`grep` parses its command line arguments using the standard library.
It makes sense because the use case is as simple as they get, and
the standard library is installed everywhere.
