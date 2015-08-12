## How to build

In order to build the project you need [Haskell Platform](https://www.haskell.org/platform/).

Then run the following commands form the project root:

```shell
cabal update
cabal sandbox init
cabal install --dependecies-only
cabal build
```

You can run executable with `cabal run` or run REPL (Read-Eval-Print-Loop) with `cabal repl`.
