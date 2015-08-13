## How to build

### Using [stack](https://github.com/commercialhaskell/stack)

```shell
stack setup
stack build
```

To run project use: `stack exec noisy-text` or run ghci with `stack ghci`.

Note: if you want to use LTS build, run `export STACK_YAML=stack-lts.yaml` first or add `--stack-yaml=stack-lts.yaml` option.

### Using cabal

In order to build the project you need [Haskell Platform](https://www.haskell.org/platform/).

Then run the following commands form the project root:

```shell
cabal update
cabal sandbox init
cabal install --dependecies-only
cabal build
```

You can run executable with `cabal run` or run REPL (Read-Eval-Print-Loop) with `cabal repl`.
