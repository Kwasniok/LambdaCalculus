# Lambda Calculus

A small [cabal](https://www.haskell.org/cabal/) package for haskell featuring the basic operations of lambda calculus.

## Feature List
- Representation of objects in Lambda Calculus as Data Structures
- IO of lambda expressions
- Validity check of lambda expressions
- Stepwise evaluation of lambda expressions

## Installation
Installation of this library via [cabal](https://www.haskell.org/cabal/) package manager:
```
cabal instal --lib
```
This will register the package for [GHC](https://www.haskell.org/ghc/) as well.

## Usage
After a succeccfull installation all modules are known to and importable from within GHC/GHCI.

### Modules
- `Lambda.Data` contains all data structures representing the lambda calculus and output functions (`show`)
- `Lambda.Parse` contains a parser function for all data structures
- `Lambda.Evaluation` contains functions related to expression evaluation

### Notation
The standard notation ~`λx. λy. x y`~ is represented as `\x. (\y. (x y))` with explicit **parentheses around each function body** if more than one term is present and exactly **one parameter per function**.
Other **term lists** have to be placed **within parentheses** as well: ~`(λx. λy. x y) a b`~ becomes `(\x. (\y. (x y)) a b)`.

## Examples
### Input/Output
Use `parse` to parse a grammatical data structure like `word` (top level structure):
```
> import Lambda.Parse
> parse word "\\a. \\b. (a b)"
Just (\a. (\b. (a b)))
>
```

Printing is implicit for each line but one can use `show` to turn a grammatical data structure into a string as a in:
```
> import Lambda.Parse
> w = parse word "\\a. \\b. (a b)"
> show w
"Just (\a. (\b. (a b)))"
>
```

### Check Validity
To check the validity of a lambda expression use `valid` as in:
```
> import Lambda.Parse
> import Lambda.Evaluate
> import Data.Maybe
> w = w = fromJust (parse word "(\\x. \\y. (x y) \\y. (y z))")
> valid w
True
> w = w = fromJust (parse word "(\\x. \\x. (x y) \\y. (y z))")
> valid w
False
> 
```

### Evaluation (Stepwise)
To perform a single evaluation step (for any **valid** lambda expression) use `eval` as in:
```

> import Lambda.Evaluate
> import Lambda.Parse
> import Data.Maybe
> w = fromJust $ parse word "(\\x. \\y. (x y) a b)"
> iterate eval w !! 0
((\x. (\y. (x y))) a b)
> iterate eval w !! 1
((\y. (a y)) b)
> iterate eval w !! 2
(a b)
> iterate eval w !! 3
(a b)
>
```

## Dependencies
- haskell compiler: [ghc](https://www.haskell.org/ghc/) v4.9.0.0 or newer
    - archlinux: [ghc package](https://www.archlinux.org/packages/community/x86_64/ghc/) (may require further packages)
- haskell package manager: [cabal](https://www.haskell.org/cabal/) v2.4 or newer
