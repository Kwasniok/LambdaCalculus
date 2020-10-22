# Lambda Calculus

A small packages featuring the basic operations of lambda calculus.

## Feature List
- Representation of objects in Lambda Calculus as Data Structures
- IO of lambda expressions
- Validity check of lambda expressions
- Stepwise evaluation of lambda expressions

## Usage
Start `ghci` from the shell and `:load ` a module.

```
$ ghci
```

### Modules
- `Data.hs` contains all data structures representing the lambda calculus and output functions (`show`)
- `Parse.hs` contains a parser function for all data structures
- `Evaluation.hs` contains functions related to expression evaluation

### Notation
The standart notation ~`λx. λy. x y`~ is represented as `\x. (\y. (x y))` with explicit **parentheses around each function body** if more than one term is present and exactly **one parameter per function**.

## Examples
### Input/Output
Use `parse` to parse a grammatical data structure like `word` (top level structure):
```
Prelude> :load Parse.hs
*Parse> parse word "\\a. \\b. (a b)"
Just (\a. (\b. (a b)))
*Parse>
```

Printing is implicit for each line but one can use `show` to turn a grammatical data structure into a string as a in:
```
Prelude> :load Parse.hs
*Parse> w = parse word "\\a. \\b. (a b)"
*Parse> show w
"Just (\a. (\b. (a b)))"
*Parse>
```

### Check Validity
To check the validity of a lambda expression use `valid` as in:
```
Prelude> :load Evaluate.hs Parse.hs
*Evaluate> import Evaluate
*Evaluate Evaluate> import Parse
*Evaluate Evaluate Parse> import Data.Maybe
*Evaluate Evaluate Parse Data.Maybe> w = w = fromJust (parse word "(\\x. \\y. (x y) \\y. (y z))")
*Evaluate Evaluate Parse Data.Maybe> valid w
True
*Evaluate Evaluate Parse Data.Maybe> w = w = fromJust (parse word "(\\x. \\x. (x y) \\y. (y z))")
*Evaluate Evaluate Parse Data.Maybe> valid w
False
*Evaluate Evaluate Parse Data.Maybe>
```

### Evaluation (Stepwise)
To perform a single evaluation step (for any **valid** lambda expression) use `eval` as in:
```
Prelude> :load Evaluate.hs Parse.hs
*Evaluate> import Evaluate 
*Evaluate Evaluate> import Parse
*Evaluate Evaluate Parse> import Data.Maybe
*Evaluate Evaluate Parse Data.Maybe> w = fromJust $ parse word "(\\x. \\y. (x y) a b)"
*Evaluate Evaluate Parse Data.Maybe> iterate eval w !! 0
((\x. (\y. (x y))) a b)
*Evaluate Evaluate Parse Data.Maybe> iterate eval w !! 1
((\y. (a y)) b)
*Evaluate Evaluate Parse Data.Maybe> iterate eval w !! 2
(a b)
*Evaluate Evaluate Parse Data.Maybe> iterate eval w !! 3
(a b)
*Evaluate Evaluate Parse Data.Maybe>
```

## Dependencies
- [ghc](https://www.haskell.org/ghc/) v4.9.0.0 or newer
    - archlinux: [ghc package](https://www.archlinux.org/packages/community/x86_64/ghc/) (may require further packages)
- haskell packages:
    - [`base-4.9.0.0`](https://hackage.haskell.org/package/base-4.9.0.0)
