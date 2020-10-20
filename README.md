# Lambda Calculus

A small packages featuring the basic operations of lambda calculus.

## Feature List
- Representation of objects in Lambda Calculus as Data Structures
- IO of lambda expressions
- Evaluation of lambda expressions

## Usage
Start `ghci` from the shell and `:load ` a module.

```
$ ghci
```

### Modules
- `Data.hs` contains all data structures representing the lambda calculus and output functions (`show`)
- `Parse.hs` contains parser function for all data structures

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

## Dependencies
- [ghc](https://www.haskell.org/ghc/) v4.9.0.0 or newer
    - archlinux: [ghc package](https://www.archlinux.org/packages/community/x86_64/ghc/) (may require further packages)
- haskell packages:
    - [`base-4.9.0.0`](https://hackage.haskell.org/package/base-4.9.0.0)
