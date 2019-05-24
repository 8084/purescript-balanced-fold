# purescript-balanced-fold [![Dependencies](https://img.shields.io/librariesio/github/klntsky/purescript-balanced-fold.svg)](https://libraries.io/github/klntsky/purescript-balanced-fold) [![Build status](https://travis-ci.org/klntsky/purescript-balanced-fold.svg?branch=master)](https://travis-ci.org/klntsky/purescript-balanced-fold)


A balanced fold, also called tree fold, is an operation of folding a list of values using some associative operation, trying as much as possible to balance.

Can be useful for constructing balanced binary trees, or more stable summation.

# Example

Given a binary tree,

```purescript
data T a = Empty | L a | B (T a) (T a)

infixl 6 B as :*:
```

which is an instance of `Semigroup` with `append = B` and `Monoid` with `mempty = Empty`,

```purescript
balancedFold (map L (1..5)) ≡ ((L 1) :*: (((L 2) :*: (L 3)) :*: ((L 4) :*: (L 5))))
```

Check out [the tests](test/TestMain.purs) for more examples.

# See also:

1. [treefold](http://hackage.haskell.org/package/treefold) on Hackage.

# Module documentation

Browse [generated-docs](generated-docs/Data/BalancedFold.md).

PM me if you want this library to be on Pursuit.
