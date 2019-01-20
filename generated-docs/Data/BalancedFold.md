## Module Data.BalancedFold

#### `balancedFold`

``` purescript
balancedFold :: forall t a. Consable t => Unconsable t => Monoid a => Monoid (t a) => t a -> a
```

A balanced fold, also called tree fold.

Folds a list of values using `append`, trying as much as possible to balance.
`Consable` and `Unconsable` indicate that `t` is a list-like structure.

#### `balancedFoldAla`

``` purescript
balancedFoldAla :: forall t a b. Newtype b a => Functor t => Unconsable t => Consable t => Monoid (t b) => Monoid b => (a -> b) -> t a -> a
```

#### `balancedFold'`

``` purescript
balancedFold' :: forall t a. Consable t => Unconsable t => Semigroup a => Monoid (t a) => t a -> Maybe a
```

A version of `balancedFold` which does not require `Monoid` constraint
for its output type.

#### `balancedFoldAla'`

``` purescript
balancedFoldAla' :: forall t a b. Newtype b a => Functor t => Unconsable t => Consable t => Monoid (t b) => Semigroup b => (a -> b) -> t a -> Maybe a
```

#### `balancedStep`

``` purescript
balancedStep :: forall t a. Consable t => Unconsable t => Monoid (t a) => Semigroup a => t a -> t a
```

Append adjacent elements.

E.g.:

```
balancedStep [a, b, c, d, e] ≡ [a, b <> c, d <> e]
```

#### `balancedFold_`

``` purescript
balancedFold_ :: forall t a. Foldable t => (a -> t a -> t a) -> (t a -> Maybe { head :: a, tail :: t a }) -> t a -> (t a -> t a -> t a) -> (a -> a -> a) -> t a -> Maybe a
```

A version with less constraints.

#### `balancedStep_`

``` purescript
balancedStep_ :: forall t a. Foldable t => (a -> t a -> t a) -> t a -> (a -> a -> a) -> t a -> t a
```

A version with less constraints.


