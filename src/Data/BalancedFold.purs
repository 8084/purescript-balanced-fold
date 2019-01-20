module Data.BalancedFold where

import Data.Consable (class Consable, cons)
import Data.Foldable (class Foldable, fold, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Unconsable (class Unconsable, uncons)
import Prelude


-- | A balanced fold, also called tree fold.
-- |
-- | Folds a list of values using `append`, trying as much as possible to balance.
-- | `Consable` and `Unconsable` indicate that `t` is a list-like structure.
balancedFold
  :: forall t a
  .  Consable t
  => Unconsable t
  => Monoid a
  => Monoid (t a)
  => t a -> a
balancedFold = balancedFold' >>> fold


balancedFoldAla :: forall t a b.
                   Newtype b a => Functor t =>
                   Unconsable t => Consable t =>
                   Monoid (t b) => Monoid b =>
                   (a -> b) -> t a -> a
balancedFoldAla f = unwrap <<< balancedFold <<< map f


-- | A version of `balancedFold` which does not require `Monoid` constraint
-- | for its output type.
balancedFold'
  :: forall t a
  .  Consable t
  => Unconsable t
  => Semigroup a
  => Monoid (t a)
  => t a -> Maybe a
balancedFold' = balancedFold_ cons uncons mempty append append


balancedFoldAla' :: forall t a b.
                   Newtype b a => Functor t =>
                   Unconsable t => Consable t =>
                   Monoid (t b) => Semigroup b =>
                   (a -> b) -> t a -> Maybe a
balancedFoldAla' f = map unwrap <<< balancedFold' <<< map f


-- | Append adjacent elements.
-- |
-- | E.g.:
-- |
-- | ```
-- | balancedStep [a, b, c, d, e] ≡ [a, b <> c, d <> e]
-- | ```
balancedStep
  :: forall t a
  .  Consable t
  => Unconsable t
  => Monoid (t a)
  => Semigroup a
  => t a -> t a
balancedStep = balancedStep_ cons mempty append


-- | A version with less constraints.
balancedFold_
  :: forall t a
  .  Foldable t
  => (a -> t a -> t a)
  -> (t a -> Maybe { head :: a, tail :: t a })
  -> t a
  -> (t a -> t a -> t a)
  -> (a -> a -> a)
  -> t a -> Maybe a
balancedFold_ cons uncons mempty append appendA t =
  uncons t <#> \_ -> go t
  where
    step = balancedStep_ cons mempty appendA
    go current =
      let next = step current in
      case unSingleton next of
        Just x -> x
        Nothing -> go next

    unSingleton :: t a -> Maybe a
    unSingleton = uncons >>> case _ of
      Just { head, tail } ->
        tail # uncons >>> case _ of
          Just _ -> Nothing
          Nothing -> Just head
      Nothing -> Nothing


-- | A version with less constraints.
balancedStep_
  :: forall t a
  .  Foldable t
  => (a -> t a -> t a)
  -> t a
  -> (a -> a -> a)
  -> t a -> t a
balancedStep_ cons mempty append ta =
  foldr consume (Tuple Nothing mempty) ta #
  \(Tuple m acc) -> maybe acc (flip cons acc) m
  where
    consume a acc =
      case acc of
        Tuple (Just a') acc' ->
          Tuple Nothing (cons (append a a') acc')
        Tuple Nothing acc' ->
          Tuple (Just a) acc'
