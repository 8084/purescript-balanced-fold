module Test.Main where

import Prelude
import Data.Array ((..), concat, replicate)
import Data.Maybe (Maybe(..), isNothing)
import Data.BalancedFold (balancedFold, balancedFold', balancedFoldAla, balancedStep)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Data.Foldable
import Data.Monoid.Additive
import Data.Monoid.Multiplicative

-- | A binary tree
data T a = L a | B (T a) (T a)
infixl 6 B as :*:

derive instance eqT :: Eq a => Eq (T a)

instance semigroupT :: Semigroup (T a) where
  append = B

instance showT :: Show a => Show (T a) where
  show (L a) = "(L " <> show a <> ")"
  show (B a b) = "(" <> show a <> " :*: " <> show b <> ")"


main :: Effect Unit
main = runTest do
  suite "balancedStep" do
    test "satisfies assertions" do
      let check input output =
            equal (balancedStep input) output
      check [] []
      check [[1]]
            [[1]]
      check [[1,2], [3,4]]
            [[1,2,3,4]]
      check [[1,2], [3,4], [5,6]]
            [[1,2], [3,4,5,6]]
      check [[1,2], [3,4], [5,6], [7,8]]
            [[1,2,3,4], [5,6,7,8]]
      check [[1,2], [3,4], [5,6], [7,8], [9,10]]
            [[1,2], [3,4,5,6], [7,8,9,10]]

  suite "balancedFold'" do
    test "satisfies assertions" do
      let check arr tree =
            equal (Just tree) (balancedFold' $ map L $ arr)
      assert "balanced fold of empty list is Nothing" $
        isNothing $ balancedFold' $ map L $ [] :: Array Unit
      check [1]    $ L 1
      check (1..2) $ (L 1) :*: (L 2)
      check (1..3) $ (L 1) :*: ((L 2) :*: (L 3))
      check (1..4) $ ((L 1) :*: (L 2)) :*: ((L 3) :*: (L 4))
      check (1..5) $ ((L 1) :*: (((L 2) :*: (L 3)) :*: ((L 4) :*: (L 5))))
      check (1..40) $ B (B (B (B (L 1) (L 2)) (B (L 3) (L 4)))
                           (B (B (L 5) (L 6)) (B (L 7) (L 8))))
                        (B (B (B (B (B (L 9) (L 10)) (B (L 11) (L 12)))
                                 (B (B (L 13) (L 14)) (B (L 15) (L 16))))
                              (B (B (B (L 17) (L 18)) (B (L 19) (L 20)))
                                 (B (B (L 21) (L 22)) (B (L 23) (L 24)))))
                           (B (B (B (B (L 25) (L 26)) (B (L 27) (L 28)))
                                 (B (B (L 29) (L 30)) (B (L 31) (L 32))))
                              (B (B (B (L 33) (L 34)) (B (L 35) (L 36)))
                                 (B (B (L 37) (L 38)) (B (L 39) (L 40))))))

  suite "balancedFold" do
    test "balancedFold is equivalent to concat for Array" do
      for_ [ []
           , [[1,2]]
           , [[1,2], [3,4]]
           , [[1,2], [3,4], [5,6]]
           , [[1,2], [3,4], [5,6], [7,8]]
           , [[1,2], [3,4], [5,6], [7,8], [9,10]]
           , [[1,2], [3,4], [5,6], [7,8], [9,10], [11,12]]
           , [[1,2], [3,4], [5,6], [7,8], [9,10], [11,12], [13,14]]
           ] \input -> do
        equal (balancedFold input) (concat input)

  suite "balancedFoldAla" do
    test "improves summation accuracy" do
      let list = replicate 10 9.9
      equal "99.0" $ show (balancedFoldAla Additive list)
      equal "99.00000000000001" $ show (foldr add 0.0 list)
