module Main (main) where

import Data.Random.Choose.Internal.Prelude
import qualified Data.Random.Choose as Choose
import Data.Random.Choose (Tree(..), Forest(..))

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, Gen, Property, (.&&.),
    arbitrary, conjoin, label, listOf, verboseCheckAll)

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as Classes
import Test.QuickCheck.Checkers (EqProp)

import Prelude (putStrLn)

main :: IO ()
main = defaultMain tests

tests =
    [ testGroup "Tree"
        [ testProperty "monoid laws" $
              checkersBatchProp $ Classes.monoid
              (undefined :: Tree Int8 Int8)
        , testProperty "functor laws" $
              checkersBatchProp $ Classes.functor
              (undefined :: Tree Int8 (Int8, Int16, Int32))
        ]
    , testGroup "Forest"
        [ testProperty "monoid laws" $
              checkersBatchProp $ Classes.monoid
              (undefined :: Forest Int8 Int8)
        , testProperty "functor laws" $
              checkersBatchProp $ Classes.functor
              (undefined :: Forest Int8 (Int8, Int16, Int32))
        ]
    ]

-------------------------------------------------------------------
--  Tree generation
-------------------------------------------------------------------

instance (Eq k, Eq a) => EqProp (Tree k a)
  where
    (=-=) = Checkers.eq

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Tree k a)
  where
    arbitrary = genTree

genTree :: (Ord k, Arbitrary k, Arbitrary a) => Gen (Tree k a)
genTree = mconcat <$> listOf genSingletonTree

genSingletonTree :: (Arbitrary k, Arbitrary a) => Gen (Tree k a)
genSingletonTree = Choose.singletonTree <$> arbitrary <*> arbitrary


-------------------------------------------------------------------
--  Forest generation
-------------------------------------------------------------------

instance (Eq k, Eq a) => EqProp (Forest k a)
  where
    (=-=) = Checkers.eq

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Forest k a)
  where
    arbitrary = genForest

genForest :: (Ord k, Arbitrary k, Arbitrary a) => Gen (Forest k a)
genForest = mconcat <$> listOf genSingletonForest

genSingletonForest :: (Arbitrary k, Arbitrary a) => Gen (Forest k a)
genSingletonForest = Choose.singletonForest <$> genNonEmpty <*> arbitrary


-------------------------------------------------------------------
--  Generic testing utilities
-------------------------------------------------------------------

genNonEmpty :: Arbitrary a => Gen (NonEmpty a)
genNonEmpty = (:|) <$> arbitrary <*> arbitrary

checkersTestProp :: Checkers.Test -> Property
checkersTestProp (name, prop) = label name prop

checkersBatchProp :: Checkers.TestBatch -> Property
checkersBatchProp (name, tests) =
    label name $ conjoin $ checkersTestProp <$> tests
