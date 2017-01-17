module Main (main) where

-- import qualified Data.Random.Choose as Choose
import Data.Random.Choose (Tree(..), Forest(..),
    emptyTree, flatTree, singletonTree, addToTree, singletonForest)

import Control.Applicative (Applicative(..))
import Data.Bool
import Data.Eq (Eq(..))
import Data.Foldable (length, toList)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (mconcat)
import Data.Ord (Ord(..))
import Data.Sequence (Seq)
import GHC.Num (Num(..))
import Prelude (undefined)
import System.IO (IO)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Test.QuickCheck (Arbitrary, Gen, Property,
    arbitrary, conjoin, label, listOf)

import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as Classes
import Test.QuickCheck.Checkers (EqProp)

main :: IO ()
main = defaultMain tests

type X = Int8

tests :: [Test]
tests =
    [ testGroup "Tree"
        [ testProperty "monoid laws" $ checkersBatchProp $
              Classes.monoid (undefined :: Tree X X)
        , testProperty "functor laws" $ checkersBatchProp $
              Classes.functor (undefined :: Tree X (X, X, X))
        , testCase "empty tree - length" $ length emptyTree @?= 0
        , testCase "empty tree - to list" $ toList emptyTree @?= ([] :: [X])
        , testProperty "flat tree - length" $ \(xs :: Seq X) ->
              length (flatTree xs) == length xs
        , testProperty "flat tree - to list" $ \(xs :: Seq X) ->
              toList (flatTree xs) == toList xs
        , testProperty "singleton tree - length" $ \(k :: [X]) (x :: X) ->
              length (singletonTree k x) == 1
        , testProperty "singleton tree - to list" $ \(k :: [X]) (x :: X) ->
              toList (singletonTree k x) == [x]
        , testProperty "add to tree - length" $ \(xs :: Seq X) (t :: Tree X X) ->
              length (addToTree xs t) == length xs + length t
        ]
    , testGroup "Forest"
        [ testProperty "monoid laws" $ checkersBatchProp $
              Classes.monoid (undefined :: Forest X X)
        , testProperty "functor laws" $ checkersBatchProp $
              Classes.functor (undefined :: Forest X (X, X, X))
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
genSingletonTree = singletonTree <$> arbitrary <*> arbitrary


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
genSingletonForest = singletonForest <$> genNonEmpty <*> arbitrary


-------------------------------------------------------------------
--  Generic testing utilities
-------------------------------------------------------------------

genNonEmpty :: Arbitrary a => Gen (NonEmpty a)
genNonEmpty = (:|) <$> arbitrary <*> arbitrary

checkersTestProp :: Checkers.Test -> Property
checkersTestProp (name, prop) = label name prop

checkersBatchProp :: Checkers.TestBatch -> Property
checkersBatchProp (name, checkersTests) =
    label name $ conjoin $ checkersTestProp <$> checkersTests
