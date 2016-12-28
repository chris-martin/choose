module Data.Random.Choose.Internal.Prelude
    ( module X
    , intToString

      -- * Seq
    , emptySeq, dropSeq, seqCons, singletonSeq, seqFromList, sortSeq

      -- * Map
    , emptyMap, singletonMap, mapUnionWith, mapToAscList, mapLookupMin
    , mapInsert, mapDelete, mapInsertF, mapFromList

      -- * Streaming
    , streamScanM, streamLast_, streamToList, streamToList_
    , streamMapped, streamMap, streamZipWith, streamIterate
    , streamStdinLn, streamStdoutLn
    ) where

import Control.Applicative as X (Applicative(..))
import Control.Monad as X (Monad(..), (=<<), forM, forM_, mapM, mapM_
                          , sequence, when)
import Control.Monad.Random as X (MonadRandom, Rand, RandT, Random, RandomGen
                                 , getRandom, evalRandIO)
import Control.Monad.IO.Class as X (MonadIO)
import Control.Monad.Trans.Class as X (MonadTrans, lift)
import Data.Bool as X (Bool(..), (&&), (||), not, otherwise)
import Data.Either as X (Either(..))
import Data.Eq as X (Eq(..))
import Data.Foldable as X (Foldable(..), all)
import Data.Functor as X (Functor(..), (<$>))
import Data.Function as X ((.), ($), (&), flip, id)
import Data.Int as X (Int, Int8, Int16, Int32, Int64)
import Data.List as X (sort, sortBy)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X (Maybe(..), maybe)
import Data.Monoid as X (Monoid(..), Sum(..), (<>))
import Data.Ord as X (Ord(..), comparing)
import Data.Sequence as X (Seq)
import Data.String as X (String)
import Data.Traversable as X (Traversable, traverse)
import Data.Tuple as X (fst, snd)
import GHC.Enum as X (succ)
import GHC.Num as X (Num(..))
import GHC.Show as X (Show)
import Prelude as X (undefined)
import Streaming as X (Stream, Of, chunksOf, hoist)
import System.IO as X (IO)
import System.IO.Error as X (ioError)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified GHC.Show as Show
import qualified Streaming.Prelude as StreamingPrelude

intToString :: Int -> String
intToString = Show.show

emptyMap :: Map k a
emptyMap = Map.empty

emptySeq :: Seq a
emptySeq = Seq.empty

singletonSeq :: a -> Seq a
singletonSeq = Seq.singleton

singletonMap :: k -> a -> Map k a
singletonMap = Map.singleton

mapUnionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnionWith = Map.unionWith

mapToAscList :: Map k a -> [(k, a)]
mapToAscList = Map.toAscList

dropSeq :: Int -> Seq a -> Seq a
dropSeq = Seq.drop

seqCons :: a -> Seq a -> Seq a
seqCons = (Seq.<|)

seqFromList :: [a] -> Seq a
seqFromList = Seq.fromList

streamScanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b)
            -> Stream (Of a) m r -> Stream (Of b) m r
streamScanM = StreamingPrelude.scanM

streamLast_ :: Monad m => Stream (Of a) m r -> m (Maybe a)
streamLast_ = StreamingPrelude.last_

streamToList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
streamToList = StreamingPrelude.toList

streamToList_ :: Monad m => Stream (Of a) m () -> m [a]
streamToList_ = StreamingPrelude.toList_

streamMapped :: (Monad m, Functor f) =>
    (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
streamMapped = StreamingPrelude.mapped

streamMap :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
streamMap = StreamingPrelude.map

streamZipWith :: Monad m => (a -> b -> c)
    -> Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of c) m r
streamZipWith = StreamingPrelude.zipWith

streamIterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r
streamIterate = StreamingPrelude.iterate

sortSeq :: Ord a => Seq a -> Seq a
sortSeq = Seq.sort

streamStdinLn :: MonadIO m => Stream (Of String) m ()
streamStdinLn = StreamingPrelude.stdinLn

streamStdoutLn :: MonadIO m => Stream (Of String) m () -> m ()
streamStdoutLn = StreamingPrelude.stdoutLn

mapLookupMin :: Map k a -> Maybe (k, a)
mapLookupMin = Map.lookupMin

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Map.insert

mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Map.delete

mapInsertF :: (Ord k, Functor f) => k -> f a -> Map k a -> f (Map k a)
mapInsertF k fa map = (\a -> mapInsert k a map) <$> fa

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = Map.fromList
