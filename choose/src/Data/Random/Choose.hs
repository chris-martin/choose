-- | "Data.Random.Choose" provides an efficient mechanism to select /n/ items
-- uniformly at random from an input stream, for some fixed /n/.

module Data.Random.Choose (

      choose

    -- * Tree
    -- $algorithm
    , Tree(..), emptyTree, singletonTree, flatTree, addToTree
    , treeConcat, treeDrop, treeTakeRight, disambiguateTree
    , treeLength, treeNull, treeFoldr

    -- ** Forest
    , Forest(..), emptyForest, singletonForest, forestDrop, addToForest
    , forestLength, forestNull, forestConcat, forestFoldr

    -- * Utilities

    -- ** Streaming
    , treeStream, seqStream

    -- ** Indexed
    , Indexed(..), indexedEq, indexedCompare, indexedStream, sortIndexedValues
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..), mapM)
import Control.Monad.Random (MonadRandom, Random, getRandom)
import Data.Bool (Bool(..), (||), otherwise)
import Data.Eq (Eq(..))
import Data.Foldable (Foldable(..), all)
import Data.Function ((.), ($), id)
import Data.Functor (Functor(..), (<$>))
import Data.Int (Int, Int8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (Monoid(..), Sum(..), (<>))
import Data.Ord (Ord(..), Ordering)
import Data.Sequence (Seq)
import Data.Traversable (Traversable)
import GHC.Enum (Bounded(..), Enum(..))
import GHC.Num (Num(..))
import GHC.Show (Show)
import Streaming (Stream, Of, chunksOf)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Streaming.Prelude as Stream

{- $algorithm

We store items on a 'Tree', moving them down into randomly-selected branches. At
the end, the rightmost /n/ items on the tree are selected. After we add items to
the tree ('addToTree'), we take prune its leftmost items ('treeTakeRight')

It may be helpful to think of this a @Tree k a@ as a bag of items of type @a@
with "score" of type @[k]@ lazily assigned to each item. The length of the score
is /d/, where /d/ is the maximum depth of the tree. Moving an item downwards
through the tree corresponds to appending a @k@ to its score. Note that the
laziness is necessary because /d/ is not known a priori. The process of moving
items futher down the tree is referred to as disambiguation ('disambiguateTree')
because its purpose is to resolve "ties" in the score.
-}


--------------------------------------------------------------------------------
--  Tree
--------------------------------------------------------------------------------

data Tree k a = Tree
    { treeSize     :: Sum Int    -- ^ Total number of items at this node and
                                 -- below
    , treeValues   :: Seq a      -- ^ Items at this node
    , treeChildren :: Forest k a -- ^ Subtrees (lesser keys are evicted first,
                                 -- greater keys are more likely to be included
                                 -- in the final result)
    } deriving (Eq, Functor, Show)

instance Ord k => Monoid (Tree k a) where mempty = emptyTree
                                          mappend = treeConcat

instance Foldable (Tree k) where length = treeLength
                                 null = treeNull
                                 foldr = treeFoldr

treeLength :: Tree k a -> Int
treeLength = getSum . treeSize

treeNull :: Tree k a -> Bool
treeNull = (== mempty) . treeSize

treeFoldr :: (a -> b -> b) -> b -> Tree k a -> b
treeFoldr f z (Tree _ values children) = let z' = foldr f z  values
                                         in       foldr f z' children

-- | A tree containing with no items.
emptyTree :: Tree k a
emptyTree = Tree 0 Seq.empty emptyForest

-- | A tree containing a single item.
singletonTree
    :: [k]      -- ^ /ks/: The item's "score" (its position in the tree)
    -> a        -- ^ /x/: The singular item in the tree
    -> Tree k a -- ^ A tree containing a single item /x/ at position /ks/
singletonTree []     a = Tree 1 (Seq.singleton a) emptyForest
singletonTree (k:ks) a = Tree 1 mempty (singletonForest (k :| ks) a)

-- | A fully ambiguous tree with all of the values at the root (none of the
-- items have any "score" at all).
flatTree :: Seq a    -- ^ Items to be made into a tree
         -> Tree k a -- ^ A tree with all of the items at its root
flatTree xs = Tree 1 xs emptyForest

-- | Combine two trees. The result contains each item from both trees at the
-- same position it was located in its original tree. This is 'mappend'.
treeConcat :: Ord k => Tree k a -> Tree k a -> Tree k a
treeConcat (Tree size  values  children)
           (Tree size' values' children') = Tree (size     <> size')
                                                 (values   <> values')
                                                 (children <> children')

-- | Add some items to the root of a tree.
addToTree
    :: Seq a    -- ^ /xs/: Items to add to the tree
    -> Tree k a -- ^ /t/: Tree we're adding things to
    -> Tree k a -- ^ A tree containing all of the items from /t/ in their same
                -- positions, plus each of the /xs/ at the root of the tree.
addToTree items t = t { treeValues = items <> treeValues t }

-- | Remove the /n/ leftmost items from a tree.
treeDrop :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int          -- ^ /n/: Maximum number of items to remove from the tree
    ->    Tree k a  -- ^ /t/: Tree we're removing things from
    -> m (Tree k a) -- ^ /t/ with the leftmost /n/ items removed from it, or an
                    -- empty tree if /t/ contains fewer than /n/ items
treeDrop n t | n <= 0        = pure t
             | n >= length t = pure emptyTree
             | otherwise = do t'        <- disambiguateTree t
                              children' <- forestDrop n $ treeChildren t'
                              pure $ Tree (Sum $ length t' - n) mempty children'

-- | Retain the /n/ rightmost items in a tree.
treeTakeRight :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int          -- ^ /n/: Maximum number of items to retain in the tree
    ->    Tree k a  -- ^ /t/: Tree we're removing things from
    -> m (Tree k a) -- ^ The subset of /t/ containing its rightmost /n/ items,
                    -- or /t/ if /t/ contains fewer than /n/ items.
treeTakeRight n t = treeDrop (length t - n) t

-- | Perform disambiguation at the root level, pushing items from the root down
-- into subtrees as necessary.
disambiguateTree :: forall m k a. (Ord k, Random k, MonadRandom m)
    =>    Tree k a  -- ^ /t/: A tree that might be in need of disambiguation
    -> m (Tree k a)
disambiguateTree t@(Tree size values children)
    | null (treeValues t) || length t == 1 = pure t
    | otherwise = Tree size mempty <$> addToForest values children


--------------------------------------------------------------------------------
--  Forest
--------------------------------------------------------------------------------

newtype Forest k a = Forest { forestMap :: Map k (Tree k a) }
    deriving (Eq, Functor, Show)

instance Ord k => Monoid (Forest k a)
  where mempty = emptyForest
        mappend = forestConcat

instance Foldable (Forest k)
  where length = forestLength
        null = forestNull
        foldr = forestFoldr

forestLength :: Forest k a -> Int
forestLength = getSum . foldMap (Sum . length) . forestMap

forestNull :: Forest k a -> Bool
forestNull = all null . forestMap

forestFoldr :: (a -> b -> b) -> b -> Forest k a -> b
forestFoldr f z = foldr (\b t -> foldr f t b) z . forestMap

-- | A forest containing no trees.
emptyForest :: Forest k a
emptyForest = Forest Map.empty

-- | A forest containing a single item.
singletonForest
    :: NonEmpty k -- ^ /ks/: The item's "score" (its position in the forest)
    -> a          -- ^ /x/: The singular item in the forest
    -> Forest k a -- ^ A forest containing a single item /x/ at position /ks/
singletonForest (k :| ks) a = Forest $ Map.singleton k $ singletonTree ks a

-- | Remove /n/ items from a forest.
forestDrop :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int            -- ^ /n/: Maximum number of items to remove from
                      --   the forest
    ->    Forest k a  -- ^ /f/: Forest we're removing things from
    -> m (Forest k a)
forestDrop n f@(Forest map) = maybe (pure f) go (Map.lookupMin map)
  where
    -- k: The leftmost key in the forest
    -- t: The leftmost tree in the forest
    go (k, t)

        -- If the tree contains more than n items, drop n items from it.
        | length t > n = Forest <$> insertF k (treeDrop n t) map

        -- If the tree contains n items or fewer, remove the tree entirely and
        -- recurse on the rest of the forest.
        | otherwise = let f' = Forest $ Map.delete k map
                      in  forestDrop (n - length t) f'

-- | Insert into a 'Map', in the context of some 'Functor'. See 'Map.alterF'.
insertF :: forall k a f. (Ord k, Functor f) => k -> f a -> Map k a -> f (Map k a)
insertF k fa = Map.alterF alter k where alter _ = Just <$> fa

-- | Add multiple items to a forest by assigning each one to a randomly-selected
-- subtree.
addToForest :: forall t m k a.
    (Traversable t, Ord k, Random k, MonadRandom m)
    => t a            -- ^ Items to add to the forest
    ->    Forest k a  -- ^ /f/
    -> m (Forest k a)
addToForest values forest =
    foldr (<>) forest <$> mapM disambiguation values
  where
    disambiguation v = (\k -> singletonForest (pure k) v) <$> getRandom

-- | Combine two forests. The result contains each item from both forests at the
-- same position it was located in its original forest. This is 'mappend'.
forestConcat :: Ord k => Forest k a -> Forest k a -> Forest k a
forestConcat (Forest x) (Forest y) = Forest $ Map.unionWith (<>) x y


--------------------------------------------------------------------------------
--  Indexed
--------------------------------------------------------------------------------

-- | A value (type @a@) paired with some index (type @i@) that represents its
-- position in some sequence. The 'Eq' and 'Ord' instances are based solely on
-- the index, ignoring the value.
--
-- We used 'Indexed' to keep track of each value's position in the stream so
-- that after the final values are chosen we can sort them back into their
-- original order.
data Indexed i a = Indexed
    { index        :: !i -- ^ Determines how 'Indexed' values are compared
    , indexedValue :: !a -- ^ An interesting value that has been tagged with an
                         -- index
    }

instance Eq i => Eq (Indexed i a) where (==) = indexedEq

instance Ord i => Ord (Indexed i a) where compare = indexedCompare

indexedEq :: Eq i => Indexed i a -> Indexed i a -> Bool
indexedEq x y = index x == index y

indexedCompare :: Ord i => Indexed i a -> Indexed i a -> Ordering
indexedCompare x y = compare (index x) (index y)

-- | Sort a collection of indexed values according to index, then strip out the
-- indexes to just get the values.
sortIndexedValues :: forall t i a. (Foldable t, Ord i)
    => t (Indexed i a) -- ^ Collection of indexed values
    -> Seq a           -- ^ The values, sorted by index
sortIndexedValues = fmap indexedValue . Seq.sort . Seq.fromList . toList


--------------------------------------------------------------------------------
--  Streaming
--------------------------------------------------------------------------------

treeStream :: forall k a m r. (Ord k, Random k, MonadRandom m) =>
       Int                     -- ^ /n/: Maximum number of items to choose
    -> Stream (Of (Seq a)) m r -- ^ /s/: Stream of chunks of items to choose from
    -> m (Tree k a)            -- ^ A stream of 'Tree's containing at most /n/
                               -- of the items from /s/
treeStream limit =
    fmap (maybe emptyTree id) .
    Stream.last_ .
    Stream.scanM (\t items -> treeTakeRight limit $ addToTree items t)
                 (pure emptyTree) pure

-- | Chunk a stream into fixed-length 'Seq's.
seqStream :: forall m a r. (Monad m)
    => Int                     -- ^ /n/: Maximum number of items to include in
                               -- each chunk
    -> Stream (Of      a)  m r -- ^ /s/: A stream of items of type @a@
    -> Stream (Of (Seq a)) m r -- ^ A stream containing the items from /s/
                               -- grouped into contiguous 'Seq's of size at most
                               -- /n/.
seqStream size = Stream.map Seq.fromList
               . Stream.mapped Stream.toList
               . chunksOf size

-- | Example: A stream of @[a, b, c]@ becomes a stream of @[Indexed 0 a, Indexed
-- 1 b, Indexed 2 c]@.
indexedStream :: forall m i a r. (Monad m, Bounded i, Enum i)
    => Stream (Of            a)  m r -- ^ Stream of @a@
    -> Stream (Of (Indexed i a)) m r -- ^ Stream of @Indexed a@
indexedStream = Stream.zipWith Indexed (Stream.iterate succ minBound)

-- | Select /n/ items uniformly at random from an input stream.
choose :: forall m a r. (MonadRandom m)
    => Int               -- ^ /n/: Maximum number of items to choose from the
                         -- stream
    -> Stream (Of a) m r -- ^ /s/: Stream from which to select items
    -> m (Seq a)         -- ^ At most /n/ items selected from /s/ uniformly at
                         -- random, in the order they appeared in /s/
choose n s = do
    tree <- treeStream n $ seqStream 1024 $ indexedStream s
    pure $ sortIndexedValues (tree :: Tree Int8 (Indexed Int a))
