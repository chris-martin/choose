{- |

"Data.Random.Choose" provides an efficient mechanism to select /n/ items
uniformly at random from an input stream, for some fixed /n/.

-}

module Data.Random.Choose (

      choose

    -- * Tree
    -- $algorithm
    , Tree(..), emptyTree, singletonTree, flatTree, addToTree
    , treeDrop, treeTakeRight, disambiguateTree

    -- ** Forest
    , Forest(..), emptyForest, singletonForest, forestDrop, addToForest

    -- * Utilities

    -- ** Streaming
    , treeStream, seqStream

    -- ** Indexed
    , Indexed, indexedStream, sortIndexedValues
    ) where

import Data.Random.Choose.Internal.Prelude

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
    { treeSize     :: Sum Int    -- ^ Total number of items at this node
                                 --   and below
    , treeValues   :: Seq a      -- ^ Items at this node
    , treeChildren :: Forest k a -- ^ Subtrees (lesser keys are evicted
                                 --   first, greater keys are more likely
                                 --   to be included in the final result)
    } deriving (Eq, Functor, Show)

instance Ord k => Monoid (Tree k a)
  where
    mempty = emptyTree

    mappend (Tree size  values  children)
            (Tree size' values' children') =
        Tree (size     <> size')
             (values   <> values')
             (children <> children')

instance Foldable (Tree k)
  where
    length = getSum      . treeSize
    null   = (== mempty) . treeSize

    foldr f z (Tree _ values children) =
        let z' = foldr f z  values
        in       foldr f z' children

emptyTree :: Tree k a
emptyTree = Tree 0 emptySeq emptyForest

singletonTree :: [k] -> a -> Tree k a
singletonTree []     a = Tree 1 (singletonSeq a) emptyForest
singletonTree (k:ks) a = Tree 1 mempty (singletonForest (k :| ks) a)

-- | A fully ambiguous tree with all of the values at the root.
flatTree :: Seq a -> Tree k a
flatTree xs = Tree 1 xs emptyForest

addToTree
    :: Seq a    -- ^ Items to add to the tree
    -> Tree k a
    -> Tree k a
addToTree items t = t { treeValues = items <> treeValues t }

-- | Remove /n/ items from a tree.
treeDrop :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int          -- ^ /n/
    ->    Tree k a
    -> m (Tree k a)
treeDrop n t
    | n <= 0        = pure t
    | n >= length t = pure emptyTree
    | otherwise = do
        t'        <- disambiguateTree t
        children' <- forestDrop n $ treeChildren t'
        pure $ Tree (Sum $ length t' - n) mempty children'

treeTakeRight :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int          -- ^ Maximum number of elements to retain
    ->    Tree k a
    -> m (Tree k a)
treeTakeRight n t = treeDrop (length t - n) t

-- | Perform disambiguation at the root level, pushing items from
--   the root down into subtrees as necessary.
disambiguateTree :: forall m k a. (Ord k, Random k, MonadRandom m)
    =>    Tree k a
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
  where
    mempty = emptyForest
    mappend (Forest x) (Forest y) = Forest $ mapUnionWith (<>) x y

instance Foldable (Forest k)
  where
    length    = getSum . foldMap (Sum . length) . forestMap
    null      = all null                        . forestMap
    foldr f z = foldr (\b t -> foldr f t b) z   . forestMap

emptyForest :: Forest k a
emptyForest = Forest emptyMap

singletonForest :: NonEmpty k -> a -> Forest k a
singletonForest (k :| ks) a = Forest $ singletonMap k $ singletonTree ks a

-- | Remove /n/ items from a forest.
forestDrop :: forall m k a. (Ord k, Random k, MonadRandom m)
    => Int -- ^/@n/
    -> Forest k a -> m (Forest k a)
forestDrop n f@(Forest map) = maybe (pure f) go (mapLookupMin map)
  where
    go (k, t)
        | length t <= n = let f' = Forest $ mapDelete k map
                          in  forestDrop (n - length t) f'
        | otherwise = Forest <$> (mapInsertF k (treeDrop n t) map)

-- | Add multiple items to a forest by assigning each one to a
--   randomly-selected subtree.
addToForest :: forall t m k a.
    (Traversable t, Ord k, Random k, MonadRandom m)
    => t a -- ^ Items to add to the forest
    -> Forest k a -> m (Forest k a)
addToForest values forest =
    foldr (<>) forest <$> forM values disambiguation
  where
    disambiguation v = (\k -> singletonForest (pure k) v) <$> getRandom


--------------------------------------------------------------------------------
--  Indexed
--------------------------------------------------------------------------------

data Indexed a = Indexed { index :: Int, indexedValue :: a }

instance Eq  (Indexed a)
  where
    x == y = index x == index y

instance Ord (Indexed a)
  where
    compare x y = compare (index x) (index y)

sortIndexedValues :: forall t a. (Foldable t) => t (Indexed a) -> Seq a
sortIndexedValues = fmap indexedValue . sortSeq . seqFromList . toList


--------------------------------------------------------------------------------
--  Streaming
--------------------------------------------------------------------------------

treeStream :: forall k a m r. (Ord k, Random k, MonadRandom m) =>
       Int                     -- ^ /n/: Maximum number of items to choose
    -> Stream (Of (Seq a)) m r -- ^ /s/: Stream of chunks of items to choose from
    -> m (Tree k a)            -- ^ A stream of 'Tree's containing at most /n/ of
                               --   the items from /s/
treeStream limit =
    fmap (maybe emptyTree id) .
    streamLast_ .
    streamScanM (\t items -> treeTakeRight limit $ addToTree items t)
                (pure emptyTree) pure

-- | Chunk a stream into fixed-length 'Seq's.
seqStream :: forall m a r. (Monad m)
    => Int                     -- ^ /n/: Maximum number of items to include in
                               --   each chunk
    -> Stream (Of      a)  m r -- ^ /s/: A stream of items of type @a@
    -> Stream (Of (Seq a)) m r -- ^ A stream containing the items from /s/
                               --   grouped into contiguous 'Seq's of size at
                               --   most /n/.
seqStream size =
    streamMap seqFromList .
    streamMapped streamToList .
    chunksOf size

-- | Example: A stream of @[a, b, c]@ becomes a stream of
--   @[Indexed 0 a, Indexed 1 b, Indexed 2 c]@.
indexedStream :: forall m a r. (Monad m)
    => Stream (Of          a)  m r -- ^ Stream of @a@
    -> Stream (Of (Indexed a)) m r -- ^ Stream of @Indexed a@
indexedStream = streamZipWith Indexed (streamIterate succ 0)

-- | Select /n/ items uniformly at random from an input stream.
choose :: forall m a r. (MonadRandom m)
    => Int               -- ^ /n/: Maximum number of items to choose
                         --   from the stream
    -> Stream (Of a) m r -- ^ /s/: Stream from which to select items
    -> m (Seq a)         -- ^ At most /n/ items selected from /s/ uniformly
                         --   at random, in the order they appeared in /s/
choose n s = do
    tree <- treeStream n $ seqStream 1024 $ indexedStream s
    pure $ sortIndexedValues (tree :: Tree Int8 (Indexed a))
