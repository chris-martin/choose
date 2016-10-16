{- |

Defines the 'Tree' data structure and operations on it to implement the random
selection algorithm described in "Data.Random.Choose".

-}

module Data.Random.Choose.Tree
    ( Tree(..), empty, insert, applyLimit, evict, disambiguate ) where

--------------------------------------------------------------------------------

import Control.Monad.Random (Rand, RandomGen, getRandom)

--------------------------------------------------------------------------------

data Tree a = Nil | Tree
    { treeSize   :: Int    -- ^ Total number of items at this node and below
    , treeValues :: [a]    -- ^ Items at this node
    , treeLeft   :: Tree a -- ^ Left subtree (less likely for inclusion)
    , treeRight  :: Tree a -- ^ Right subtree (more likely for inclusion)
    }
-- ^ A binary tree with arbitrarily many values at each node.

instance Foldable Tree where

    foldr _ z Nil = z
    foldr f z (Tree size (x:xs) left right) =
        foldr f (f x z) (Tree (size - 1) xs left right)
    foldr f z (Tree size [] left right) =
        (\z -> foldr f z left) . (\z -> foldr f z right) $ z

    length Nil      = 0
    length t@Tree{} = treeSize t

    null Nil      = True
    null t@Tree{} = treeSize t == 0

empty :: Tree a
-- ^ A tree with no elements.

insert :: a -> Tree a -> Tree a
-- ^ Trivial insertion into the root of a tree, increasing its size by 1
--   and leaving its children unmodified.

applyLimit :: (RandomGen g)
    => Int -- ^ @limit@
    -> Tree a
    -> Rand g (Tree a)
-- ^ Remove items from the tree until its size is at most @limit@.
--   This may involve disambiguation if eviction takes place.

evict :: (RandomGen g) => Tree a -> Rand g (Tree a)
-- ^ Remove one item from the tree (or leave the tree unmodified if it is
--   already empty). This may involve disambiguation if there is not already
--   a clear leftmost item.

disambiguate :: (RandomGen g) => Tree a -> Rand g (Tree a)
-- ^ Perform disambiguation at the root level only, pushing items from
--   the root down into subtrees as necessary.

--------------------------------------------------------------------------------

empty = Nil

insert x Nil = Tree 1 [x] Nil Nil

insert x (Tree size xs left right) = Tree (size + 1) (x:xs) left right

applyLimit limit _ | limit <= 0 = pure Nil

applyLimit limit tree

    -- If the tree is small enough: We don't need to do anything.
    | length tree <= limit = pure tree

    -- If the tree is oversized: Remove an item from it, and recurse.
    | otherwise = applyLimit limit =<< evict tree

evict tree | length tree <= 1 = pure Nil

evict tree = do
    (Tree _ _ left right) <- disambiguate tree

    -- Evict from one of the subtrees, preferring to evict from the left.
    (left', right') <- if not . null $ left
                       then (\x -> (x, right)) <$> evict left
                       else (\x -> (left, x))  <$> evict right

    return $ Tree (length left' + length right') [] left' right'

-- For a tree with no items at the root, no disambiguation is possible
-- (remember that disambiguate operates at the root only).
disambiguate tree@(Tree _ [] _ _) = pure tree

-- For a tree which contains a single item and no children, no
-- disambiguation is required.
disambiguate tree@(Tree _ [_] Nil Nil) = pure tree

-- There is at least one item at the root that needs to be pushed down, to
-- disambiguate it (either from items in subtrees, or from other items at
-- the root).
disambiguate (Tree size (x:xs) left right) = do

    -- Randomly decide whether to push x into the left or right subtree.
    b <- getRandom
    let (left', right') = if b
                          then (insert x left, right)
                          else (left, insert x right)

    -- In tree', a single item from the original tree has been pushed down.
    let tree' = Tree size xs left' right'

    -- There still may be other items at the root, so recurse.
    disambiguate tree'
