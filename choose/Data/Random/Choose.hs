{- |

"Data.Random.Choose" provides an efficient mechanism to select /n/ items
uniformly at random from an input stream, for some fixed /n/.

* "Data.Random.Choose.Tree" contains the implementation details.
* "Data.Random.Choose.IO" puts everything together as a single function
  (using IO).

-}

module Data.Random.Choose
    ( -- * Algorithm outline
      -- $outline
    ) where

{- $outline

We store items on a binary tree ('Tree'), moving them down the left or right
branch according to a coin flip. At the end, the rightmost /n/ items on the tree
are selected. Each time we 'insert' into a tree having /n/ items, we prune its
leftmost item ('applyLimit' and 'evict').

It may be helpful to think of this as lazily assigning each item a /d/-bit
score, where /d/ is the maximum depth of the tree. Moving an item to the left
or right corresponds to appending a 0 or 1 to its score. Note that the laziness
is necessary, because /d/ is not known a priori.

The process of moving items futher down the tree is referred to as
disambiguation ('disambiguate') because its purpose is to resolve ties in the
score.

-}
