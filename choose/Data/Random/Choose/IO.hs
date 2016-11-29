{- |

'IO'-specific functionality around the random selection algorithm described in
"Data.Random.Choose".

-}

module Data.Random.Choose.IO
    ( getSelectionsIO
    ) where

import Data.Random.Choose.Internal.Imports
import qualified Data.Random.Choose.Tree as Tree
import Data.Random.Choose.Tree (Tree)

import Data.List (sortBy)
import Data.Ord (comparing)

--------------------------------------------------------------------------------

getSelectionsIO :: forall a.
       IO (Maybe a) -- ^ Producer of items to choose from; produces 'Nothing'
                    --   when there are no more items)
    -> Int          -- ^ /n/: Number of items to choose
    -> IO [a]       -- ^ /n/ of the items (or all of the items if fewer than
                    --   /n/ are produced)

-- ^ Selects /n/ items uniformly at random from an input stream.

--------------------------------------------------------------------------------

data Indexed a = Indexed { index :: Int, indexedValue :: a }

sortIndexedValues :: [Indexed a] -> [a]
sortIndexedValues = fmap indexedValue . sortBy (comparing index)

getSelectionsIO getItem limit = f 0 Tree.empty
  where

    -- We store each item along with its index (i) so that when we're done,
    -- we can sort by the index, thus outputting the selected items in the
    -- same order in which they were read.
    f :: Int -> Tree (Indexed a) -> IO [a]
    f i tree = do
        lineMaybe <- getItem
        case lineMaybe of

            -- We read a line; insert it into the tree and recurse.
            Just line -> do
                tree' <- evalRandIO $ tree & Tree.insert (Indexed i line)
                                           & Tree.applyLimit limit
                f (i + 1) tree'

            -- We've reached the end of the input. Convert the tree to a list,
            -- sort it, and strip out the indices to return just the text.
            Nothing -> pure $ sortIndexedValues $ toList tree
