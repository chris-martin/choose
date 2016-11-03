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

--------------------------------------------------------------------------------

getSelectionsIO :: forall a. (Ord a)
    => IO (Maybe a) -- ^ Producer of items to choose from; produces 'Nothing'
                    --   when there are no more items)
    -> Int          -- ^ /n/: Number of items to choose
    -> IO [a]       -- ^ /n/ of the items (or all of the items if fewer than
                    --   /n/ are produced)

-- ^ Selects /n/ items uniformly at random from an input stream.

--------------------------------------------------------------------------------

getSelectionsIO getItem limit = f 0 Tree.empty
  where

    -- We store each line of text along with its index (i) so that when we're
    -- done, we can sort by the index, thus outputting the selected items in
    -- the same order in which they were read.
    f :: Int -> Tree (Int, a) -> IO [a]
    f i tree = do
        lineMaybe <- getItem
        case lineMaybe of

            -- We read a line; insert it into the tree and recurse.
            Just line -> do
                tree' <- evalRandIO ( tree
                                    & Tree.insert (i, line)
                                    & Tree.applyLimit limit
                                    )
                f (i + 1) tree'

            -- We've reached the end of the input. Convert the tree to a list,
            -- sort it, and strip out the indices to return just the text.
            Nothing -> return $ snd <$> sort (toList tree)
