{- |

A command-line program that uses "Data.Random.Choose".

Its arguments are specified by "Data.Random.Choose.Executable.Args".

-}

module Data.Random.Choose.Executable
    ( main
    ) where

import Data.Random.Choose.Internal.Prelude
import qualified Data.Random.Choose.Executable.Args as Args
import Data.Random.Choose (choose)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

--------------------------------------------------------------------------------

-- | The entry point for an executable that reads lines from stdin and outputs
--   some fixed number of them, selected uniformly at random.
main :: IO ()
main = do
    args <- Args.getArgs
    let n = Args.getN args
    when (n > 0) $ do
        selections <- choose n $ streamMap Text.pack streamStdinLn
        mapM_ TextIO.putStrLn selections
