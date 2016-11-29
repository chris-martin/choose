{- |

A command-line program that uses "Data.Random.Choose".

Its arguments are specified by "Data.Random.Choose.Executable.Args".

-}

module Data.Random.Choose.Executable
    ( main
    ) where

import Data.Random.Choose.Internal.Imports
import qualified Data.Random.Choose.Executable.Args as Args
import Data.Random.Choose.IO (getSelectionsIO)

import qualified Data.Text.IO as TextIO

import Control.Exception.Base (try)
import Data.Text (Text)
import System.IO (stdin)
import System.IO.Error (isEOFError)

--------------------------------------------------------------------------------

main :: IO ()
-- ^ The entry point for an executable that reads lines from stdin and outputs
--   some fixed number of them, selected uniformly at random.

readLine :: IO (Maybe Text)
-- ^ Produces 'Just' a line of text, or 'Nothing' if the stream has ended.

--------------------------------------------------------------------------------

main = do
    args <- Args.getArgs
    let n = Args.getN args
    when (n > 0) $ do
        selections <- getSelectionsIO readLine n
        mapM_ TextIO.putStrLn selections

readLine = do
    lineEither <- try $ TextIO.hGetLine stdin
    case lineEither of
        Left e     -> if isEOFError e then pure Nothing else ioError e
        Right line -> pure $ Just line
