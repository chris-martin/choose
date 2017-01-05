{- |

A command-line program that uses "Data.Random.Choose".

Its arguments are specified by "Data.Random.Choose.Executable.Args".

-}

module Data.Random.Choose.Executable
    ( main

    -- * Args
    , Args(..), argsParser, argsInfoMod, argsParserInfo

    -- ** /n/
    , getN, defaultN, parserN

    ) where

import Data.Random.Choose (choose)

import Control.Applicative (Applicative(..), (<|>), optional)
import Control.Monad (Monad(..), mapM_, when)
import Data.Function ((.), ($), id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.Ord (Ord(..))
import Data.String (String)
import GHC.Show (show)
import System.IO (IO)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)

import Prelude (putStrLn)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Streaming.Prelude as Stream
import qualified Options.Applicative.Builder as Opt

import Paths_choose_exe (version)
import Data.Version (showVersion)

--------------------------------------------------------------------------------

-- | The entry point for an executable that reads lines from stdin and outputs
--   some fixed number of them, selected uniformly at random.
main :: IO ()
main = execParser argsParserInfo >>= mainArgs

mainArgs :: Args -> IO ()
mainArgs ArgsVersion = putStrLn $ "choose " <> showVersion version
mainArgs (ArgsNormal args') = mainArgs' args'

mainArgs' :: Args' -> IO ()
mainArgs' args =
    when (n > 0) $ do
        selections <- choose n $ Stream.map Text.pack Stream.stdinLn
        mapM_ TextIO.putStrLn selections
  where
    n = getN args


--------------------------------------------------------------------------------
--  Args
--------------------------------------------------------------------------------

-- | The command-line arguments for 'main'.
data Args = ArgsNormal Args'
          | ArgsVersion

data Args' = Args'
    { _argN :: Maybe Int  -- ^ /n/, the number of items to choose.
    }

-- | The default value for /n/ if not specified.
defaultN :: Int
defaultN = 1

getN :: Args' -> Int
getN = maybe defaultN id . _argN

parserN :: Parser (Maybe Int)
parserN = optional $ Opt.argument read mod
  where
      read = Opt.auto :: Opt.ReadM Int
      mod = Opt.metavar "N" <> Opt.help help
      help = "Number of items to choose (default: "
             <> (show :: Int -> String) defaultN <> ")"

argsParser :: Parser Args
argsParser = versionParser <|> normalParser
  where
    versionParser = Opt.flag' ArgsVersion $
        Opt.short 'v' <> Opt.long "version" <> Opt.hidden
    normalParser = ArgsNormal . Args' <$> parserN

argsInfoMod :: Opt.InfoMod a
argsInfoMod = Opt.header "Selects lines from stdin uniformly at random."

argsParserInfo :: ParserInfo Args
argsParserInfo = Opt.info (helper <*> argsParser) argsInfoMod
