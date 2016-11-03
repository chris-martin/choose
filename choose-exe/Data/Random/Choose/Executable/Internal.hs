module Data.Random.Choose.Executable.Internal
    ( intToString
    ) where

import Data.Random.Choose.Internal.Imports

import qualified GHC.Show as Show

intToString :: Int -> String
intToString = Show.show
