module Data.SessType
  ( SessType
  , module X
  , readGlobalType
  , globalTypeParser
  ) where

import           Data.SessType.Parser (globalTypeParser, readGlobalType)
import           Data.SessType.Syntax as X

type SessType = GlobalType
