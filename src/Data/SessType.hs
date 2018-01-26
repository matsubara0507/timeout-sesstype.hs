module Data.SessType
  ( SessType
  , module X
  , readGlobalType
  , globalTypeParser
  , projection
  , projectionAll
  , ProjectionError
  ) where

import           Data.SessType.Parser     (globalTypeParser, readGlobalType)
import           Data.SessType.Pretty     as X
import           Data.SessType.Projection (ProjectionError, projection,
                                           projectionAll)
import           Data.SessType.Syntax     as X

type SessType = GlobalType
