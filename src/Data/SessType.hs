module Data.SessType
  ( SessType
  , module X
  , readGlobalType
  , readAction
  , globalTypeParser
  , projection
  , projectionAll
  , ProjectionError
  ) where

import           Data.SessType.Parser     (globalTypeParser, readAction,
                                           readGlobalType)
import           Data.SessType.Pretty     as X
import           Data.SessType.Projection (ProjectionError, projection,
                                           projectionAll)
import           Data.SessType.Semantics  as X
import           Data.SessType.Syntax     as X

type SessType = GlobalType
