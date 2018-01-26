{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SessType.Pretty where

import           Prelude              hiding (LT, unlines)

import           Control.Lens         ((^.))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.SessType.Syntax
import           Data.Text            (Text, pack, unlines)

class Pretty a where
  pretty :: a -> Text

instance Pretty GlobalType where
  pretty (Comm meta g') =
    mconcat [meta ^. #from, " -> ", meta ^. #to, " : ", meta ^. #message, " . ", pretty g']
  pretty (Rec var g') =
    mconcat ["μ", var, " . ", pretty g']
  pretty (RVar var) = var
  pretty CommEnd = "end"
  pretty (Timeout meta g') = mconcat
    [ meta ^. #owner, "@[", pretty $ meta ^. #delta, ", ", pretty g', "] . "
    , "(", pretty $ meta ^. #normal, ", ", pretty $ meta ^. #abend, ")"
    ]

instance Pretty LocalType where
  pretty (Send meta t') =
    mconcat [meta ^. #to, "!", meta ^. #message, " . ", pretty t']
  pretty (Recv meta t') =
    mconcat [meta ^. #from, "?", meta ^. #message, " . ", pretty t']
  pretty (RecL var t') =
    mconcat ["μ", var, " . ", pretty t']
  pretty (RVarL var) = var
  pretty CommEndL = "end"
  pretty (TimeoutL meta t') = mconcat
    [ meta ^. #owner, "@[", pretty $ meta ^. #delta, ", ", pretty t', "] . "
    , mconcat $
        if meta ^. #normal == meta ^. #abend then
          [ pretty $ meta ^. #normal ]
        else
          [ "(", pretty $ meta ^. #normal, ", ", pretty $ meta ^. #abend, ")" ]
    ]

instance Pretty TConstraint where
  pretty (LT c t) = mconcat [c, " < ", pack $ show t]
  pretty (LE c t) = mconcat [c, " <= ", pack $ show t]

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty =
    unlines . fmap (\(k,v) -> mconcat [pretty k, ": ", pretty v]) . Map.toList

instance Pretty Text where
  pretty = id
