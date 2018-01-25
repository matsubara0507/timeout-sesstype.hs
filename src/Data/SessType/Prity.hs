{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SessType.Prity where

import           Prelude              hiding (LT)

import           Control.Lens         ((^.))
import           Data.SessType.Syntax
import           Data.Text            (Text, pack)

class Prity a where
  prity :: a -> Text

instance Prity GlobalType where
  prity = undefined

instance Prity LocalType where
  prity (Send meta t') =
    mconcat [meta ^. #to, "!", meta ^. #message, " . ", prity t']
  prity (Recv meta t') =
    mconcat [meta ^. #from, "?", meta ^. #message, " . ", prity t']
  prity (RecL var t') =
    mconcat ["*", var, " . ", prity t']
  prity (RVarL var) = var
  prity CommEndL = "end"
  prity (TimeoutL meta t') = mconcat
    [ meta ^. #owner, "@[", prity $ meta ^. #delta, ", ", prity t'
    , "] . (", prity $ meta ^. #normal, ", ", prity $ meta ^. #abend, ")"
    ]

instance Prity TConstraint where
  prity (LT c t) = mconcat [c, " < ", pack $ show t]
  prity (LE c t) = mconcat [c, " <= ", pack $ show t]
