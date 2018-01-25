{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SessType.Projection where

import           Control.Lens         ((&), (^.))
import           Data.Extensible
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.SessType.Syntax

projectionAll :: GlobalType -> Map Participant LocalType
projectionAll = (flip Map.fromSet . participants) <*> flip projection

projection :: Participant -> GlobalType -> LocalType
projection p (Comm meta g') =
  projection p g' & if
    | meta ^. #from == p -> Send (shrink meta)
    | meta ^. #to   == p -> Recv (shrink meta)
    | otherwise -> id
projection p (Rec var g') =
  case projection p g' of
    RVarL _ -> CommEndL
    t'      -> RecL var t'
projection _ (RVar var) = RVarL var
projection _ CommEnd = CommEndL
projection p (Timeout meta g') =
  TimeoutL (shrink $ #normal @= t2 <: #abend @= t3 <: meta) t1
  where
    t1 = projection p g'
    t2 = projection p $ meta ^. #normal
    t3 = projection p $ meta ^. #abend
