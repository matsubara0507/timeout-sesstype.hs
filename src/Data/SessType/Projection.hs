{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.SessType.Projection where

import           Control.Arrow                  (second)
import           Control.Lens                   ((<&>), (^.))
import           Control.Monad.Reader.Class
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.SessType.Syntax
import           Data.Text                      (Text)

type Projection t a =
  t -> Eff '[ ReaderDef Env, EitherDef Error ] a

type Env = (Participant, Gamma)

updateGamma :: Var -> GlobalType -> Env -> Env
updateGamma v g = second $ Map.insert v g

type Gamma = Map Var GlobalType

type Error = (Text, GlobalType)

projectionAll :: GlobalType -> Map Participant LocalType
projectionAll =
  snd . Map.mapEitherWithKey projection . (Map.fromSet <$> const <*> participants)

projection :: Participant -> GlobalType -> Either Error LocalType
projection p =
  leaveEff . runEitherDef . flip runReaderDef (p, mempty) . projection'

projection' :: Projection GlobalType LocalType
projection' (Comm meta g') = do
  p <- reader fst
  projection' g' <&> if
    | meta ^. #from == p -> Send (shrink meta)
    | meta ^. #to   == p -> Recv (shrink meta)
    | otherwise -> id
projection' (Rec var g') = do
  t' <- local (updateGamma var g') $ projection' g'
  return $ case t' of
    RVarL _ -> CommEndL
    _       -> RecL var t'
projection' (RVar var) = pure $ RVarL var
projection' CommEnd = pure CommEndL
projection' (Timeout meta g') = do
  t1 <- projection' g'
  t2 <- projection' $ meta ^. #normal
  t3 <- projection' $ meta ^. #abend
  return $ TimeoutL (shrink $ #normal @= t2 <: #abend @= t3 <: meta) t1
