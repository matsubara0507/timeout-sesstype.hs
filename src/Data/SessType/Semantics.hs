{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.SessType.Semantics where

import           Prelude              hiding (LT, init)

import           Control.Lens         ((^.))
import           Control.Monad        (join)
import           Data.Extensible
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.SessType.Queue
import           Data.SessType.Syntax
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)

type LTSError = Text

type ClockAssign = Map Clock (Maybe Time)
type Queues = Map (Participant, Participant) (Queue Message)

initClock :: Maybe Time -> Set Clock -> ClockAssign
initClock v = Map.fromSet (const v)

data TransAction
  = SendAction CommMeta
  | RecvAction CommMeta
  | TimeElapse Time
  deriving (Show, Eq)

send, recv :: (Participant, Participant) -> Message -> TransAction
send (p,q) ms = SendAction $ #from @= p <: #to @= q <: #message @= ms <: nil
recv (p,q) ms = RecvAction $ #from @= p <: #to @= q <: #message @= ms <: nil

time :: Time -> TransAction
time = TimeElapse

isTimeElapse :: TransAction -> Bool
isTimeElapse (TimeElapse _) = True
isTimeElapse _              = False

bot :: Maybe Time
bot = Nothing

class Transition a where
  data ST a
  init :: a -> ST a
  peel :: ST a -> a
  transition :: TransAction -> ST a -> Either LTSError (ST a)

instance Transition GlobalType where
  data ST GlobalType = G ClockAssign GlobalType deriving (Show, Eq)
  init = flip G <*> (initClock Nothing . clocks)
  peel (G _ gt) = gt
  transition (TimeElapse t) (G nu gt) =
    let nu' = addClocks t nu  in if
    | maybe True (models nu') $ rdy gt -> pure $ G nu' gt
    | otherwise -> Left "no transition pattern"
  transition act@(SendAction meta) (G nu (Comm meta' gt)) = if
    | meta == meta' ->
        pure $ G nu (Comm' meta' gt)
    | not $ elem (meta ^. #from) [meta' ^. #from, meta' ^. #to] -> do
        (G nu' gt') <- transition act (G nu gt)
        return $ G nu' (Comm meta' gt')
    | otherwise -> Left "no transition pattern"
  transition act@(RecvAction meta) (G nu (Comm meta' gt)) = if
    | not $ elem (meta ^. #from) [meta' ^. #from, meta' ^. #to] -> do
        (G nu' gt') <- transition act (G nu gt)
        return $ G nu' (Comm meta' gt')
    | otherwise -> Left "no transition pattern"
  transition act@(RecvAction meta) (G nu (Comm' meta' gt)) = if
    | meta == meta' ->
        pure $ G nu gt
    | meta ^. #to /= meta' ^. #to -> do
        (G nu' gt') <- transition act (G nu gt)
        return $ G nu' (Comm' meta' gt')
    | otherwise -> Left "no transition pattern"
  transition act@(SendAction meta) (G nu (Comm' meta' gt)) = if
    | meta ^. #to /= meta' ^. #to -> do
        (G nu' gt') <- transition act (G nu gt)
        return $ G nu' (Comm' meta' gt')
    | otherwise -> Left "no transition pattern"
  transition act (G nu gt@(Rec v gt')) =
    transition act (G nu $ replaceRVar v gt gt')
  transition act (G nu (Timeout meta gt)) = if
    | clockEq nu (meta ^. #delta) bot && not (isTimeElapse act) -> do
        let
          nu' = replaceClock (meta ^. #delta) (Just 0) nu
        (G nu'' gt') <- transition act (G nu' gt)
        return $ G nu'' (Timeout meta gt')
    | nu `models` (meta ^. #delta) && not (isTimeElapse act) && gt /= CommEnd -> do
      (G nu' gt') <- transition act (G nu gt)
      return $ G nu' (Timeout meta gt')
    | nu `models` (meta ^. #delta) && not (isTimeElapse act) -> do
        let
          nu' = replaceClock (meta ^. #delta) Nothing nu
        (G nu'' gt') <- transition act (G nu' $ meta ^. #normal)
        return $ G nu'' gt'
    | not (nu `models` (meta ^. #delta))
        && not (clockEq nu (meta ^. #delta) bot) && not (isTimeElapse act) -> do
        let
          nu' = replaceClock (meta ^. #delta) Nothing nu
        (G nu'' gt') <- transition act (G nu' $ meta ^. #abend)
        return $ G nu'' gt'
    | otherwise -> Left "no transition pattern"
  transition _ _ = Left "no transition pattern"

clockEq :: ClockAssign -> TConstraint -> Maybe Time -> Bool
clockEq nu delta t =
  all (maybe False (t ==) . flip Map.lookup nu) (Set.toList $ clocks delta)

replaceClock :: TConstraint -> Maybe Time -> ClockAssign -> ClockAssign
replaceClock delta t nu =
  Set.foldr (Map.adjust $ const t) nu $ clocks delta

addClocks :: Time -> ClockAssign -> ClockAssign
addClocks t = Map.map (fmap ((+) t))

models :: ClockAssign -> TConstraint -> Bool
models nu (LT v t) = maybe False (<  t) . join $ Map.lookup v nu
models nu (LE v t) = maybe False (<= t) . join $ Map.lookup v nu

rdy :: GlobalType -> Maybe TConstraint
rdy (Comm _ gt)       = rdy gt
rdy (Comm' _ gt)      = rdy gt
rdy (Rec _ gt)        = rdy gt
rdy (RVar _)          = Nothing
rdy CommEnd           = Nothing
rdy (Timeout meta gt) = const (meta ^. #delta) <$> recieves gt

recieves :: GlobalType -> Maybe Participant
recieves (Comm' meta _) = pure $ meta ^. #to
recieves _              = Nothing
