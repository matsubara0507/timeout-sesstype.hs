{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.SessType.Projection where

import           Control.Arrow                  (second)
import           Control.Lens                   ((<&>), (^.))
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.Function                  (on)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.SessType.Syntax
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)

type Projection t a =
  t -> Eff '[ ReaderDef Env, EitherDef ProjectionError ] a

type Env = (Participant, Gamma)

updateGamma :: Var -> GlobalType -> Env -> Env
updateGamma v g = second $ Map.insert v g

type Gamma = Map Var GlobalType

type ProjectionError = (Text, GlobalType)

projectionAll :: GlobalType -> Map Participant LocalType
projectionAll =
  snd . Map.mapEitherWithKey projection . (Map.fromSet <$> const <*> participants)

projection :: Participant -> GlobalType -> Either ProjectionError LocalType
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
projection' g@(Timeout meta g') = do
  t1 <- projection' g'
  t2 <- projection' $ meta ^. #normal
  t3 <- projection' $ meta ^. #abend
  (p, gamma) <- ask
  if
    | p == meta ^. #owner && checkOwner p gamma (t1,t2,t3) ->
        return $ TimeoutL (shrink $ #normal @= t2 <: #abend @= t3 <: meta) t1
    | p /= meta ^. #owner && checkNotOwner p gamma meta (t1,t2,t3) -> do
        meta' <- flip marge g $ shrink (#normal @= t2 <: #abend @= t3 <: meta)
        return $ TimeoutL meta' t1
    | otherwise -> throwError ("not projectable", g)

checkOwner :: Participant -> Gamma -> (LocalType, LocalType, LocalType) -> Bool
checkOwner p gamma (t1,t2,t3) =
  and [ssnd p gamma t1, ercv t1, ssnd p gamma t2, ssnd p gamma t3]

checkNotOwner ::
  Participant
  -> Gamma
  -> (TimeoutMeta GlobalType)
  -> (LocalType, LocalType, LocalType)
  -> Bool
checkNotOwner p gamma meta (t1,t2,t3) = and
  [ not $ ssnd p gamma t1
  , not $ ercv t1
  , srcv p gamma t2
  , srcv p gamma t3
  , guarded meta gamma (Set.singleton p) $ meta ^. #normal
  , guarded meta gamma (Set.singleton p) $ meta ^. #abend
  ]

ssnd, srcv :: Participant -> Gamma -> LocalType -> Bool
ssnd = sact SendAct
srcv = sact RecvAct

data ActionType = SendAct | RecvAct deriving (Show, Eq)

sact :: ActionType -> Participant -> Gamma -> LocalType -> Bool
sact act _ _  (Send _ _) = act == SendAct
sact act _ _ (Recv _ _) = act == RecvAct
sact act p gamma (RecL _ t') = sact act p gamma t'
sact act p gamma (RVarL var) = maybe False (sact' act p) (Map.lookup var gamma)
sact _ _ _ _  = False

sact' :: ActionType -> Participant -> GlobalType -> Bool
sact' act p (Comm meta g') = if
  | p /= meta ^. #from && p /= meta ^. #to -> sact' act p g'
  | otherwise ->
      (p == meta ^. #from && act == SendAct) || (p == meta ^. #to && act == RecvAct)
sact' act p (Rec _ g') = sact' act p g'
sact' _ _ _ = False

ercv :: LocalType -> Bool
ercv (Send _ t')       = ercv t'
ercv (Recv _ CommEndL) = True
ercv (Recv _ t')       = ercv t'
ercv _                 = False

guarded :: TimeoutMeta GlobalType -> Gamma -> Set Participant -> GlobalType -> Bool
guarded meta gamma ps (Comm meta' g') = if
  | meta ^. #owner == meta' ^. #to && any (meta' ^. #from ==) ps -> True
  | any (meta' ^. #from ==) ps -> guarded meta gamma (Set.insert (meta' ^. #to) ps) g'
  | otherwise -> guarded meta gamma ps g'
guarded meta gamma ps (Rec _ g') = guarded meta gamma ps g'
guarded meta gamma ps (RVar var) = maybe False (guarded meta mempty ps) (Map.lookup var gamma)
guarded _ _ _ CommEnd = True
guarded meta gamma ps (Timeout meta' _) = if
  | meta' ^. #delta == meta ^. #delta -> False
  | otherwise -> ((&&) `on` guarded meta gamma ps) (meta' ^. #normal) (meta' ^. #abend)

marge :: TimeoutMeta LocalType -> Projection GlobalType (TimeoutMeta LocalType)
marge meta g
  | meta ^. #normal == meta ^. #abend = pure meta
  | otherwise = do
      ms1 <- label (meta ^. #normal) g
      ms2 <- label (meta ^. #abend) g
      diff ms1 ms2 meta g

diff :: Message -> Message -> (TimeoutMeta LocalType) -> Projection GlobalType (TimeoutMeta LocalType)
diff ms1 ms2 meta g
  | ms1 /= ms2 = pure meta
  | otherwise  = throwError ("match messages", g)

label :: LocalType -> Projection GlobalType Message
label (Recv meta _) _ = pure $ meta ^. #message
label (RecL _ t') g = label t' g
label (RVarL var) g = do
  (p, gamma) <- ask
  local (const (p, mempty)) $
    maybe (throwError ("undefined rec var", g)) (flip label' g) (Map.lookup var gamma)
label _ g = throwError ("no start recieve", g)

label' :: GlobalType -> Projection GlobalType Message
label' (Comm meta g') g = do
  p <- reader fst
  if
    | meta ^. #to == p   -> pure $ meta ^. #message
    | meta ^. #from /= p -> label' g' g
    | otherwise          -> throwError ("no start recieve", g)
label' (Rec _ g') g = label' g' g
label' _ g          = throwError ("no start recieve", g)
