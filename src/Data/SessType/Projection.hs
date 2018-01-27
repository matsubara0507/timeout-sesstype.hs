{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.SessType.Projection where

import           Control.Lens                   (over, view, (<&>), (^.))
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Extensible
import           Data.Extensible.Effect.Default
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.SessType.Syntax
import           Data.SessType.Utils            (andM)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)

type Projection a =
  Eff '[ ReaderDef Env, EitherDef ProjectionError, StateDef GlobalType ] a

runProjection ::
  Env -> GlobalType -> Projection a -> (Either ProjectionError a, GlobalType)
runProjection env gt =
  leaveEff . flip runStateDef gt . runEitherDef . flip runReaderDef env

type Env = Record
  '[ "role"    >: Participant
   , "recVars" >: Gamma
   ]

type Gamma = Map Var GlobalType

updateGamma :: Var -> GlobalType -> Env -> Env
updateGamma v g = over #recVars (Map.insert v g)

type ProjectionError = Record
  '[ "message" >: Text
   , "target"  >: GlobalType
   ]

throwWithTarget :: Text -> Projection a
throwWithTarget emessage =
  throwError =<< hsequence (#message <@=> pure emessage <: #target <@=> get <: nil)

projectionAll :: GlobalType -> Map Participant LocalType
projectionAll =
  snd . Map.mapEitherWithKey projection . (Map.fromSet <$> const <*> participants)

projection :: Participant -> GlobalType -> Either ProjectionError LocalType
projection p gt =
  fst $ runProjection (#role @= p <: #recVars @= mempty <: nil) gt projection'

projection' :: Projection LocalType
projection' = do
  gt <- get
  case gt of
    Comm meta gt' -> do
      p <- reader (view #role)
      put gt'
      projection' <&> if
        | meta ^. #from == p -> Send (shrink meta)
        | meta ^. #to   == p -> Recv (shrink meta)
        | otherwise -> id
    Rec var gt' -> do
      t' <- local (updateGamma var gt') (put gt' *> projection')
      return $ case t' of
        RVarL _ -> CommEndL
        _       -> RecL var t'
    RVar var -> pure $ RVarL var
    CommEnd -> pure CommEndL
    Timeout meta gt' -> do
      t1 <- put gt' *> projection'
      t2 <- put (meta ^. #normal) *> projection'
      t3 <- put (meta ^. #abend) *> projection'
      p <- reader (view #role)
      put gt
      if p == meta ^. #owner then do
        let meta' = shrink $ #normal @= t2 <: #abend @= t3 <: meta
        (TimeoutL meta' t1 `isProjectableIf`) =<< checkOwner (t1,t2,t3)
      else do
        meta' <- marge $ shrink (#normal @= t2 <: #abend @= t3 <: meta)
        (TimeoutL meta' t1 `isProjectableIf`) =<< checkNotOwner meta (t1,t2,t3)

isProjectableIf :: LocalType -> Bool -> Projection LocalType
isProjectableIf lt True = pure lt
isProjectableIf _ False = throwWithTarget "not projectable"

checkOwner :: (LocalType, LocalType, LocalType) -> Projection Bool
checkOwner (t1,t2,t3) = andM [ssnd t1, ercv t1, ssnd t2, ssnd t3]

checkNotOwner ::
  TimeoutMeta GlobalType -> (LocalType, LocalType, LocalType) -> Projection Bool
checkNotOwner meta (t1,t2,t3) = do
  p <- reader (view #role)
  andM
    [ not <$> ssnd t1, not <$> ercv t1, srcv t2, srcv t3
    , guarded meta (Set.singleton p) $ meta ^. #normal
    , guarded meta (Set.singleton p) $ meta ^. #abend
    ]

ssnd, srcv :: LocalType -> Projection Bool
ssnd = sact SendAct
srcv = sact RecvAct

data ActionType = SendAct | RecvAct deriving (Show, Eq)

sact :: ActionType -> LocalType -> Projection Bool
sact act (Send _ _) = pure $ act == SendAct
sact act (Recv _ _) = pure $ act == RecvAct
sact act (RecL _ lt') = sact act lt'
sact act (RVarL var) =
  maybe (pure False) (sact' act) =<< reader (Map.lookup var . view #recVars)
sact _ _ = pure False

sact' :: ActionType -> GlobalType -> Projection Bool
sact' act (Comm meta gt') = do
  p <- reader (view #role)
  let
    ip = meta ^. #from
    op = meta ^. #to
  if p /= ip && p /= op then
    sact' act gt'
  else
    pure $ (p == ip && act == SendAct) || (p == op && act == RecvAct)
sact' act (Rec _ gt') = sact' act gt'
sact' _ _ = pure False

ercv :: LocalType -> Projection Bool
ercv (Send _ t')       = ercv t'
ercv (Recv _ CommEndL) = pure True
ercv (Recv _ t')       = ercv t'
ercv _                 = pure False

guarded ::
  TimeoutMeta GlobalType -> Set Participant -> GlobalType -> Projection Bool
guarded meta ps (Comm meta' gt') = if
  | meta ^. #owner == meta' ^. #to && elem (meta' ^. #from) ps -> pure True
  | elem (meta' ^. #from) ps -> guarded meta (Set.insert (meta' ^. #to) ps) gt'
  | otherwise -> guarded meta ps gt'
guarded meta ps (Rec _ gt') = guarded meta ps gt'
guarded meta ps (RVar var) = do
  gamma <- reader (view #recVars)
  local (over #recVars mempty) $
    maybe (pure False) (guarded meta ps) (Map.lookup var gamma)
guarded _ _ CommEnd = pure True
guarded meta ps (Timeout meta' _) = if
  | meta' ^. #delta == meta ^. #delta -> pure False
  | otherwise ->
      (&&) <$> guarded meta ps (meta' ^. #normal) <*> guarded meta ps (meta' ^. #abend)

marge :: TimeoutMeta LocalType -> Projection (TimeoutMeta LocalType)
marge meta
  | meta ^. #normal == meta ^. #abend = pure meta
  | otherwise = do
      ms1 <- label (meta ^. #normal)
      ms2 <- label (meta ^. #abend)
      diff ms1 ms2 meta

diff :: Message -> Message -> TimeoutMeta LocalType -> Projection (TimeoutMeta LocalType)
diff ms1 ms2 meta
  | ms1 /= ms2 = pure meta
  | otherwise  = throwWithTarget "match messages"

label :: LocalType -> Projection Message
label (Recv meta _) = pure $ meta ^. #message
label (RecL _ t') = label t'
label (RVarL var) = do
  gamma <- reader (view #recVars)
  local (over #recVars mempty) $
    maybe (throwWithTarget "undefined rec var") label' (Map.lookup var gamma)
label _ = throwWithTarget "no start recieve"

label' :: GlobalType -> Projection Message
label' (Comm meta gt') = do
  p <- reader (view #role)
  if
    | meta ^. #to == p   -> pure $ meta ^. #message
    | meta ^. #from /= p -> label' gt'
    | otherwise          -> throwWithTarget "no start recieve"
label' (Rec _ gt') = label' gt'
label' _ = throwWithTarget "no start recieve"
