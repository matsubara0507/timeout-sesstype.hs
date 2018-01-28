{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Data.SessType.Syntax where

import           Prelude         hiding (LT)

import           Control.Lens    ((%~), (&), (^.))
import           Data.Extensible
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)

type Participant = Text

type Message = Text

type Var = Text

data GlobalType
  = Comm CommMeta GlobalType
  | Comm' CommMeta GlobalType -- Intermediate state, sended but not receive
  | Rec Var GlobalType
  | RVar Var
  | CommEnd
  | Timeout (TimeoutMeta GlobalType) GlobalType
  deriving (Show, Eq)

data LocalType
  = Send SendMeta LocalType
  | Recv RecvMeta LocalType
  | RecL Var LocalType
  | RVarL Var
  | CommEndL
  | TimeoutL (TimeoutMeta LocalType) LocalType
  deriving (Show, Eq)

type CommMeta = Record
  '[ "from"    >: Participant
   , "to"      >: Participant
   , "message" >: Message
   ]

type SendMeta = Record
  '[ "to"      >: Participant
   , "message" >: Message
   ]

type RecvMeta = Record
  '[ "from"    >: Participant
   , "message" >: Message
   ]

type TimeoutMeta st = Record
  '[ "owner"  >: Participant
   , "delta"  >: TConstraint
   , "normal" >: st
   , "abend"  >: st
   ]

data TConstraint
  = LT Clock Time
  | LE Clock Time
  deriving (Show, Eq)

type Clock = Text

type Time = Double

participants :: GlobalType -> Set Participant
participants (Comm meta g') =
  foldr Set.insert (participants g') [meta ^. #from, meta ^. #to]
participants (Rec _ g') = participants g'
participants (Timeout meta g') =
  mconcat $ participants <$> [g', meta ^. #normal, meta ^. #abend]
participants _ = mempty

class ToClock a where
  clocks :: a -> Set Clock

instance ToClock GlobalType where
  clocks (Comm _ gt) = clocks gt
  clocks (Rec _ gt)  = clocks gt
  clocks (Timeout meta _) = mconcat $
    [clocks (meta ^. #delta), clocks (meta ^. #normal), clocks (meta ^. #abend)]
  clocks _ = mempty

instance ToClock LocalType where
  clocks (Send _ lt) = clocks lt
  clocks (Recv _ lt) = clocks lt
  clocks (RecL _ lt) = clocks lt
  clocks (TimeoutL meta _) = mconcat $
    [clocks (meta ^. #delta), clocks (meta ^. #normal), clocks (meta ^. #abend)]
  clocks _ = mempty

instance ToClock TConstraint where
  clocks (LT x _) = Set.singleton x
  clocks (LE x _) = Set.singleton x

class Recursion a where
  replaceRVar :: Var -> a -> a -> a

instance Recursion GlobalType where
  replaceRVar v gt (Comm meta gt') = Comm meta (replaceRVar v gt gt')
  replaceRVar v gt (Comm' meta gt') = Comm' meta (replaceRVar v gt gt')
  replaceRVar v gt (Rec v' gt') = Rec v' (replaceRVar v gt gt')
  replaceRVar v gt (RVar v') = if v' == v then gt else (RVar v')
  replaceRVar _ _ CommEnd = CommEnd
  replaceRVar v gt (Timeout meta gt') =
    Timeout (meta & #normal %~ (replaceRVar v gt) & #abend %~ (replaceRVar v gt)) gt'
