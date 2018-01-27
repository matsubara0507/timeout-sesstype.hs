{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Data.SessType.Syntax where

import           Control.Lens    ((^.))
import           Data.Extensible
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)

type Participant = Text

type Message = Text

type Var = Text

data GlobalType
  = Comm CommMeta GlobalType
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
