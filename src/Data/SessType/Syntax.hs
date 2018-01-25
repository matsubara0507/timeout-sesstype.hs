{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Data.SessType.Syntax where

import           Data.Extensible
import           Data.Text       (Text)

type Participant = Text

type Message = Text

type Var = Text

data GlobalType
  = Comm CommMeta GlobalType
  | Rec Var GlobalType
  | RVar Var
  | CommEnd
  | Timeout TimeoutMeta GlobalType
  deriving (Show, Eq)

type CommMeta = Record
  '[ "from"    >: Participant
   , "to"      >: Participant
   , "message" >: Message
   ]

type TimeoutMeta = Record
  '[ "owner"  >: Participant
   , "delta"  >: TConstraint
   , "normal" >: GlobalType
   , "abend"  >: GlobalType
   ]

data TConstraint
  = LT Clock Time
  | LE Clock Time
  deriving (Show, Eq)

type Clock = Text

type Time = Double
