{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sample where

import           Prelude         hiding (LT)

import           Data.Extensible
import           Data.SessType

-- A -> B : hello . end
globalType1 :: GlobalType
globalType1 = atobEnd "hello"

localType1A :: LocalType
localType1A = Send (#to @= "B" <: #message @= "hello" <: nil) CommEndL

localType1B :: LocalType
localType1B = Recv (#from @= "A" <: #message @= "hello" <: nil) CommEndL

-- *t . A -> B : hello . t
globalType2 :: GlobalType
globalType2 = Rec "t" $ atobT "hello"

localType2A :: LocalType
localType2A =
  RecL "t" $ Send (#to @= "B" <: #message @= "hello" <: nil) $ RVarL "t"

localType2B :: LocalType
localType2B =
  RecL "t" $ Recv (#from @= "A" <: #message @= "hello" <: nil) $ RVarL "t"

-- A@[x < 10, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . end)
globalType3 :: GlobalType
globalType3 =
  Timeout
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= atobEnd "ok"
    <: #abend @= atobEnd "fail"
    <: nil ) $
      Comm (#from @= "A" <: #to @= "B" <: #message @= "ping" <: nil) $
        Comm (#from @= "B" <: #to @= "A" <: #message @= "pong" <: nil) CommEnd

localType3A :: LocalType
localType3A =
  TimeoutL
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= Send (#to @= "B" <: #message @= "ok" <: nil) CommEndL
    <: #abend @= Send (#to @= "B" <: #message @= "fail" <: nil) CommEndL
    <: nil ) $
      Send (#to @= "B" <: #message @= "ping" <: nil) $
        Recv (#from @= "B" <: #message @= "pong" <: nil) CommEndL

localType3B :: LocalType
localType3B =
  TimeoutL
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= Recv (#from @= "A" <: #message @= "ok" <: nil) CommEndL
    <: #abend @= Recv (#from @= "A" <: #message @= "fail" <: nil) CommEndL
    <: nil ) $
      Recv (#from @= "A" <: #message @= "ping" <: nil) $
        Send (#to @= "A" <: #message @= "pong" <: nil) CommEndL

-- *t . B -> A : sync . A@[x < 10, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . t)
globalType4 :: GlobalType
globalType4 =
  Rec "t" $
    Comm (#from @= "B" <: #to @= "A" <: #message @= "sync" <: nil) $
    Timeout
       ( #owner @= "A"
      <: #delta @= ("x" `LT` 10.0)
      <: #normal @= atobEnd "ok"
      <: #abend @= atobT "fail"
      <: nil ) $
        Comm (#from @= "A" <: #to @= "B" <: #message @= "ping" <: nil) $
          Comm (#from @= "B" <: #to @= "A" <: #message @= "pong" <: nil) CommEnd

localType4A :: LocalType
localType4A =
  RecL "t" $
    Recv (#from @= "B" <: #message @= "sync" <: nil) $
    TimeoutL
       ( #owner @= "A"
      <: #delta @= ("x" `LT` 10.0)
      <: #normal @= Send (#to @= "B" <: #message @= "ok" <: nil) CommEndL
      <: #abend @= Send (#to @= "B" <: #message @= "fail" <: nil) (RVarL "t")
      <: nil ) $
        Send (#to @= "B" <: #message @= "ping" <: nil) $
          Recv (#from @= "B" <: #message @= "pong" <: nil) CommEndL

localType4B :: LocalType
localType4B =
  RecL "t" $
    Send (#to @= "A" <: #message @= "sync" <: nil) $
    TimeoutL
       ( #owner @= "A"
      <: #delta @= ("x" `LT` 10.0)
      <: #normal @= Recv (#from @= "A" <: #message @= "ok" <: nil) CommEndL
      <: #abend @= Recv (#from @= "A" <: #message @= "fail" <: nil) (RVarL "t")
      <: nil ) $
        Recv (#from @= "A" <: #message @= "ping" <: nil) $
          Send (#to @= "A" <: #message @= "pong" <: nil) CommEndL


-- A -> B : l1 . A -> C : l2 . B -> C : l3 . end
globalType5 :: GlobalType
globalType5 =
  Comm (#from @= "A" <: #to @= "B" <: #message @= "l1" <: nil) $
    Comm (#from @= "A" <: #to @= "C" <: #message @= "l2" <: nil) $
      Comm (#from @= "B" <: #to @= "C" <: #message @= "l3" <: nil) CommEnd

localType5A :: LocalType
localType5A =
  Send (#to @= "B" <: #message @= "l1" <: nil) $
    Send (#to @= "C" <: #message @= "l2" <: nil) CommEndL

localType5B :: LocalType
localType5B =
  Recv (#from @= "A" <: #message @= "l1" <: nil) $
    Send (#to @= "C" <: #message @= "l3" <: nil) CommEndL

localType5C :: LocalType
localType5C =
  Recv (#from @= "A" <: #message @= "l2" <: nil) $
    Recv (#from @= "B" <: #message @= "l3" <: nil) CommEndL


atobEnd :: Message -> GlobalType
atobEnd ms = Comm (#from @= "A" <: #to @= "B" <: #message @= ms <: nil) CommEnd

atobT :: Message -> GlobalType
atobT ms = Comm (#from @= "A" <: #to @= "B" <: #message @= ms <: nil) $ RVar "t"
