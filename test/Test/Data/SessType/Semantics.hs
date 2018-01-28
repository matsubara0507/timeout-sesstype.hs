{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Data.SessType.Semantics where

import           Prelude                 hiding (LT, init)

import           Control.Monad           ((<=<))
import           Data.Extensible
import           Data.SessType.Semantics
import           Data.SessType.Syntax
import           Test.Sample
import           Test.Tasty              hiding (Timeout)
import           Test.Tasty.HUnit        (testCase, (@?=))

gt1, gt2, gt3, gt4, gt5, gt6, gt7, gt8, gt9, gt10, gt11, gt12 :: GlobalType
gt1 = Comm (#from @= "A" <: #to @= "B" <: #message @= "hello" <: nil) CommEnd
gt2 = Comm' (#from @= "A" <: #to @= "B" <: #message @= "hello" <: nil) CommEnd
gt3 = Comm (#from @= "C" <: #to @= "D" <: #message @= "hello" <: nil) gt1
gt4 = Comm (#from @= "C" <: #to @= "D" <: #message @= "hello" <: nil) gt2
gt5 = Comm (#from @= "C" <: #to @= "D" <: #message @= "hello" <: nil) CommEnd
gt6 = Rec "t" $ Comm (#from @= "A" <: #to @= "B" <: #message @= "hello" <: nil) $ RVar "t"
gt7 = Comm' (#from @= "A" <: #to @= "B" <: #message @= "hello" <: nil) $ gt6
gt8 =
  Timeout
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= atobEnd "ok"
    <: #abend @= atobEnd "fail"
    <: nil ) $
      Comm (#from @= "A" <: #to @= "B" <: #message @= "ping" <: nil) $
        Comm (#from @= "B" <: #to @= "A" <: #message @= "pong" <: nil) CommEnd
gt9 =
  Timeout
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= atobEnd "ok"
    <: #abend @= atobEnd "fail"
    <: nil ) $
      Comm' (#from @= "A" <: #to @= "B" <: #message @= "ping" <: nil) $
        Comm (#from @= "B" <: #to @= "A" <: #message @= "pong" <: nil) CommEnd
gt10 =
  Timeout
     ( #owner @= "A"
    <: #delta @= ("x" `LT` 10.0)
    <: #normal @= atobEnd "ok"
    <: #abend @= atobEnd "fail"
    <: nil ) $
      Comm (#from @= "B" <: #to @= "A" <: #message @= "pong" <: nil) CommEnd
gt11 = Comm' (#from @= "A" <: #to @= "B" <: #message @= "ok" <: nil) CommEnd
gt12 = Comm' (#from @= "A" <: #to @= "B" <: #message @= "fail" <: nil) CommEnd

test_globalTransition :: [TestTree]
test_globalTransition =
  [ testCase "send message" $
      fmap peel (transition (send ("A","B") "hello") $ init gt1) @?= Right gt2
  , testCase "async send messgae" $
      fmap peel (transition (send ("A","B") "hello") $ init gt3) @?= Right gt4
  , testCase "fail: send messgae: misstake message" $
      fmap peel (transition (send ("A","B") "hello!") $ init gt1) @?= Left "no transition pattern"
  , testCase "fail: send messgae: recieve action" $
      fmap peel (transition (recv ("A","B") "hello") $ init gt1) @?= Left "no transition pattern"
  , testCase "recieve message" $
      fmap peel (transition (recv ("A","B") "hello") $ init gt2) @?= Right CommEnd
  , testCase "async recieve message" $
      fmap peel (transition (recv ("A","B") "hello") $ init gt4) @?= Right gt5
  , testCase "fail: recieve message: misstake message" $
      fmap peel (transition (recv ("A","B") "hello!") $ init gt2) @?= Left "no transition pattern"
  , testCase "fail: recieve message: send action" $
      fmap peel (transition (send ("A","B") "hello") $ init gt2) @?= Left "no transition pattern"
  , testCase "recursion" $
      fmap peel (transition (send ("A","B") "hello") $ init gt6) @?= Right gt7
  , testCase "timeout: start inner comm" $
      fmap peel (transition (send ("A","B") "ping") $ init gt8) @?= Right gt9
  , testCase "timeout: inner comm" $
      fmap peel (transition (recv ("A","B") "ping") <=< transition (send ("A","B") "ping") $ init gt8) @?= Right gt10
  , testCase "timeout: correct comm" $
      fmap peel (
        transition (send ("A","B") "ok")
          <=< transition (recv ("B","A") "pong")
          <=< transition (send ("B","A") "pong")
          <=< transition (recv ("A","B") "ping")
          <=< transition (send ("A","B") "ping")
            $ init gt8
      ) @?= Right gt11
  , testCase "timeout: timeout comm" $
      fmap peel (
        transition (send ("A","B") "fail")
          <=< transition (time 10)
          <=< transition (recv ("A","B") "ping")
          <=< transition (send ("A","B") "ping")
            $ init gt8
      ) @?= Right gt12
  , testCase "fail: timeout: cannot timeout" $
      fmap peel (
        transition (send ("A","B") "fail")
          <=< transition (recv ("B","A") "pong")
          <=< transition (send ("B","A") "pong")
          <=< transition (recv ("A","B") "ping")
          <=< transition (send ("A","B") "ping")
            $ init gt8
      ) @?= Left "no transition pattern"
  , testCase "fail: timeout: cannnot correct comm" $
      fmap peel (
        transition (send ("A","B") "ok")
          <=< transition (time 10)
          <=< transition (recv ("B","A") "pong")
          <=< transition (send ("B","A") "pong")
          <=< transition (recv ("A","B") "ping")
          <=< transition (send ("A","B") "ping")
            $ init gt8
      ) @?= Left "no transition pattern"
  , testCase "time elapse: bottom" $
      (transition (time 10) $ init gt1) @?= Right (init gt1)
  , testCase "fail: time elapse: ready" $
      fmap peel (transition (time 10) <=< transition (send ("A","B") "ping") $ init gt8) @?= Left "no transition pattern"
  ]
