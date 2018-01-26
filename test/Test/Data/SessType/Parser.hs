{-# LANGUAGE OverloadedStrings #-}

module Test.Data.SessType.Parser where

import           Data.SessType.Parser
import           Test.Sample
import           Test.Tasty
import           Test.Tasty.HUnit     (testCase, (@?=))

test_readGlobalType :: [TestTree]
test_readGlobalType =
  [ testCase "simple comm end" $
      readGlobalType "A -> B : hello . end" @?= Just globalType1
  , testCase "simple recursion" $
      readGlobalType "*t . A -> B : hello . t" @?= Just globalType2
  , testCase "simple timeout" $
      readGlobalType "A@[x < 10.0, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . end)" @?= Just globalType3
  , testCase "timeout with recursion" $
      readGlobalType "*t . B -> A : sync . A@[x < 10.0, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . t)" @?= Just globalType4
  , testCase "multiparty comm end" $
      readGlobalType "A -> B : l1 . A -> C : l2 . B -> C : l3 . end" @?= Just globalType5
  , testCase "fail: participant is title case" $
      readGlobalType "a -> B : hello . end" @?= Nothing
  , testCase "fail: message is camel case" $
      readGlobalType "A -> B : Hello . end" @?= Nothing
  , testCase "fail: rec var is camel case" $
      readGlobalType "*X . A -> B : hello . X" @?= Nothing
  , testCase "fail: num in time constraint is float, no integral" $
      readGlobalType "A@[x < 10, A -> B : ping . B -> A : pong . end] . (A -> B : ok . end, A -> B : fail . end)" @?= Nothing
  ]
