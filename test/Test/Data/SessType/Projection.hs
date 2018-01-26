{-# LANGUAGE OverloadedStrings #-}

module Test.Data.SessType.Projection where

import qualified Data.Map                 as Map
import           Data.SessType.Parser
import           Data.SessType.Projection
import           Test.Sample
import           Test.Tasty
import           Test.Tasty.HUnit         (testCase, (@?=))

test_projectionAll :: [TestTree]
test_projectionAll =
  [ testCase "simple comm end" $
      projectionAll globalType1 @?= Map.fromList [("A", localType1A), ("B", localType1B)]
  , testCase "simple recursion" $
      projectionAll globalType2 @?= Map.fromList [("A", localType2A), ("B", localType2B)]
  , testCase "simple timeout" $
      projectionAll globalType3 @?= Map.fromList [("A", localType3A), ("B", localType3B)]
  , testCase "timeout with recursion" $
      projectionAll globalType4 @?= Map.fromList [("A", localType4A), ("B", localType4B)]
  , testCase "multiparty comm end" $
      projectionAll globalType5 @?= Map.fromList [("A", localType5A), ("B", localType5B), ("C", localType5C)]
  , testCase "fail: not start send A in G1" $
      projectionAll <$> readGlobalType "A@[x<10.0,B->A:l2.end].(A->B:l3.end,A->B:l4.end)" @?= Just mempty
  , testCase "fail: not start reciev B in G2"  $
      projectionAll <$> readGlobalType "A@[x<10.0,A->B:l1.B->A:l2.end].(B->A:l3.end,A->B:l4.end)" @?= Just mempty
  , testCase "fail: unguarded B"  $
      projectionAll <$> readGlobalType "*t.A@[x<10.0,A->B:ping.B->A:pong.end].(A->B:ok.end,A->B:fail.t)"
        @?= Just (Map.fromList [("A", localType4A')])
  , testCase "fail: same message"  $
      projectionAll <$> readGlobalType "*t.B->A:sync.A@[x<10.0,A->B:ping.B->A:pong.end].(A->B:ok.end,A->B:ok.t)"
        @?= Just (Map.fromList [("A", localType4A'')])
  ]
