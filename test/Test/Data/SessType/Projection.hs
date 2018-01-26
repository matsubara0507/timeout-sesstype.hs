{-# LANGUAGE OverloadedStrings #-}

module Test.Data.SessType.Projection where

import qualified Data.Map                 as Map
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
  ]
