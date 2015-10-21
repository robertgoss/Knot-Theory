module Test.LinkDiagram.RegionTest where

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import qualified Data.Set as Set

import LinkDiagram.Region

--Test Crossing module
--Reduce the depth to 4 as region expands quickly and this is sufficient.
testRegion :: TestTree
testRegion = localOption (SmallCheckDepth 4) $
                         testGroup "Test Region" [ testSmallcheck,
                                                   testUnknotInBounds,
                                                   testEdgeInBounds
                                                 ]
              
--Test region produced by smallcheck is valid                                     
testSmallcheck :: TestTree
testSmallcheck = testGroup "Test smallcheck instance is valid" [ testSmallcheckNoEmpty,
                                                                 testSmallcheckEdgesetsDistinct
                                                               ]

testSmallcheckNoEmpty :: TestTree
testSmallcheckNoEmpty = SC.testProperty "Region should have no empty edge sets." smallcheckNoEmptyTest'
  where smallcheckNoEmptyTest' :: TestingRegion Int Int -> Bool
        smallcheckNoEmptyTest' = smallcheckNoEmptyTest . tRegion
        smallcheckNoEmptyTest region = Set.empty `Set.notMember` regionEdges region

testSmallcheckEdgesetsDistinct :: TestTree
testSmallcheckEdgesetsDistinct = SC.testProperty "Region edge sets should be distinct" smallcheckEdgesetsDistinctTest'
  where smallcheckEdgesetsDistinctTest' :: TestingRegion Int Int -> Bool
        smallcheckEdgesetsDistinctTest' = smallcheckEdgesetsDistinctTest . tRegion
        smallcheckEdgesetsDistinctTest region = all distinct allPairs
          where edgeSets = Set.toList $ regionEdges region
                allPairs = filter nonEqual [(set1, set2) | set1 <- edgeSets, set2 <- edgeSets] --Get all non equal pairs
                distinct (set1,set2) = Set.null $ set1 `Set.intersection` set2
                nonEqual (set1,set2) = set1 /= set2
                
--Test if unknot is in bounds
testUnknotInBounds :: TestTree
testUnknotInBounds = testGroup "Test unknotInBounds" [testUnknotInBoundsTrue, testUnknotInBoundsFalse]

--Test 2 cases if it is both true and false
testUnknotInBoundsTrue :: TestTree
testUnknotInBoundsTrue = testCase "Unknot is in bounds" $ True @?= unknotInBounds region 4
  where region :: Region Int Int
        region = Region (Set.fromList [Set.fromList [1,2,4],Set.fromList [0,3,100]]) (Set.fromList [1,-1,4,8,100])
  
testUnknotInBoundsFalse :: TestTree
testUnknotInBoundsFalse = testCase "Unknot is not in bounds" $ False @?= unknotInBounds region 3
  where region :: Region Int Int
        region = Region (Set.fromList [Set.fromList [1,2,4],Set.fromList [0,3,100]]) (Set.fromList [1,-1,4,8,100])
  
--Test if edge is in bounds
testEdgeInBounds :: TestTree
testEdgeInBounds = testGroup "Test edgeInBounds" [testEdgeInBoundsTrue, testEdgeInBoundsFalse]
  
--Test 2 cases if it is both true and false
testEdgeInBoundsTrue :: TestTree
testEdgeInBoundsTrue = testCase "Edge is in bounds" $ True @?= edgeInBounds region 2
  where region :: Region Int Int
        region = Region (Set.fromList [Set.fromList [1,2,4],Set.fromList [0,3,100]]) (Set.fromList [1,-1,4,8,100])
        
testEdgeInBoundsFalse :: TestTree
testEdgeInBoundsFalse = testCase "Edge is not in bounds" $ False @?= edgeInBounds region 5
  where region :: Region Int Int
        region = Region (Set.fromList [Set.fromList [1,2,4],Set.fromList [0,3,100]]) (Set.fromList [1,-1,4,8,100])
  