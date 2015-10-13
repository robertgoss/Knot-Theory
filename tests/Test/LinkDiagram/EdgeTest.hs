module Test.LinkDiagram.EdgeTest where

import LinkDiagram.Edge

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit


--Test Crossing module
--Reduce the depth to 3 as edge expands quickly and this is sufficient.
testEdge :: TestTree
testEdge = localOption (SmallCheckDepth 3) $
                         testGroup "Test Edge" [ testSmallcheck,
                                                 testMeetsRegion,
                                                 testCrossingMeetingNumber
                                               ]
                         
--TestSmallcheck
--Make sure all the smallcheck crossings are valid
testSmallcheck :: TestTree
testSmallcheck = testGroup "Test smallcheck instance" [testSmallcheckDistinctRegions]

testSmallcheckDistinctRegions :: TestTree
testSmallcheckDistinctRegions = SC.testProperty "The regions in an edge should be distinct" testDistinctRegions'
  where testDistinctRegions' :: TestingEdge Int Int Int -> Bool
        testDistinctRegions' = testDistinctRegions . tEdge
        testDistinctRegions edge = leftRegion edge /= rightRegion edge
        
--Tests meetsRegion
testMeetsRegion :: TestTree
testMeetsRegion = testGroup "Test meetsRegion" [testMeetingRegionTrue, testMeetingRegionFalse]

--A pair of tests for if a unknot meets a regions and otherwise
testMeetingRegionTrue :: TestTree
testMeetingRegionTrue = testCase "Edge meets region" $ True @?= meetsRegion 1 unknot
  where unknot :: Edge Int Int Int
        unknot = Edge 1 2 4 1 1
        
testMeetingRegionFalse :: TestTree
testMeetingRegionFalse = testCase "Edge does not meet region" $ False @?= meetsRegion 2 unknot
  where unknot :: Edge Int Int Int
        unknot = Edge 1 2 4 1 1



--Tests crossingMeetingNumber
--Test combinations of edge and loop with crossings that meet and don't meet
testCrossingMeetingNumber :: TestTree
testCrossingMeetingNumber = testGroup "Test crossingMeetingNumber" [testMeetingNumberEdgeCrossing,
                                                                    testMeetingNumberEdgeDistinctCrossing,
                                                                    testMeetingNumberLoopCrossing,
                                                                    testMeetingNumberLoopDistinctCrossing
                                                                   ]
                                                                   
testMeetingNumberEdgeCrossing :: TestTree
testMeetingNumberEdgeCrossing = testCase "Test meeting of edge with a crossing it meets." 
                                  $ 1 @?= crossingMeetingNumber edge 3
  where edge :: Edge Int Int Int
        edge = Edge 1 3 2 4 1
  
testMeetingNumberEdgeDistinctCrossing :: TestTree
testMeetingNumberEdgeDistinctCrossing = testCase "Test meeting of edge with a crossing it does not meet." 
                                  $ 0 @?= crossingMeetingNumber edge 2
  where edge :: Edge Int Int Int
        edge = Edge 1 3 2 4 1
  
testMeetingNumberLoopCrossing :: TestTree
testMeetingNumberLoopCrossing = testCase "Test meeting of loop with a crossing it meets." 
                                  $ 2 @?= crossingMeetingNumber edge 3
  where edge :: Edge Int Int Int
        edge = Edge 3 3 2 4 1
  
testMeetingNumberLoopDistinctCrossing :: TestTree
testMeetingNumberLoopDistinctCrossing = testCase "Test meeting of loop with a crossing it does not meet." 
                                  $ 0 @?= crossingMeetingNumber edge 2
  where edge :: Edge Int Int Int
        edge = Edge 3 3 2 4 1