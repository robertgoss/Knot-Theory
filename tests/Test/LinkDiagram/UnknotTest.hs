module Test.LinkDiagram.UnknotTest where

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit


import LinkDiagram.Unknot

--Test Unknot module
--Reduce the depth to 4 as unknot expands quickly and this is sufficient.
testUnknot :: TestTree
testUnknot = localOption (SmallCheckDepth 4) $
                         testGroup "Test Unknot" [ testSmallcheck,
                                                   testMeetingRegion
                                                 ]
          
--Test region produced by smallcheck is valid                                        
testSmallcheck :: TestTree
testSmallcheck = testGroup "Test smallcheck instance is valid" [ testSmallcheckDistinctRegions ]

--Regions of a unknot should be distinct
testSmallcheckDistinctRegions :: TestTree
testSmallcheckDistinctRegions = SC.testProperty "" smallcheckDistinctRegionsTest'
  where smallcheckDistinctRegionsTest' :: TestingUnknot Int Int -> Bool
        smallcheckDistinctRegionsTest' = smallcheckDistinctRegionsTest . tUnknot
        smallcheckDistinctRegionsTest unknot = leftRegion unknot /= rightRegion unknot

--Test meetsRegion
testMeetingRegion :: TestTree
testMeetingRegion = testGroup "Test meetsRegion" [testMeetingRegionTrue, testMeetingRegionFalse]

--A pair of tests for if a unknot meets a regions and otherwise
testMeetingRegionTrue :: TestTree
testMeetingRegionTrue = testCase "Unknot meets region" $ True @?= meetsRegion 1 unknot
  where unknot :: Unknot Int Int
        unknot = Unknot 1 4 1
        
testMeetingRegionFalse :: TestTree
testMeetingRegionFalse = testCase "Unknot does not meet region" $ False @?= meetsRegion 2 unknot
  where unknot :: Unknot Int Int
        unknot = Unknot 1 4 1
