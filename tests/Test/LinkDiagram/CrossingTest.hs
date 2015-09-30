module Test.LinkDiagram.CrossingTest where

import LinkDiagram.Crossing

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit


--Test Crossing module
--Reduce the depth to 3 as crossing expands quickly and this is sufficient.
testCrossing :: TestTree
testCrossing = localOption (SmallCheckDepth 3) $
                         testGroup "Test Crossing" [ testSmallcheck,
                                                     testEdgeIndicies,
                                                     testFromEdges,
                                                     testMeetingNumber,
                                                     testOpposite]
--TestSmallSmallcheck
--Make sure all the smallcheck crossings are valid
testSmallcheck :: TestTree
testSmallcheck = testGroup "Test smallcheck instance" [testSmallcheckValid]

testSmallcheckValid :: TestTree
testSmallcheckValid = SC.testProperty "All crossing should be valid with distinct properties" testDistinctProp'
  where testDistinctProp' :: TestingCrossing Int -> Bool
        testDistinctProp' = testDistinctProp . tCrossing
        testDistinctProp (Crossing e1 e2 e3 e4) = not $ all (==e1) [e2,e3,e4]
        testDistinctProp (LoopCrossingTL loop e1 e2) = not $ all (==loop) [e1,e2]
        testDistinctProp (LoopCrossingTR loop e1 e2) = not $ all (==loop) [e1,e2]
        testDistinctProp (LoopCrossingBL loop e1 e2) = not $ all (==loop) [e1,e2]
        testDistinctProp (LoopCrossingBR loop e1 e2) = not $ all (==loop) [e1,e2]
        testDistinctProp (DoubleLoopL loop1 loop2) = loop1 /= loop2
        testDistinctProp (DoubleLoopR loop1 loop2) = loop1 /= loop2
        

--Test edgeIndices
testEdgeIndicies :: TestTree
testEdgeIndicies = testGroup "Test edgeIndices" [testEdgeIndices4Element] 

testEdgeIndices4Element :: TestTree
testEdgeIndices4Element = SC.testProperty "edgeIndices should always produce a 4 element list." edgeIndices4ElementTest
    where edgeIndices4ElementTest :: TestingCrossing Int -> Bool
          edgeIndices4ElementTest = (==4) . length . edgeIndices . tCrossing

--Test fromEdges
testFromEdges :: TestTree
testFromEdges = testGroup "Test fromEdges" [ testFromEdgesInverse,
                                             testFromEdges3EdgesInvalid,
                                             testFromEdges4EdgesInvalid,
                                             testFromEdgesVOppositeInvalid,
                                             testFromEdgesHOppositeInvalid,
                                             testFromEdgesDoubleLoopOppositeInvalid,
                                             testCrossingCreated,
                                             testLoopCreated,
                                             testDoubleLoopCreated
                                           ]

--Prop

--Edgeindex and fromEdges mutual inverse
-- With just a wrapper and we convert the output of edgeIndices to edgeIndices.
testFromEdgesInverse :: TestTree
testFromEdgesInverse = SC.testProperty "fromEdges is an inverse to edgeIndices" fromEdgesInverseTest
      where fromEdgesInverseTest :: TestingCrossing Int -> Bool
            fromEdgesInverseTest tCross = Just crossing == (fromEdges . toTuple . edgeIndices) crossing
                where crossing = tCrossing tCross
                      toTuple [e1,e2,e3,e4] = (e1,e2,e3,e4)
                      toTuple _ = error "edgeIndices should always return 4 element list"

--Test various local constraint are invalid:
testFromEdges3EdgesInvalid :: TestTree
testFromEdges3EdgesInvalid = testCase "A crossing with 3 edges is invalid" $ Nothing @?= fromEdges (1::Int,2,1,1)

testFromEdges4EdgesInvalid :: TestTree
testFromEdges4EdgesInvalid = testCase "A crossing with 4 edges is invalid" $ Nothing @?= fromEdges (2::Int,2,2,2)

testFromEdgesVOppositeInvalid :: TestTree
testFromEdgesVOppositeInvalid = testCase "A crossing with a vertical loop is invalid" $ Nothing @?= fromEdges (1::Int,2,1,3)

testFromEdgesHOppositeInvalid :: TestTree
testFromEdgesHOppositeInvalid = testCase "A crossing with a horizontal loop is invalid" $ Nothing @?= fromEdges (1::Int,2,3,2)

testFromEdgesDoubleLoopOppositeInvalid :: TestTree
testFromEdgesDoubleLoopOppositeInvalid = testCase "A double loop crossing with a horizontal loop is invalid" $ Nothing @?= fromEdges (1::Int,2,1,2)

--Some unit tests to make sure a crossing loop and double loop are correctly created.
testCrossingCreated :: TestTree
testCrossingCreated = testCase "Can correctly construct valid crossing" $ Just (Crossing 1 2 3 4) @?= fromEdges (1::Int,2,3,4)

testLoopCreated :: TestTree
testLoopCreated = testCase "Can correctly construct valid crossing with a loop" $ Just (LoopCrossingTR 3 1 2) @?= fromEdges (1::Int,2,3,3)

testDoubleLoopCreated :: TestTree
testDoubleLoopCreated = testCase "Can correctly construct valid crossing with 2 loops" $ Just (DoubleLoopL  1 2) @?= fromEdges (1::Int,1,2,2)

--Test meeting number
testMeetingNumber :: TestTree
testMeetingNumber = testGroup "Test edgeMeetingNumber" [testMeetingNumberNonEdgeWithCrossing,
                                                        testMeetingNumberNonEdgeWithLoop,
                                                        testMeetingNumberNonEdgeWithDLoop,
                                                        testMeetingNumberEdgeWithCrossing,
                                                        testMeetingNumberEdgeWithLoop,
                                                        testMeetingNumberLoopWithLoop,
                                                        testMeetingNumberLoopWithDLoop
                                                       ]

--Test the various combinations of a crossing type and if an edge
-- either doesn't meet the crossing, meets the crossing in one place or as a loop.
testMeetingNumberNonEdgeWithCrossing :: TestTree
testMeetingNumberNonEdgeWithCrossing = testCase "Meeting of non edge with a crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing (1::Int) 3 5 7) 2

testMeetingNumberNonEdgeWithLoop :: TestTree
testMeetingNumberNonEdgeWithLoop = testCase "Meeting of non edge with a looped crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing (1::Int) 1 5 7) 2

testMeetingNumberNonEdgeWithDLoop :: TestTree
testMeetingNumberNonEdgeWithDLoop = testCase "Meeting of non edge with a double looped crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing (1::Int) 1 7 7) 2

testMeetingNumberEdgeWithCrossing :: TestTree
testMeetingNumberEdgeWithCrossing = testCase "Meeting of edge with a crossing"
                                            $ 1 @?= edgeMeetingNumber (Crossing (1::Int) 3 5 7) 1

testMeetingNumberEdgeWithLoop :: TestTree
testMeetingNumberEdgeWithLoop = testCase "Meeting of edge with a looped crossing"
                                            $ 1 @?= edgeMeetingNumber (Crossing (1::Int) 1 5 7) 5

testMeetingNumberLoopWithLoop :: TestTree
testMeetingNumberLoopWithLoop = testCase "Meeting of loop with a looped crossing"
                                            $ 2 @?= edgeMeetingNumber (Crossing (1::Int) 1 5 7) 1

testMeetingNumberLoopWithDLoop :: TestTree
testMeetingNumberLoopWithDLoop = testCase "Meeting of loop with a double looped crossing"
                                            $ 2 @?= edgeMeetingNumber (Crossing (1::Int) 1 7 7) 7

--Test oppositeEdges
testOpposite :: TestTree
testOpposite = testGroup "Test oppositeEdges" [testOppositeCorrectCrossing,
                                               testOppositeCorrectLoop,
                                               testOppositeCorrectDLoop,
                                               testOppositeIncorrectCrossing,
                                               testOppositeIncorrectLoop,
                                               testOppositeIncorrectDLoop
                                              ]

--Test various crossing types recognise opposite edges
--We add a Int type specifier to allow com
testOppositeCorrectCrossing :: TestTree
testOppositeCorrectCrossing = testCase "Opposite edges on a crossing"
                                           $ True @?= oppositeEdges (Crossing (1::Int) 3 5 7) 1 5

testOppositeCorrectLoop :: TestTree
testOppositeCorrectLoop = testCase "Opposite edges on a looped crossing"
                                           $ True @?= oppositeEdges  (Crossing (1::Int) 1 5 7) 1 7

testOppositeCorrectDLoop :: TestTree
testOppositeCorrectDLoop = testCase "Opposite edges on a double looped crossing"
                                           $ True @?= oppositeEdges  (Crossing (1::Int) 1 2 2) 1 2

--Test various crossing types recognise edges which are not opposite
testOppositeIncorrectCrossing :: TestTree
testOppositeIncorrectCrossing = testCase "Non-opposite edges on a crossing"
                                           $ False @?= oppositeEdges  (Crossing (1::Int) 3 5 7) 1 3

testOppositeIncorrectLoop :: TestTree
testOppositeIncorrectLoop = testCase "Non-opposite edges on a looped crossing"
                                           $ False @?= oppositeEdges  (Crossing (1::Int) 1 5 7) 1 1

testOppositeIncorrectDLoop :: TestTree
testOppositeIncorrectDLoop = testCase "Non-opposite edges on a double looped crossing"
                                           $ False @?= oppositeEdges  (Crossing (1::Int) 1 2 2) 2 2