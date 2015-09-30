module Test.LinkDiagram.CrossingTest where

import LinkDiagram.Crossing

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit


--Test Crossing module
testCrossing :: TestTree
testCrossing = testGroup "Test Crossing" [ testFromEdges,
                                           testMeetingNumber,
                                           testOpposite]

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
testFromEdgesInverse = SC.testProperty "fromEdges is an inverse to edgeIndices" fromEdgesInverseTest
      where fromEdgesInverseTest :: TestingCrossing Int -> Bool
            fromEdgesInverseTest tCross = Just crossing == (fromEdges . toTuple . edgeIndices) crossing
                where crossing = tCrossing tCross
                      toTuple [e1,e2,e3,e4] = (e1,e2,e3,e4)

--Test various local constraint are invalid:
testFromEdges3EdgesInvalid = testCase "A crossing with 3 edges is invalid" $ Nothing @?= fromEdges (1,2,1,1)

testFromEdges4EdgesInvalid = testCase "A crossing with 4 edges is invalid" $ Nothing @?= fromEdges (2,2,2,2)

testFromEdgesVOppositeInvalid = testCase "A crossing with a vertical loop is invalid" $ Nothing @?= fromEdges (1,2,1,3)

testFromEdgesHOppositeInvalid = testCase "A crossing with a horizontal loop is invalid" $ Nothing @?= fromEdges (1,2,3,2)

testFromEdgesDoubleLoopOppositeInvalid = testCase "A double loop crossing with a horizontal loop is invalid" $ Nothing @?= fromEdges (1,2,1,2)

--Some unit tests to make sure a crossing loop and double loop are correctly created.
testCrossingCreated = testCase "Can correctly construct valid crossing" $ Just (Crossing 1 2 3 4) @?= fromEdges (1,2,3,4)

testLoopCreated = testCase "Can correctly construct valid crossing with a loop" $ Just (LoopCrossingTR 3 1 2) @?= fromEdges (1,2,3,3)

testDoubleLoopCreated = testCase "Can correctly construct valid crossing with 2 loops" $ Just (DoubleLoopL  1 2) @?= fromEdges (1,1,2,2)

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
testMeetingNumberNonEdgeWithCrossing = testCase "Meeting of non edge with a crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing 1 3 5 7) 2

testMeetingNumberNonEdgeWithLoop = testCase "Meeting of non edge with a looped crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing 1 1 5 7) 2

testMeetingNumberNonEdgeWithDLoop = testCase "Meeting of non edge with a double looped crossing"
                                            $ 0 @?= edgeMeetingNumber (Crossing 1 1 7 7) 2

testMeetingNumberEdgeWithCrossing = testCase "Meeting of edge with a crossing"
                                            $ 1 @?= edgeMeetingNumber (Crossing 1 3 5 7) 1

testMeetingNumberEdgeWithLoop = testCase "Meeting of edge with a looped crossing"
                                            $ 1 @?= edgeMeetingNumber (Crossing 1 1 5 7) 5

testMeetingNumberLoopWithLoop = testCase "Meeting of loop with a looped crossing"
                                            $ 2 @?= edgeMeetingNumber (Crossing 1 1 5 7) 1

testMeetingNumberLoopWithDLoop = testCase "Meeting of loop with a double looped crossing"
                                            $ 2 @?= edgeMeetingNumber (Crossing 1 1 7 7) 7

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
--We add a Int type specifier to alow com
testOppositeCorrectCrossing = testCase "Opposite edges on a crossing"
                                           $ True @?= oppositeEdges (Crossing 1 3 5 7) 1 5

testOppositeCorrectLoop = testCase "Opposite edges on a looped crossing"
                                           $ True @?= oppositeEdges  (Crossing 1 1 5 7) 1 7

testOppositeCorrectDLoop = testCase "Opposite edges on a double looped crossing"
                                           $ True @?= oppositeEdges  (Crossing 1 1 2 2) 1 2

--Test various crossing types recognise edges which are not opposite
testOppositeIncorrectCrossing = testCase "Non-opposite edges on a crossing"
                                           $ False @?= oppositeEdges  (Crossing 1 3 5 7) 1 3

testOppositeIncorrectLoop = testCase "Non-opposite edges on a looped crossing"
                                           $ False @?= oppositeEdges  (Crossing 1 1 5 7) 1 1

testOppositeIncorrectDLoop = testCase "Non-opposite edges on a double looped crossing"
                                           $ False @?= oppositeEdges  (Crossing 1 1 2 2) 2 2