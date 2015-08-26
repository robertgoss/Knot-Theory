module KnotDiagramTest where
import KnotDiagram

import RolfsonTable

import Data.Maybe(isJust)


import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

--Test Knot diagram module
testKnotDiagram :: TestTree
testKnotDiagram = testGroup "Test Knot Diagram" [testFromPlanarDiagram]

--Test construction of knot diagram from planar diagram
testFromPlanarDiagram :: TestTree
testFromPlanarDiagram = testGroup "Tests construction of knot diagram from planar diagram"
                                  [testPDAllRolfson,
                                   testPDBowTie,
                                   testPDDoubleTwist,
                                   testPDInvalidEdges,
                                   testPDInvalidLocallyPlanar,
                                   testPDInvalidGlobalPlanar
                                  ]
                                  


--All Rolfson planar diagrams should construct valid knot diagrams
--Set option depth to use all knots by depth.
testPDAllRolfson :: TestTree
testPDAllRolfson = localOption (SmallCheckDepth 8) $
                     SC.testProperty "All rolfson knots are valid" $
                        isJust . fromPlanarDiagram . planarDiagram


--Does the given pd form a valid knot.
validPd :: PlanarDiagram -> Bool
validPd = isJust . fromPlanarDiagram

--The 'bow-tie' unknot with one crossing should construct a valid knot
-- This tests a double loop crossing
testPDBowTie :: TestTree
testPDBowTie = testCase "Construct bow-tie knot" $ True @?= validPd pdBowTie
  where pdBowTie = [(1,1,2,2)]

--A double twisted unknot should construct a valid knot
-- This tests crossings with a single loop
testPDDoubleTwist :: TestTree
testPDDoubleTwist = testCase "Construct double twist knot" 
                                $ True @?= validPd pdDoubleTwist
  where pdDoubleTwist = [(4,1,1,2),(3,2,4,3)]

--The following knot is not a valid diagram
-- Tests that all edges just connect at most 2 distinct crossings
testPDInvalidEdges :: TestTree
testPDInvalidEdges = testCase "Invalid 3 crossing edge" 
                                $ False @?= validPd pd3cross
  where pd3cross = [(1,2,3,4),(1,2,2,3),(1,3,3,4)]

--The following knot is not a valid diagram
--Tests that loop must be locally planar
testPDInvalidLocallyPlanar :: TestTree
testPDInvalidLocallyPlanar = testCase "Invalid local non-planar" 
                                $ False @?= validPd pdLocalNonPlanar
  where pdLocalNonPlanar = [(1,3,2,1),(4,3,4,2)]

--The following knot is not a valid diagram
--Tests that globally non-planar diagram is not valid.
testPDInvalidGlobalPlanar :: TestTree
testPDInvalidGlobalPlanar = testCase "Invalid global non-planar" 
                                $ False @?= validPd pdGlobalNonPlanar
  where pdGlobalNonPlanar = [(1,1,2,2)] --Still need to find value