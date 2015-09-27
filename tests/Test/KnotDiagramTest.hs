module Test.KnotDiagramTest where
import KnotDiagram

import RolfsonTable

import Test.TestingInterface

import Data.Maybe(isJust,fromJust)

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

--Test Knot diagram module
testKnotDiagram :: TestTree
--Add a depth option so rolfson table knots of all depths are checked.
testKnotDiagram = localOption (SmallCheckDepth 8) $
                     testGroup "Test Knot Diagram" 
                            [testFromPlanarDiagram,
                             testKnotWalk
                            ]

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
testPDAllRolfson = SC.testProperty "All rolfson knots are valid" $
                        isJust . fromPlanarDiagram . planarDiagram . tRolfson


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
  
  
--Test construction of knot diagram from planar diagram
testKnotWalk :: TestTree
testKnotWalk = testGroup "Tests the knot walk of a knot"
                                  [ testKWAllEdgesUsed,
                                    testKWNoReapeatEdges,
                                    testKWBowTie,
                                    testKWDoubleTwist,
                                    testKWTrefoil
                                  ]

--Test all the edges are used in a walk 
--Uses rolfson table to generate knots for now.
testKWAllEdgesUsed :: TestTree
testKWAllEdgesUsed = SC.testProperty "When a knot is walked all edges are used" $
                       testKnot . knotDiagram . tRolfson
                       --Checks length is sufficient as we also check irredundancy.
                  where testKnot knot = IMap.size (edges knot) == length (knotWalk knot)
                        
--Tests that no edge appears twice in a walk
--Uses rolfson table to generate knots for now.
testKWNoReapeatEdges :: TestTree
testKWNoReapeatEdges = SC.testProperty "When a knot is walked all no edge is repeated" $
                        testKnot . knotDiagram . tRolfson
                    where testKnot knot = noRepeats (knotWalk knot)
                          noRepeats xs = length xs == Set.size (Set.fromList xs)
                    
--The 'bow-tie' unknot with one crossing has a given 2 element walk
-- This tests a double loop crossing is handled correctly 
testKWBowTie :: TestTree
testKWBowTie = testCase "Walk bow-tie knot" $ [1,2] @?= knotWalk knotBowTie
  where knotBowTie = fromJust $ fromPlanarDiagram [(1,1,2,2)]
  
--The 'double-twist' unknot with two crossings has a given 4 element walk
-- This tests a double loop crossing is handled correctly 
testKWDoubleTwist :: TestTree
testKWDoubleTwist = testCase "Walk double-twist knot" $ [1,2,3,4] @?= knotWalk knotDoubleTwist
  where knotDoubleTwist = fromJust $ fromPlanarDiagram [(4,1,1,2),(3,2,4,3)]
  
  
--The trefoil has a given 6 element walk
-- This tests a non-trivial
testKWTrefoil :: TestTree
testKWTrefoil = testCase "Walk trefoil knot" $ [1..6] @?= knotWalk knotTrefoil
  where knotTrefoil = fromJust $ fromPlanarDiagram [(4,2,5,1),(2,6,3,5),(6,4,1,3)]
