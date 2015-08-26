module KnotDiagramTest where
import KnotDiagram

import RolfsonTable


import Test.Tasty
import Test.Tasty.QuickCheck as QC
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
testPDAllRolfson = undefined

--The 'bow-tie' unknot with one crossing should construct a valid knot
-- This tests a double loop crossing
testPDBowTie = undefined

--A double twisted unknot should construct a valid knot
-- This tests crossings with a single loop
testPDDoubleTwist = undefined

--The following knot is not a valid diagram
-- Tests that all edges just connect at most 2 distinct crossings
testPDInvalidEdges = undefined

--The following knot is not a valid diagram
--Tests that loop must be locally planar
testPDInvalidLocallyPlanar = undefined

--The following knot is not a valid diagram
--Tests that globally non-planar diagram is not valid.
testPDInvalidGlobalPlanar = undefined