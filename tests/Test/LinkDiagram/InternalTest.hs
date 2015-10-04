module Test.LinkDiagram.InternalTest where

import LinkDiagram.Internal

import LinkDiagram.Crossing
import LinkDiagram.Region
import LinkDiagram.Unknot
import LinkDiagram.Edge
import LinkDiagram.Component

import Test.LinkDiagram.TestingInterface

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

--Test internal module

testInternal :: TestTree
testInternal = testGroup "Test Internal" [testValidLink
                                         ]
--Helper object
--Valid link diagrams 
--For diagrams see pages
--An unknot
--Tests simplest knot
unknot :: LinkDiagramData
unknot = LinkDiagramData {
  crossings = IMap.empty,
  edges = IMap.empty,
  unknots = IMap.fromList [(1,Unknot 1 3 2)],
  regions = IMap.fromList [(1,Region Set.empty (Set.singleton 1)), 
                           (3, Region Set.empty (Set.singleton 1))],
  components = IMap.fromList [(2,UnknottedComponent 1)]
}

--Trefoil
--Tests non-trival knot
trefoil :: LinkDiagramData
trefoil = LinkDiagramData {
  crossings = IMap.fromList [(1,Crossing 3 6 4 1),
                             (2,Crossing 1 4 2 5),
                             (3,Crossing 5 2 6 3)
                            ],
  edges = IMap.fromList [(1, Edge 1 2 2 1 2),
                         (2, Edge 2 3 5 3 2),
                         (3, Edge 3 1 4 1 2),
                         (4, Edge 1 2 5 2 2),
                         (5, Edge 2 3 3 1 2),
                         (6, Edge 3 1 5 4 2)
                        ],
  unknots = IMap.empty,
  regions = IMap.fromList [(1, Region (Set.singleton (Set.fromList [1,3,5])) Set.empty),
                           (2, Region (Set.singleton (Set.fromList [1,4])) Set.empty),
                           (3, Region (Set.singleton (Set.fromList [2,5])) Set.empty),
                           (4, Region (Set.singleton (Set.fromList [3,6])) Set.empty),
                           (5, Region (Set.singleton (Set.fromList [2,4,6])) Set.empty)
                          ],
  components = IMap.fromList [(2,PathComponent [1,2,3,4,5,6])]
}

--BowTie
--Test double loop crossing
bowtie :: LinkDiagramData
bowtie = LinkDiagramData {
  crossings = IMap.fromList [(1,DoubleLoopL 2 1)],
  edges = IMap.fromList [(1,Edge 1 1 3 1 4),
                         (2,Edge 1 1 2 3 4)
                        ],
  unknots =  IMap.empty,
  regions = IMap.fromList [(1,Region (Set.singleton (Set.singleton 1)) Set.empty),
                           (2,Region (Set.singleton (Set.singleton 2)) Set.empty),
                           (3,Region (Set.singleton (Set.fromList [1,2])) Set.empty)
                          ],
  components = IMap.fromList [(4, PathComponent [1,2])]
}

--Double Twist
--Test crossing with loops
doubleTwist :: LinkDiagramData
doubleTwist = LinkDiagramData {
  crossings = IMap.fromList [(1,LoopCrossingTR 1 4 2),
                             (3,LoopCrossingTR 3 2 4)
                            ],
  edges = IMap.fromList [(1,Edge 1 1 7 1 3),
                         (2,Edge 1 3 3 7 3),
                         (3,Edge 3 3 7 5 3),
                         (4,Edge 3 1 3 7 3)
                        ],
  unknots = IMap.empty,
  regions = IMap.fromList [(1, Region (Set.singleton (Set.singleton 1)) Set.empty),
                           (3, Region (Set.singleton (Set.fromList [2,4])) Set.empty),
                           (5, Region (Set.singleton (Set.singleton 3)) Set.empty),
                           (7, Region (Set.singleton (Set.fromList [1,2,3,4])) Set.empty)
                          ],
  components = IMap.fromList [(3,PathComponent [1,2,3,4])]
}

--A unlink of a trefoil and a unknot
--Tests link with multiple components
trefoilUnknot :: LinkDiagramData
trefoilUnknot = LinkDiagramData {
  crossings = IMap.fromList [(1,Crossing 3 6 4 1),
                             (2,Crossing 1 4 2 5),
                             (3,Crossing 5 2 6 3)
                            ],
  edges = IMap.fromList [(1, Edge 1 2 2 1 2),
                         (2, Edge 2 3 5 3 2),
                         (3, Edge 3 1 4 1 2),
                         (4, Edge 1 2 5 2 2),
                         (5, Edge 2 3 3 1 2),
                         (6, Edge 3 1 5 4 2)
                        ],
  unknots = IMap.fromList [(4, Unknot 5 6 3)],
  regions = IMap.fromList [(1, Region (Set.singleton (Set.fromList [1,3,5])) Set.empty),
                           (2, Region (Set.singleton (Set.fromList [1,4])) Set.empty),
                           (3, Region (Set.singleton (Set.fromList [2,5])) Set.empty),
                           (4, Region (Set.singleton (Set.fromList [3,6])) Set.empty),
                           (5, Region (Set.singleton (Set.fromList [2,4,6])) (Set.singleton 4)),
                           (6, Region Set.empty (Set.singleton 4))
                          ],
  components = IMap.fromList [(2,PathComponent [1,2,3,4,5,6]),
                              (3, UnknottedComponent 4)
                             ]
}

--Borromean rings
borromeanRings :: LinkDiagramData
borromeanRings = LinkDiagramData {
  crossings = IMap.fromList [(1,Crossing 12 1 9 2),
                             (3,Crossing 4 5 1 8),
                             (4,Crossing 9 5 10 6),
                             (5,Crossing 7 11 8 12),
                             (6,Crossing 2 6 3 7),
                             (7,Crossing 3 10 4 11)
                            ],
  edges = IMap.fromList [(1,Edge 3 1 8 2 1),
                         (2,Edge 1 6 3 5 1),
                         (3,Edge 6 7 7 1 1),
                         (4,Edge 7 3 4 6 1),
                         (5,Edge 3 4 4 8 3),
                         (6,Edge 4 6 7 3 3),
                         (7,Edge 6 5 1 5 3),
                         (8,Edge 5 3 6 2 3),
                         (9,Edge 1 4 8 3 2),
                         (10,Edge 4 7 4 7 2),
                         (11,Edge 7 5 6 1 2),
                         (12,Edge 5 1 2 5 2)
                        ],
  unknots = IMap.empty,
  regions = IMap.fromList [(1,Region (Set.singleton (Set.fromList [3,7,11])) Set.empty),
                           (2,Region (Set.singleton (Set.fromList [1,8,12])) Set.empty),
                           (3,Region (Set.singleton (Set.fromList [2,6,9])) Set.empty),
                           (4,Region (Set.singleton (Set.fromList [4,5,10])) Set.empty),
                           (5,Region (Set.singleton (Set.fromList [2,7,12])) Set.empty),
                           (6,Region (Set.singleton (Set.fromList [4,8,11])) Set.empty),
                           (7,Region (Set.singleton (Set.fromList [3,6,10])) Set.empty),
                           (8,Region (Set.singleton (Set.fromList [1,5,9])) Set.empty)
                          ],
  components = IMap.fromList [(1,PathComponent [1,2,3,4]),
                              (2,PathComponent [9,10,11,12]),
                              (3,PathComponent [5,6,7,8])
                             ]
}

--Test multiple components which are linked


--We will test validation by checking that these are correct and 
--Produce invalid links by altering then

--Test valid Link
testValidLink :: TestTree
testValidLink = testGroup "Test isValidLinkDiagram" 
                                          [testValidLinkUnknotValid,
                                           testValidLinkTrefoilValid,
                                           testValidLinkBowtieValid,
                                           testValidLinkDoubleTwistValid,
                                           testValidLinkTrefoilUnknotValid,
                                           testValidLinkBorromeanRingsValid
                    
                                          ]
                                          
--Use the known knots and test they are valid
testValidLinkUnknotValid :: TestTree
testValidLinkUnknotValid = testCase "Test valid unknot is valid" 
                                       $ True @?= isValidLinkDiagram unknot

testValidLinkTrefoilValid :: TestTree
testValidLinkTrefoilValid = testCase "Test valid trefoil is valid" 
                                       $ True @?= isValidLinkDiagram trefoil

testValidLinkBowtieValid :: TestTree
testValidLinkBowtieValid = testCase "Test valid bowtie is valid" 
                                        $ True @?= isValidLinkDiagram bowtie

testValidLinkDoubleTwistValid :: TestTree
testValidLinkDoubleTwistValid = testCase "Test valid double twist is valid" 
                                         $ True @?= isValidLinkDiagram doubleTwist

testValidLinkTrefoilUnknotValid :: TestTree
testValidLinkTrefoilUnknotValid = testCase "Test valid trefoil unknot is valid" 
                                         $ True @?= isValidLinkDiagram trefoilUnknot

testValidLinkBorromeanRingsValid :: TestTree
testValidLinkBorromeanRingsValid = testCase "Test valid Borromean Rings is valid" 
                                         $ True @?= isValidLinkDiagram borromeanRings