module Test.LinkDiagram.InternalTest where

import LinkDiagram.Internal

import LinkDiagram.Crossing
import LinkDiagram.Region
import LinkDiagram.Unknot
import LinkDiagram.Edge
import LinkDiagram.Component

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import Test.Tasty
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
                                           testValidLinkBorromeanRingsValid,
                                           testValidCrossingsValid,
                                           testValidEdgesValid,
                                           testValidUnknotsValid,
                                           testValidRegionsValid,
                                           testValidComponentsValid
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
                                         
--Tests that invalid crossings are detected
testValidCrossingsValid :: TestTree
testValidCrossingsValid = testGroup "Test that invalid crossings are detected" 
                                          [testValidCrossingEdgeRef,
                                           testValidCrossingFirstEdgeIncoming,
                                           testValidCrossingRegionsMatch]
      
--Test that a link with a crossing which contains an reference to a non existant edge
-- Is not valid          
--Modify a crossing of trefoil                           
testValidCrossingEdgeRef :: TestTree
testValidCrossingEdgeRef = testCase "The references to edges must be valid" $ False @?= isValidCrossings invalidTrefoil
  where invalidTrefoil = trefoil {crossings = newCrossings}
        newCrossings = IMap.insert 1 (Crossing 3 6 4 10) 
                     . IMap.insert 2 (Crossing 10 4 2 5) $  crossings trefoil 
        --Alter by changing crossings to change the edge index 1 to the invalid edge index 10.

--Test that a link with a crossing whose first edge is not incoming is not valid
--Modify a crossing of trefoil
testValidCrossingFirstEdgeIncoming :: TestTree
testValidCrossingFirstEdgeIncoming = testCase "The first edge must be incoming" $ False @?= isValidCrossings invalidTrefoil
  where invalidTrefoil = trefoil {crossings = newCrossings}
        newCrossings = IMap.insert 1 (Crossing 4 10 3 6) $  crossings trefoil 
        --Alter by changing rotating the 1st crossing 180 degrees so the first edge is outgoing

--Test that a link with a crossing with 2 neighbouring edges have matching regions
--Modify an edge of trefoil to switch the regions around
testValidCrossingRegionsMatch :: TestTree
testValidCrossingRegionsMatch = testCase "The regions of neighbouring edges must match" $ False @?= isValidCrossings invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.insert 3 (Edge 3 1 1 4 2) $  edges trefoil 
        --Swap from right to left regions on edge 3

        
--Tests that invalid edges are detected
testValidEdgesValid :: TestTree
testValidEdgesValid = testGroup "Test that invalid edges are detected" 
                                          [testValidEdgeCrossingRef,
                                           testValidEdgeRegionRef,
                                           testValidEdgeComponentRef,
                                           testValidEdgeRegionMeets,
                                           testValidEdgeRegionsDistinct,
                                           testValidEdgeComponentPath,
                                           testValidEdgeComponentPathContains]
                                           
--Test that a link with a edge which contains an reference to a non existant crossing
-- Is not valid          
--Modify an edge of trefoil                           
testValidEdgeCrossingRef :: TestTree
testValidEdgeCrossingRef = testCase "The references to crossings must be valid" $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.insert 1 (Edge 10 2 2 1 2) 
                 . IMap.insert 3 (Edge 3 10 4 1 2) 
                 . IMap.insert 4 (Edge 10 2 5 2 2)
                 . IMap.insert 6 (Edge 3 10 5 4 2) $ edges trefoil 
        --Alter by changing edges to change the crossing index 1 to the invalid edge index 10.

--Test that a link with a edge which contains an reference to a non existant region
-- Is not valid          
--Modify an edge of trefoil                           
testValidEdgeRegionRef :: TestTree
testValidEdgeRegionRef = testCase "The references to regions must be valid" $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.insert 1 (Edge 1 2 20 1 2) 
                 . IMap.insert 4 (Edge 1 2 5 20 2) $ edges trefoil 
        --Alter by changing edges to change the crossing index 2 to the invalid edge index 20.

        
--Test that a link with a edge which contains an reference to a non existant component
-- Is not valid          
--Modify an edge of trefoil                           
testValidEdgeComponentRef :: TestTree
testValidEdgeComponentRef = testCase "The references to component must be valid" $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.fromList [(1, Edge 1 2 2 1 8),
                                  (2, Edge 2 3 5 3 8),
                                  (3, Edge 3 1 4 1 8),
                                  (4, Edge 1 2 5 2 8),
                                  (5, Edge 2 3 3 1 8),
                                  (6, Edge 3 1 5 4 8)]
        --Alter by changing edges to change the component index 2 to the invalid edge index 8.

--Test that a link with an edge which meets a region should be in the boundary of the region it meets
--Modify a region of trefoil
testValidEdgeRegionMeets :: TestTree
testValidEdgeRegionMeets = testCase "The region an edge meets should have the edge in it's boundary"
                                             $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {regions = newRegions}
        newRegions = IMap.insert 2 newRegion $ regions trefoil
        newRegion = Region (Set.singleton (Set.singleton 4)) Set.empty
        --Alter a region removing a bounding edge.
        
--Test that a link with an edge should have both regions distinct
--Modify an edge of trefoil                           
testValidEdgeRegionsDistinct :: TestTree
testValidEdgeRegionsDistinct = testCase "Both regions meeting an edge should be distinct" 
                                    $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.insert 1 (Edge 1 2 2 2 2) $ edges trefoil
        --Alter an edge to make both regions the same.
        
--Test that a link with edge should have it's component be a path component
--Modify the component of a trefoil
testValidEdgeComponentPath :: TestTree
testValidEdgeComponentPath = testCase "Component should be a path component" 
                                     $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {components = newComponents}
        newComponents = IMap.fromList [(2,UnknottedComponent 2)]
        --Make a component into an unknotted component
   
--Test that a link with edge should have it's component should have the edge in it's path
--Modify the component of a trefoil
testValidEdgeComponentPathContains :: TestTree
testValidEdgeComponentPathContains = testCase "Component should be a have edge in path" 
                                           $ False @?= isValidEdges invalidTrefoil
  where invalidTrefoil = trefoil {components = newComponents}
        newComponents = IMap.fromList [(2,PathComponent [2,3,4,5,6])]
        --Make a remove edge 1 from path component 
        
--Tests that invalid unknots are detected
testValidUnknotsValid :: TestTree
testValidUnknotsValid = testGroup "Test that invalid unknots are detected" 
                                          [testValidUnknotRegionRef,
                                           testValidUnknotComponentRef,
                                           testValidUnknotRegionMeets,
                                           testValidUnknotRegionsDistinct,
                                           testValidUnknotComponentUnknot,
                                           testValidUnknotComponentUnknotIndex
                                          ]
                                         
--Test that a link with a unknot which contains an reference to a non existant region
-- Is not valid          
--Modify an unknot of unknot                           
testValidUnknotRegionRef :: TestTree
testValidUnknotRegionRef = testCase "The references to regions must be valid" 
                                               $ False @?= isValidUnknots invalidTrefoil
  where invalidTrefoil = unknot {unknots = newUnknots}
        newUnknots = IMap.fromList [(1,Unknot 1 10 2)]
        --Alter by changing unknot so reference to region 3 is non existant region 10
        
--Test that a link with a unknot which contains an reference to a non existant component
-- Is not valid          
--Modify an unknot of unknot                           
testValidUnknotComponentRef :: TestTree
testValidUnknotComponentRef = testCase "The references to regions must be valid" 
                                               $ False @?= isValidUnknots invalidTrefoil
  where invalidTrefoil = unknot {unknots = newUnknots}
        newUnknots = IMap.fromList [(1,Unknot 1 3 10)]
        --Alter by changing unknot so reference to component 2 is non existant component 10
        
--Test that a link with an unknot which meets a region should be in the boundary of the region it meets
--Modify a region of unknot
testValidUnknotRegionMeets :: TestTree
testValidUnknotRegionMeets = testCase "The region an edge meets should have the edge in it's boundary"
                                             $ False @?= isValidUnknots invalidUnknot
  where invalidUnknot = unknot {regions = newRegions}
        newRegions = IMap.insert 1 newRegion $ regions unknot
        newRegion = Region Set.empty Set.empty
        --Alter a region removing a bounding unknot.
        
--Test that a link with an unknot should have both regions distinct
--Modify an edge of unknot                         
testValidUnknotRegionsDistinct :: TestTree
testValidUnknotRegionsDistinct = testCase "Both regions meeting an unknot should be distinct" 
                                    $ False @?= isValidUnknots invalidUnknot
  where invalidUnknot = unknot {unknots = newUnknots}
        newUnknots = IMap.fromList [(1,Unknot 3 3 2)]
        --Alter an unknot to make both regions the same.
        
--Test that a link with unknot should have it's component be a unknotted component
--Modify the component of a unknot
testValidUnknotComponentUnknot :: TestTree
testValidUnknotComponentUnknot = testCase "Component should be a unknotted component" 
                                         $ False @?= isValidUnknots invalidUnknot
  where invalidUnknot = unknot {components = newComponents}
        newComponents = IMap.fromList [(2,PathComponent [1])]
        --Make a component into an path component
   
--Test that a link with unknot should have it's unknot component indexing it
--Modify the component of a unknot
testValidUnknotComponentUnknotIndex :: TestTree
testValidUnknotComponentUnknotIndex = testCase "Component should index the unknot" 
                                           $ False @?= isValidUnknots invalidUnknot
  where invalidUnknot = unknot {components = newComponents}
        newComponents = IMap.fromList [(2,UnknottedComponent 10)]
        --Make a component index different unknot
        
--Tests that invalid unknots are detected
testValidRegionsValid :: TestTree
testValidRegionsValid = testGroup "Test that invalid unknots are detected" 
                                          [testValidRegionEdgeRef,
                                           testValidRegionUnknotRef,
                                           testValidRegionMeetsUnknot,
                                           testValidRegionMeetsEdge,
                                           testValidRegionBoundariesDistinct
                                          ]
                                          
--Test that a link with a region which contains an reference to a non existant unknot
-- Is not valid          
--Modify an region of unknot                           
testValidRegionUnknotRef :: TestTree
testValidRegionUnknotRef = testCase "The references to unknots must be valid" 
                                               $ False @?= isValidRegions invalidTrefoil
  where invalidTrefoil = unknot {regions = newRegions}
        newRegions = IMap.fromList [(1,Region Set.empty (Set.singleton 10))]
        --Alter by changing unknot so reference to unknot 1 is non existant unknot 10
        
--Test that a link with a region which contains an reference to a non existant edge
-- Is not valid          
--Modify an region of trefoil                           
testValidRegionEdgeRef :: TestTree
testValidRegionEdgeRef = testCase "The references to regions must be valid" 
                                               $ False @?= isValidRegions invalidTrefoil
  where invalidTrefoil = trefoil {regions = newRegions}
        newRegions = IMap.insert 1 (Region (Set.singleton (Set.fromList [10,3,5])) Set.empty)
                   . IMap.insert 2 (Region (Set.singleton (Set.fromList [10,4])) Set.empty) 
                   $ regions trefoil
        --Alter by changing region so reference to edge 1 is non existant edge 10
        
--Test that a link with an region which meets a unknot should be in the boundary of the unknot
--Modify a unknot of unknot
testValidRegionMeetsUnknot :: TestTree
testValidRegionMeetsUnknot = testCase "Should be in boundary of unknot it meets"
                                                $ False @?= isValidRegions invalidTrefoil
  where invalidTrefoil = unknot {unknots = newUnknots}
        newUnknots = IMap.fromList [(1,Unknot 3 3 2)]
        --Alter remove region 1 from unknot boundary
  
--Test that a link with an region which meets a unknot should be in the boundary of the unknot
--Modify a edge of trefoil
testValidRegionMeetsEdge :: TestTree
testValidRegionMeetsEdge = testCase "Should be in boundary of edge it meets"
                                                $ False @?= isValidRegions invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.insert 1 (Edge 1 2 4 1 2)
                 . IMap.insert 4 (Edge 1 2 5 4 2) $ edges trefoil
        --Alter change region 2 from edge boundary to region 4

--Test that a link with a region with non distinct boundaries is invalid.
--Modify region of trefoil
testValidRegionBoundariesDistinct :: TestTree
testValidRegionBoundariesDistinct = testCase "Boundaries of a region should be distinct"
                                                $ False @?= isValidRegions invalidTrefoil
  where invalidTrefoil = trefoil {regions = newRegions}
        newRegions = IMap.insert 1 (Region (Set.fromList [Set.fromList [1,3],
                                                          Set.fromList [1,5]
                                                         ]) Set.empty)
                   $ regions trefoil
        --Alter region 1 to have non-distinct boundaries
        
--Test that invalid components are detected
testValidComponentsValid :: TestTree
testValidComponentsValid = testGroup "Test that invalid components are detected" 
                                                 [testValidComponentsUnknotRef,
                                                  testValidComponentsEdgeRef,
                                                  testValidComponentsUnknotComp,
                                                  testValidComponentsPathComp,
                                                  testValidComponentsSequentialEdges
                                                 ]
--Test that a link with a unknotted which contains an reference to a non existant unknot
-- Is not valid          
--Add a component to trefoil                                                   
testValidComponentsUnknotRef :: TestTree
testValidComponentsUnknotRef = testCase "The references to unknots must be valid" 
                                               $ False @?= isValidComponents invalidTrefoil
  where invalidTrefoil = unknot {components = newComponents}
        newComponents = IMap.insert 10 (UnknottedComponent 10) $ components trefoil
        --Alter by adding a unknotted compoent with non existant unknot  
        
--Test that a link with a path which contains an reference to a non existant edge
-- Is not valid          
--Add a component to trefoil                                                   
testValidComponentsEdgeRef :: TestTree
testValidComponentsEdgeRef = testCase "The references to edges must be valid" 
                                               $ False @?= isValidComponents invalidTrefoil
  where invalidTrefoil = unknot {components = newComponents}
        newComponents = IMap.fromList [(2,PathComponent [1,2,3,4,5,6,10])]
        --Alter by adding a non existing edge 10 to path                        
      
--Test that any unknot of a component of a link should index this component  
--Alter unknot 
testValidComponentsUnknotComp :: TestTree
testValidComponentsUnknotComp = testCase "Should be the component of indexed unknot"
                                               $ False @?= isValidComponents invalidTrefoil
  where invalidTrefoil = unknot {unknots = newUnknots}
        newUnknots = IMap.fromList [(1,Unknot 1 3 10)]
        --Make indexed component of unknot not index component 
        
--Test that any edge of a path component of a link should index this component  
--Alter trefoil
testValidComponentsPathComp :: TestTree
testValidComponentsPathComp = testCase "Should be the component of indexed edge"
                                               $ False @?= isValidComponents invalidTrefoil
  where invalidTrefoil = trefoil {edges = newEdges}
        newEdges = IMap.fromList [(1, Edge 1 2 2 1 10),
                         (2, Edge 2 3 5 3 10),
                         (3, Edge 3 1 4 1 10),
                         (4, Edge 1 2 5 2 10),
                         (5, Edge 2 3 3 1 10),
                         (6, Edge 3 1 5 4 10)
                        ]
        --Make indexed component of edge not index component 
        
--Test that any link with a component which is not sequential is not valid
--Alter trefoil
testValidComponentsSequentialEdges :: TestTree 
testValidComponentsSequentialEdges = testCase "A paths edges should be valid" 
                                               $ False @?= isValidComponents invalidTrefoil
  where invalidTrefoil = unknot {components = newComponents}
        newComponents = IMap.fromList [(2,PathComponent [1,5,4,3,2,6])]  
        --Reorder the edges in the path of trefoil.