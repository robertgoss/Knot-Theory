module KnotMorphismTest where

import KnotMorphism

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

--Test Knot morphisms module
testKnotDiagram :: TestTree
testKnotDiagram = testGroup "Test Knot Diagram" 
                          [testKnotDiagramMorphisms,
                           testKnotDiagramIsomorphisms,
                           testKnotDiagramsAllIsomorphisms
                          ]
                          
--Test that knotDiagramIsomorphisms obey properties of a morphisms
testKnotDiagramMorphisms :: TestTree     
testKnotDiagramMorphisms = testGroup "Test knotDiagramIsomorphism Morphism properties"
                              [testKDMIdentityIsIdeompotent,
                               testKDMCompositionIsAssoc
                              ]

testKDMIdentityIsIdeompotent :: TestTree
testKDMIdentityIsIdeompotent = undefined

testKDMCompositionIsAssoc :: TestTree
testKDMCompositionIsAssoc = undefined

--Test that knotDiagramIsomorphisms obey properties of an isomorphisms
testKnotDiagramIsomorphisms :: TestTree     
testKnotDiagramIsomorphisms = testGroup "Test knotDiagramIsomorphism Isomorphism properties"
                                 [testKDIRevertIsLeftInverse,
                                  testKDIRevertIsRightInverse
                                 ]
                                 
testKDIRevertIsLeftInverse :: TestTree
testKDIRevertIsLeftInverse = undefined

testKDIRevertIsRightInverse :: TestTree
testKDIRevertIsRightInverse = undefined


--Test the properties of the function to find all isomorphisms between 2 knots
testKnotDiagramsAllIsomorphisms :: TestTree
testKnotDiagramsAllIsomorphisms = testGroup "Test knotDiagramIsomorphism Isomorphism properties"
                                 [testKDAIallKnotsHaveSelfIsomorpism,
                                  testKDAITrefoilSelfIsomorphism,
                                  testKDAIRolfsonKnotOnlyIsoToSelf,
                                  testKDAITerfoilNotIsoToReflect,
                                  testKDAITerfoilNotIsoToMirror,
                                  testKDAITerfoilToOtherTrefoil
                                 ]
                                 
   
testKDAIallKnotsHaveSelfIsomorpism :: TestTree                              
testKDAIallKnotsHaveSelfIsomorpism = undefined

testKDAITrefoilSelfIsomorphism :: TestTree
testKDAITrefoilSelfIsomorphism = undefined

testKDAIRolfsonKnotOnlyIsoToSelf :: TestTree
testKDAIRolfsonKnotOnlyIsoToSelf = undefined

testKDAITerfoilNotIsoToReflect :: TestTree
testKDAITerfoilNotIsoToReflect = undefined

testKDAITerfoilNotIsoToMirror :: TestTree
testKDAITerfoilNotIsoToMirror = undefined

testKDAITerfoilToOtherTrefoil :: TestTree
testKDAITerfoilToOtherTrefoil = undefined
