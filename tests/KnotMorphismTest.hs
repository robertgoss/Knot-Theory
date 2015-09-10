module KnotMorphismTest where

import KnotDiagram
import KnotMorphism
import RolfsonTable

import TestingInterface

import Data.Maybe(fromJust)

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

--Test Knot morphisms module
testKnotMorphism :: TestTree
testKnotMorphism = testGroup "Test Knot Diagram" 
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
testKDMIdentityIsIdeompotent = SC.testProperty "Identity is ideompotent" ideompotentTest
  where ideompotentTest knot = ident `composeUnsafe` ident == ident
          where ident = identity $ tKnotDiagram knot :: KnotDiagramIsomorphism
        
--In this test we are just using the same knot as base to get something we can compose.
--Eventually we want to create a system of coherent maps!!
testKDMCompositionIsAssoc :: TestTree
testKDMCompositionIsAssoc = SC.testProperty "Composition is associative" assocTest
  where assocTest tKnot = (f `composeUnsafe` g) `composeUnsafe` h == f `composeUnsafe` (g `composeUnsafe` h)
           where f = cycle automorphisms !! 1 -- Valid as aut never empty
                 g = cycle automorphisms !! 2 -- Valid as aut never empty
                 h = cycle automorphisms !! 3 -- Valid as aut never empty
                 automorphisms = knotDiagramsAllIsomorphisms knot knot
                 knot = tKnotDiagram tKnot
       

--Test that knotDiagramIsomorphisms obey properties of an isomorphisms
testKnotDiagramIsomorphisms :: TestTree     
testKnotDiagramIsomorphisms = testGroup "Test knotDiagramIsomorphism Isomorphism properties"
                                 [testKDIInvertIsLeftInverse,
                                  testKDIInvertIsRightInverse,
                                  testKDIInvertExchangeSourceTarget,
                                  testKDIInvertExchangeTargetSource
                                 ]
                                 
testKDIInvertIsLeftInverse :: TestTree
testKDIInvertIsLeftInverse = SC.testProperty "Invert is left inverse" leftInverseTest
   where leftInverseTest tIso = invert iso `composeUnsafe` iso == identity (source iso)
           where iso = tKDIsomorphism tIso

testKDIInvertIsRightInverse :: TestTree
testKDIInvertIsRightInverse = SC.testProperty "Invert is right inverse" rightInverseTest
   where rightInverseTest tIso = iso `composeUnsafe` invert iso == identity (target iso)
           where iso = tKDIsomorphism tIso

testKDIInvertExchangeSourceTarget :: TestTree
testKDIInvertExchangeSourceTarget = SC.testProperty "Invert exchanges source and target" stTest
   where stTest tIso = source iso ==  target (invert iso)
           where iso = tKDIsomorphism tIso
           
testKDIInvertExchangeTargetSource :: TestTree
testKDIInvertExchangeTargetSource = SC.testProperty "Invert exchanges target and source" tsTest
   where tsTest tIso = target iso ==  source (invert iso)
           where iso = tKDIsomorphism tIso


--Test the properties of the function to find all isomorphisms between 2 knots
testKnotDiagramsAllIsomorphisms :: TestTree
testKnotDiagramsAllIsomorphisms = testGroup "Test knotDiagramIsomorphism Isomorphism properties"
                                 [testKDAIallKnotsHaveSelfIsomorpism,
                                  testKDAITrefoilSelfIsomorphism,
                                  testKDAIRolfsonKnotOnlyIsoToSelf,
                                  testKDAITerfoilNotIsoToReverse,
                                  testKDAITerfoilNotIsoToMirror,
                                  testKDAITerfoilToOtherTrefoil
                                 ]
                                 
   
testKDAIallKnotsHaveSelfIsomorpism :: TestTree                              
testKDAIallKnotsHaveSelfIsomorpism = SC.testProperty "All knot have self isomorphisms" $ 
                                       not . null . knotDiagramAllAutomorphisms . tKnotDiagram

testKDAITrefoilSelfIsomorphism :: TestTree
testKDAITrefoilSelfIsomorphism = testCase "Check diagram automorphisms of trefoil" $ 
                                    3 @?= length (knotDiagramAllAutomorphisms trefoil)
    where trefoil = fromJust $ fromPlanarDiagram [(4,2,5,1),(2,6,3,5),(6,4,1,3)]

testKDAIRolfsonKnotOnlyIsoToSelf :: TestTree
testKDAIRolfsonKnotOnlyIsoToSelf = SC.testProperty "Rolfson knots are only diagram isomorpic to themselves"
                                         testRolfsonNonIso
   where testRolfsonNonIso tr1 tr2 = (r1 == r2) == knotDiagramsIsomorphic k1 k2
            where r1 = tRolfson tr1
                  r2 = tRolfson tr2
                  k1 = knotDiagram r1
                  k2 = knotDiagram r2

testKDAITerfoilNotIsoToReverse:: TestTree
testKDAITerfoilNotIsoToReverse = testCase "Check trefoil is isomorphic to its reverse" $ 
                                    True @?= knotDiagramsIsomorphic trefoil trefoilReverse
    where trefoil = fromJust $ fromPlanarDiagram [(4,2,5,1),(2,6,3,5),(6,4,1,3)]
          trefoilReverse = fromJust $ fromPlanarDiagram [(3,1,4,6),(5,3,6,2),(1,5,2,4)]

testKDAITerfoilNotIsoToMirror :: TestTree
testKDAITerfoilNotIsoToMirror = testCase "Check trefoil is not isomorphic to its mirror" $ 
                                    False @?= knotDiagramsIsomorphic trefoil trefoilMirror
    where trefoil = fromJust $ fromPlanarDiagram [(4,2,5,1),(2,6,3,5),(6,4,1,3)]
          trefoilMirror = fromJust $ fromPlanarDiagram [(1,4,2,5),(5,2,6,3),(3,6,4,1)]

testKDAITerfoilToOtherTrefoil :: TestTree
testKDAITerfoilToOtherTrefoil = testCase "Check 2 differently labeled trefoils are isomorphic" $ 
                                    True @?= knotDiagramsIsomorphic trefoil trefoilOther
    where trefoil = fromJust $ fromPlanarDiagram [(4,2,5,1),(2,6,3,5),(6,4,1,3)]
          trefoilOther = fromJust $ fromPlanarDiagram [(14,2,105,-1),(2,6,3,105),(6,14,-1,3)]
