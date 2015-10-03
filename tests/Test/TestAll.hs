module Main where
import Test.KnotDiagramTest
import Test.KnotMorphismTest

import Test.LinkDiagram.CrossingTest
import Test.LinkDiagram.RegionTest
import Test.LinkDiagram.UnknotTest
import Test.LinkDiagram.EdgeTest
import Test.LinkDiagram.ComponentTest

import Test.Tasty

main :: IO()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [testKnotDiagram,
                           testKnotMorphism,
                           testCrossing,
                           testRegion,
                           testUnknot,
                           testEdge,
                           testComponent
                          ]
