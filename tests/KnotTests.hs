module Main where
import KnotDiagramTest
import KnotMorphismTest

import Test.Tasty

main :: IO()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [testKnotDiagram,
                           testKnotMorphism
                          ]
