module Test.Main where
import Test.KnotDiagramTest
import Test.KnotMorphismTest

import Test.Tasty

main :: IO()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [testKnotDiagram,
                           testKnotMorphism
                          ]
