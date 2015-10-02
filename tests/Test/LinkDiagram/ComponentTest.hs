module Test.LinkDiagram.ComponentTest where

import LinkDiagram.Component

import Test.LinkDiagram.TestingInterface

import Test.Tasty
import Test.Tasty.SmallCheck as SC

import qualified Data.Set as Set

--Test Component module
--Reduce the depth to 5 as component expands quickly and this is sufficient.
testComponent :: TestTree
testComponent = localOption (SmallCheckDepth 5) $
                         testGroup "Test Component" [ testSmallcheck ]
                         
--TestSmallSmallcheck
--Make sure all the smallcheck crossings are valid
testSmallcheck :: TestTree
testSmallcheck = testGroup "Test smallcheck instance" [testSmallcheckPathIrredundant]

testSmallcheckPathIrredundant :: TestTree
testSmallcheckPathIrredundant = SC.testProperty "Path coponent should be irredundant" testPathIrredundant'
  where testPathIrredundant' :: TestingComponent Int Int -> Bool
        testPathIrredundant' = testPathIrredundant . tComponent
        testPathIrredundant (UnknottedComponent _) = True -- Only check path components
        testPathIrredundant (PathComponent edges) = length edges == Set.size (Set.fromList edges)

