{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Test.LinkDiagram.TestingInterface where

import Test.SmallCheck.Series

import Control.Monad(liftM)

import LinkDiagram.Crossing

--Testing interfaces for using smallcheck and quickcheck without either orphan instances
-- Or defining these instances in main library
-- Also allows interfaces which depend on tables without circular dependencies

--Interfaces for crossing

newtype TestingCrossing edgeIndex = TCrossing (Crossing edgeIndex) deriving(Eq,Ord,Show)

tCrossing :: TestingCrossing edgeIndex -> Crossing edgeIndex
tCrossing (TCrossing c) = c
  
--Instance for small check.
--Use combinators to join together constructors
-- And lift to testing.
instance (Serial m edgeIndex) => Serial m (TestingCrossing edgeIndex) where
    series = liftM TCrossing $ cons4 Crossing
                               \/ cons3 LoopCrossingTL
                               \/ cons3 LoopCrossingTR
                               \/ cons3 LoopCrossingBL
                               \/ cons3 LoopCrossingBR
                               \/ cons2 DoubleLoopL
                               \/ cons2 DoubleLoopR

