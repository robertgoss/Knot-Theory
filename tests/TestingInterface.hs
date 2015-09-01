{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module TestingInterface where

import Test.SmallCheck.Series

import KnotDiagram
import RolfsonTable
import KnotMorphism


--Testing interfaces for using smallcheck and quickcheck without either orphan instances 
-- Or defining these instances in main library
-- Also allows interfaces which depend on tables without circular dependencies

   
--Interfaces for rolfson knot

newtype TestingRolfson = TRolfson RolfsonKnot deriving(Eq,Ord,Show)

tRolfson :: TestingRolfson -> RolfsonKnot
tRolfson (TRolfson r) = r
  
--Instance for small check.
instance (Monad m) => Serial m TestingRolfson where
    series = generate $ \d -> map TRolfson $ concatMap rolfsonKnotsCrossings [1..d] 