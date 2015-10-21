{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Test.TestingInterface where

import Test.SmallCheck.Series

import Control.Monad(liftM)

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
     
-- Interfaces for knot diagram

newtype TestingKnotDiagram = TKnotDiagram KnotDiagram deriving(Eq,Ord,Show)

tKnotDiagram :: TestingKnotDiagram -> KnotDiagram
tKnotDiagram (TKnotDiagram k) = k

--Instance for smallcheck
-- For now just return the Rolfson knot
instance (Monad m) => Serial m TestingKnotDiagram where
    series = liftM (TKnotDiagram . knotDiagram . tRolfson) series
    
--Instance for knot diagram isomorphism


newtype TestingKDIsomorphism = TKDIsomorphism KnotDiagramIsomorphism deriving(Eq,Ord,Show)

tKDIsomorphism :: TestingKDIsomorphism -> KnotDiagramIsomorphism
tKDIsomorphism (TKDIsomorphism iso) = iso
  
--Instance for small check.
--For a series construct a take a knot and construct an
-- isomorphism by mapping each vertex etc to its ordered index.
instance (Monad m) => Serial m TestingKDIsomorphism where
    series = liftM (TKDIsomorphism . toOrderedMorphism . tKnotDiagram) series
