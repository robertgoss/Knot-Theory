{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Test.LinkDiagram.TestingInterface where

import Test.SmallCheck.Series

import qualified Data.Set as Set

import Control.Monad(liftM)
import Control.Applicative

import LinkDiagram.Crossing
import LinkDiagram.Edge
import LinkDiagram.Region
import LinkDiagram.Unknot
import LinkDiagram.Component

--Testing interfaces for using smallcheck and quickcheck without either orphan instances
-- Or defining these instances in main library
-- Also allows interfaces which depend on tables without circular dependencies

--Interfaces for crossing

newtype TestingCrossing edgeIndex = TCrossing (Crossing edgeIndex) deriving(Eq,Ord,Show)

tCrossing :: TestingCrossing edgeIndex -> Crossing edgeIndex
tCrossing (TCrossing c) = c
  
--Instance for small check.
--Use combinators to join together constructors
-- We use an alteration as each of the parameters in the constructors should be distinct
--
-- And lift to testing.
instance (Distinct edgeIndex, Serial m edgeIndex) => Serial m (TestingCrossing edgeIndex) where
    series = liftM TCrossing $ consDistinct4 Crossing
                               \/ consDistinct3 LoopCrossingTL
                               \/ consDistinct3 LoopCrossingTR
                               \/ consDistinct3 LoopCrossingBL
                               \/ consDistinct3 LoopCrossingBR
                               \/ consDistinct2 DoubleLoopL
                               \/ consDistinct2 DoubleLoopR
                               
--Interface for edge

newtype TestingEdge vertexIndex regionIndex componentIndex
                                = TEdge (Edge vertexIndex regionIndex componentIndex) deriving(Eq,Ord,Show)

tEdge :: TestingEdge vertexIndex regionIndex componentIndex -> Edge vertexIndex regionIndex componentIndex
tEdge (TEdge e) = e

--Instance for small check
--use cons to wrap the construct
--Alter the constructor as the 2 regions need to be distinct#
--And lift to testing

instance (Distinct regionIndex, Serial m vertexIndex, Serial m regionIndex, Serial m componentIndex) 
          => Serial m (TestingEdge vertexIndex regionIndex componentIndex) where
    series = liftM TEdge $ cons5 distinctEdgeConstruct
        where distinctEdgeConstruct edge1 edge2 region1 region2 
                           = Edge edge1 edge2 region1 distinctRegion
                  where distinctRegion = if region1 /= region2 then region2 else newElement [region1]
                  
--Interface for unknot

newtype TestingUnknot regionIndex componentIndex
                                = TUnknot (Unknot regionIndex componentIndex) deriving(Eq,Ord,Show)

tUnknot :: TestingUnknot regionIndex componentIndex -> Unknot regionIndex componentIndex
tUnknot (TUnknot u) = u

--Instance for small check
--use cons to wrap the construct
--Alter the constructor as the 2 regions need to be distinct
--And lift to testing

instance (Distinct regionIndex, Serial m regionIndex, Serial m componentIndex) 
          => Serial m (TestingUnknot regionIndex componentIndex) where
    series = liftM TUnknot $ cons3 distinctUnknotConstruct
        where distinctUnknotConstruct region1 region2 
                           = Unknot region1 distinctRegion
                  where distinctRegion = if region1 /= region2 then region2 else newElement [region1]  
     
--Interface for region
newtype TestingRegion edgeIndex unknotIndex
                                = TRegion (Region edgeIndex unknotIndex) deriving(Eq,Ord,Show)

tRegion :: TestingRegion edgeIndex unknotIndex -> Region edgeIndex unknotIndex
tRegion (TRegion r) = r

--Smallcheck instance for region
--Use cons to wrap the constructor 
--Alter the constructor to use lists
--The edge boundaries should be distinct.
--And lift to testing

instance (Ord edgeIndex, Ord unknotIndex, Serial m edgeIndex, Serial m unknotIndex) 
          => Serial m (TestingRegion edgeIndex unknotIndex) where
   series = liftM TRegion $ cons2 listRegionConstructor
     where listRegionConstructor listEdges listUnknots = Region setEdgeSets setUnknots
             where setUnknots = Set.fromList listUnknots --Covert list to set
                   edgeSets = map Set.fromList listEdges -- List of sets of edges
                   distinctEdgeSets = snd $ foldl distinctSets (Set.empty, []) edgeSets -- make each of the sets distinct
                   --Keep a running union of sets and for each set take difference with this running union
                   distinctSets (union, partialSets) nextSet = (newUnion, nextDistinctSet : partialSets)
                      where newUnion = Set.union union nextSet
                            nextDistinctSet = Set.difference nextSet union
                   setEdgeSets = Set.delete Set.empty (Set.fromList distinctEdgeSets) --Convert to set and remove empty set
                            
--Interface for region
newtype TestingComponent unknotIndex edgeIndex
                                = TComponent (Component unknotIndex edgeIndex) deriving(Eq,Ord,Show)

tComponent :: TestingComponent unknotIndex edgeIndex -> Component unknotIndex edgeIndex
tComponent (TComponent c) = c               

--Smallcheck instance for component
--Use cons to wrap the constructor 
--Alter constructor so path component is irredundant
--And lift to testing
instance (Ord edgeIndex, Serial m edgeIndex, Serial m unknotIndex) 
          => Serial m (TestingComponent unknotIndex edgeIndex) where
   series = liftM TComponent $ cons1 UnknottedComponent
                             \/ cons1 pathComponentIrredundant
       where pathComponentIrredundant = PathComponent . Set.toList . Set.fromList
                   
--Add an extra cons for 5 element constructor
cons5 ::(Serial m a, Serial m b, Serial m c, Serial m d,Serial m e) 
        => (a -> b -> c -> d -> e -> f) -> Series m f
cons5 f = decDepth $  f <$> series
                        <~> series
                        <~> series
                        <~> series
                        <~> series
           
--A class of elements where we can make a distinct extention      
class (Eq a) => Distinct a where
  --Given a (non-zero) list of elements of a construct an element b
  -- such that (newElement xs) `notElem` xs.
  newElement :: [a] -> a
  
--Adding a basic instance of distinct
-- Which is Int as that is the primary index we will use.
instance Distinct Int where
  newElement [] = 0 -- Choose any element
  newElement xs = 1 + maximum xs --New element cannot be in list 
  
--Given a list of elements construct a new list of the same length
-- Such that all elements are distinct.
distinctElements :: (Distinct a) => [a] -> [a]
distinctElements = distinctElements' []
  --Construct new list adding one element from list at a time
  -- If element is distinct from rest then add it.
  -- Else add a new element using distinct class.
  where distinctElements' distinct [] = distinct
        distinctElements' distinct (x:xs) | x `notElem` distinct = distinctElements' (x:distinct) xs
                                          | otherwise = distinctElements' (newElement distinct: distinct) xs
                                         
--Expand cons to alter constructors so all the arguments are made distinct
consDistinct4 :: (Serial m a, Distinct a) => (a -> a -> a -> a -> b) -> Series m b
consDistinct4 constructor = cons4 distinctConstructor
  where distinctConstructor a1 a2 a3 a4 = constructor d1 d2 d3 d4
          where [d1,d2,d3,d4] = distinctElements [a1,a2,a3,a4]
        
consDistinct3 :: (Serial m a, Distinct a) => (a -> a -> a -> b) -> Series m b
consDistinct3 constructor = cons3 distinctConstructor
  where distinctConstructor a1 a2 a3 = constructor d1 d2 d3
          where [d1,d2,d3] = distinctElements [a1,a2,a3]
        
consDistinct2 :: (Serial m a, Distinct a) => (a -> a -> b) -> Series m b
consDistinct2 constructor = cons2 distinctConstructor
  where distinctConstructor a1 a2 = constructor d1 d2
          where [d1,d2] = distinctElements [a1,a2]