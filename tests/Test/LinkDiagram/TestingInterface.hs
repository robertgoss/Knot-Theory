{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
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