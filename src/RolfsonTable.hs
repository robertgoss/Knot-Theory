{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RolfsonTable where

import KnotDiagram

import Data.Maybe(fromJust)


--Note in this it is assumed that the Rolfson table is truncated to just the 
-- first 7 crossings this is done for space.

--Note intend to move this to a proper table interface ie a cache or IO lookup.
--Also intend to have a more generic knot type when can see better how the system works

--Basic data structure to hold any possible knot in the Rolfson table
-- The constructor is hidden so only valid knots in the table can be created

data RolfsonKnot = Rolfson Int Int deriving(Eq,Ord,Show)

--In this we check only valid knots may be created 
-- For now we only deal with knots upto 7 crossings
rolfson :: Int -> Int -> Maybe RolfsonKnot
rolfson crossings' index | crossings' < 0 = Nothing --Must have a positive number of crossings
                        | crossings' > 7 = Nothing --We truncate the Rolfson table for now
                        | index <= 0 = Nothing -- Rolfson knots are 1 indexed.
                        | index > maxIndices !! crossings' = Nothing -- Have exceeded maximum index with given crossing number
                        | otherwise = Just $ Rolfson crossings' index -- Valid knot in table
   where
   
-- Maximum indices in table for each crossing number from 0
maxIndices :: [Int]
maxIndices = [1,0,0,1,1,2,3,7]   
 
--Get all the Rolfson knots in the rolfson table with the given number of crossings
rolfsonKnotsCrossings :: Int -> [RolfsonKnot]
rolfsonKnotsCrossings d | d < 0 = []
                        | d > 7 = []
                        | otherwise = map (Rolfson d) [1..maxIndex]
  where maxIndex = maxIndices !! d
   
--Get all the Rolfson knots in the table
rolfsonKnots :: [RolfsonKnot]
rolfsonKnots = concatMap rolfsonKnotsCrossings [0..7]

--Convert a Rolfson knot to the minimum crossing diagram given in the table.
knotDiagram :: RolfsonKnot -> KnotDiagram
--We can use fromJust as these are pretested.
knotDiagram = fromJust . fromPlanarDiagram . planarDiagram

--Convert a Rolfson knot to a planar diagram which generates
-- the minimum crossing diagram given in the table.
-- Note big switch statement is used here -- move this to a database style lookup some day
planarDiagram :: RolfsonKnot -> PlanarDiagram
planarDiagram (Rolfson 0 1) = []
planarDiagram (Rolfson 3 1) = [(4,2,5,1),(2,6,3,5),(6,4,1,3)]
planarDiagram (Rolfson 4 1) = [(6,2,7,1),(2,6,3,5),(8,3,1,4),(4,7,5,8)]
planarDiagram (Rolfson 5 1) = [(1,7,2,6),(7,3,8,2),(3,9,4,8),(9,5,10,4),
                               (5,1,6,10)]
planarDiagram (Rolfson 5 2) = [(8,2,9,1),(2,8,3,7),(6,4,7,3),(4,10,5,9),
                               (5,10,6,1)]
planarDiagram (Rolfson 6 1) = [(10,2,11,1),(2,10,3,9),(8,4,9,3),(4,8,5,7),
                               (12,5,1,6),(6,11,7,12)]
planarDiagram (Rolfson 6 2) = [(1,9,2,8),(9,3,10,2),(3,11,4,10),(7,5,8,4),
                               (5,12,6,1),(11,6,12,7)]
planarDiagram (Rolfson 6 3) = [(10,2,11,1),(2,5,3,6),(8,3,9,4),(4,9,5,10),
                               (6,12,7,11),(12,8,1,7)]
planarDiagram (Rolfson 7 1) = [(1,9,2,8),(9,3,10,2),(3,11,4,10),(11,5,12,4),
                               (5,13,6,12),(13,7,14,6),(7,1,8,14)]
planarDiagram (Rolfson 7 2) = [(12,2,13,1),(2,12,3,11),(10,4,11,3),(4,10,5,9),
                               (8,6,9,5),(6,14,7,13),(14,8,1,7)]
planarDiagram (Rolfson 7 3) = [(8,1,9,2),(2,9,3,10),(10,3,11,4),(4,11,5,12),
                               (12,7,13,8),(6,13,7,14),(14,5,1,6)]
planarDiagram (Rolfson 7 4) = [(1,8,2,9),(7,2,8,3),(3,12,4,13),(11,4,12,5),
                               (5,10,6,11),(13,6,14,7),(9,14,10,1)]
planarDiagram (Rolfson 7 5) = [(1,11,2,10),(11,3,12,2),(3,13,4,12),(9,5,10,4),
                               (5,9,6,8),(13,7,14,6),(7,1,8,14)]
planarDiagram (Rolfson 7 6) = [(1,12,2,13),(11,2,12,3),(3,9,4,8),(9,5,10,4),
                               (5,1,6,14),(13,7,14,6),(7,11,8,10)]
planarDiagram (Rolfson 7 7) = [(1,8,2,9),(7,2,8,3),(3,12,4,13),(11,4,12,5),
                               (5,1,6,14),(9,7,10,6),(13,11,14,10)]

planarDiagram (Rolfson _ _) = error "Invalid Rolfson knot used" -- A rolfson knot not in table used should be impossible
