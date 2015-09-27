module LinkDiagram.Crossing where

--The type of information associated to a crossing in terms of the indices of the edges
-- This takes into account loops, it edges that start and end at the same crossing 
-- at this crossing as these are harder to orient in code.
--Deals with the local situation at the edges
data Crossing edgeIndex 
              = Crossing edgeIndex edgeIndex edgeIndex edgeIndex
                --Standard crossing with 4 distinct edges oriented clockwise 
                -- From the incoming undercrossing edge
                | LoopCrossingTL edgeIndex edgeIndex edgeIndex
                | LoopCrossingTR edgeIndex edgeIndex edgeIndex
                | LoopCrossingBL edgeIndex edgeIndex edgeIndex
                | LoopCrossingBR edgeIndex edgeIndex edgeIndex
                --A crossing with a single loop and in the 
                -- given quadrant labeled with the first edge index
                -- The remaining 2 distinct edges are given clockwise 
                | DoubleLoopL edgeIndex edgeIndex
                | DoubleLoopR edgeIndex edgeIndex
                -- A crossing that contains 2 loops the first contains
                -- the incomming undercrossing edge right or left
                -- indicate if the outgoing part of this edge if left or right
        deriving(Eq,Ord,Show)
