module KnotDiagram where

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import Data.Maybe(fromJust,isNothing)

--Helper types to indicate what the various indices refer to
-- The vertices, edges and regions are of a knot are indexed
type VertexIndex = Int
type EdgeIndex = Int
type RegionIndex = Int


--The type of information associated to a crossing
-- This is split by loops - ie edges that start and end 
-- at this crossing as these are harder to orient in code.
data Crossing = Crossing EdgeIndex EdgeIndex EdgeIndex EdgeIndex
                --Standard crossing with 4 distinct edges oriented clockwise 
                -- From the incomming undercrossing edge
                | LoopCrossingTL EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingTR EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingBL EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingBR EdgeIndex EdgeIndex EdgeIndex
                --A crossing with a single loop and in the 
                -- given quadrant labeled with the first edge index
                -- The remaining 2 distinct edges are given clockwise 
                | DoubleLoopL EdgeIndex EdgeIndex
                | DoubleLoopR EdgeIndex EdgeIndex
                -- A crossing that contains 2 loops the first contains
                -- the incomming undercrossing edge right or left
                -- indicate if the outgoing part of this edge if left or right
                
                
--Helper function gets the 4 edges that meet at this crossing as a 4 element list
-- Not done as a tuple for convenience later but may separate out.
crossingEdges :: Crossing -> [EdgeIndex]
crossingEdges (Crossing e1 e2 e3 e4) = [e1,e2,e3,e4]
crossingEdges (LoopCrossingTL loop e1 e2) = [e1,loop,loop,e2]
crossingEdges (LoopCrossingTR loop e1 e2) = [e1,e2,loop,loop]
crossingEdges (LoopCrossingBL loop e1 e2) = [loop,loop,e1,e2]
crossingEdges (LoopCrossingBR loop e1 e2) = [loop,e1,e2,loop]
crossingEdges (DoubleLoopL loop1 loop2) = [loop1,loop1,loop2,loop2]
crossingEdges (DoubleLoopR loop1 loop2) = [loop1,loop2,loop2,loop1]

--Helper function constructs a crossing from a tuple of 4 edgeindices
-- Can fail and return nothing if the loops/ edges given do not (locally)
-- form a valid crossing
crossingFromEdges :: (EdgeIndex,EdgeIndex,EdgeIndex,EdgeIndex) -> Maybe Crossing
crossingFromEdges es@(e1,e2,e3,e4) --Switch on number of loops to save labourous checks in canonical case
    | distinctNum == 4 = Just $ Crossing e1 e2 e3 e4 --All edges distinct
    | distinctNum == 3 = crossingFromSingleLoop es -- We have a single loop
    | distinctNum == 2 = crossingFromDoubleLoop es -- We have a 2 loops
    | otherwise = Nothing -- we have an impossible situation
  where distinctNum = Set.size $ Set.fromList [e1,e2,e3,e4]
        --We have a single loop find which quadrant we are in
        crossingFromSingleLoop (e1,e2,e3,e4)
          | e1==e2 = Just $ LoopCrossingBL e1 e3 e4
          | e2==e3 = Just $ LoopCrossingTL e2 e1 e4
          | e3==e4 = Just $ LoopCrossingTR e3 e1 e2
          | e4==e1 = Just $ LoopCrossingBR e4 e2 e3
          | otherwise = Nothing --An imposible local situation
        --We have a double loop check that they form correctly left or right
        --Check that they both form loops (not 3 of the same type etc)
        crossingFromDoubleLoop (e1,e2,e3,e4)
         | e1==e2 && e3==e4 = Just $ DoubleLoopL e1 e3
         | e1==e4 && e2==e3 = Just $ DoubleLoopR e1 e2
         | otherwise = Nothing -- An imposible local situation
        

--The type of information associated to an edge
-- The 2 vertices which the edge is going from / to
-- and the 2 regions that it borders on the left/right according to
-- it's orientation.
data Edge = Edge VertexIndex VertexIndex RegionIndex RegionIndex

--The type of information associated to an edge
-- The list of edges that border this region organised clockwise.
data Region = Region [EdgeIndex]

--The basic type of a knot diagram
-- Associates data to the crossings, edges and regions. 
data KnotDiagram = KnotDiagram {
  crossings :: IMap.IntMap Crossing,
  edges :: IMap.IntMap Edge,
  regions :: IMap.IntMap Region
}
