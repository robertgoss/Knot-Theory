module KnotDiagram where

import qualified Data.IntMap as IMap

--Helper types to indicate what the various indices refer to
-- The vertices, edges and regions are of a knot are indexed
type VertexIndex = Int
type EdgeIndex = Int
type RegionIndex = Int

--The type of information associated to a crossing
-- The indices of the 4 edges that meet at this crossing
-- ordered clockwise from the incoming edge which goes under the crossing
data Crossing = Crossing EdgeIndex EdgeIndex EdgeIndex EdgeIndex

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
data KnotDigram = KnotDiagram {
  crossings :: IMap.IntMap Crossing,
  edges :: IMap.IntMap Edge,
  regions :: IMap.IntMap Region
}

