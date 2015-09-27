module LinkDiagram where

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

--Helper types to indicate what the various indices refer to.
-- The vertices, edges, unknots, regions and components are of a link are indexed by
type VertexIndex = Int
type EdgeIndex = Int
type UnknotIndex = Int
type RegionIndex = Int
type ComponentIndex = Int


--The type of information associated to a crossing
-- This is split by loops - ie edges that start and end 
-- at this crossing as these are harder to orient in code.
data Crossing = Crossing EdgeIndex EdgeIndex EdgeIndex EdgeIndex
                --Standard crossing with 4 distinct edges oriented clockwise 
                -- From the incoming undercrossing edge
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
        deriving(Eq,Ord,Show)

--The type of information associated to an edge
-- The 2 vertices which the edge is going from / to
-- and the 2 regions that it borders on the left/right according to
-- it's orientation.
data Edge = Edge {
  edgeStartCross :: VertexIndex,
  edgeEndCross :: VertexIndex,
  edgeLeftRegion :: RegionIndex,
  edgeRightRegion :: RegionIndex,
  edgeComponent :: ComponentIndex
}deriving(Eq,Ord,Show)

--The type of information associated to an unknotted component 
-- It is defined by the 2 regions that it bounds and oreinted by which region is on the left / right respectivly
data Unknot = Unknot {
  unknotLeftRegion :: RegionIndex,
  unknotRightRegion :: RegionIndex
} deriving(Eq,Ord,Show)


--The type of information associated to an edge
-- A region is bounded either by a series of polygons formed
-- Either for a set of edges or from an unknot
data Region = Region {
  regionEdges :: Set.Set (Set.Set EdgeIndex),
  regionUnknots :: Set.Set UnknotIndex
} deriving(Eq,Ord,Show)

--The type of a link component this is either an unknotted component edge
-- Or an oriented list of edges 
data Component = UnknottedComponent UnknotIndex
               | Component [EdgeIndex] deriving(Eq,Ord,Show)

--The basic data of a link diagram
-- Associates data to the crossings, edges, unknots, regions and components
-- This may not be a valid link diagram 
-- In this form see LinkDiagram for this
data LinkDiagramData = LinkDiagramData {
  crossings :: IMap.IntMap Crossing,
  edges :: IMap.IntMap Edge,
  unknots :: IMap.IntMap Unknot,
  regions :: IMap.IntMap Region,
  components :: IMap.IntMap Component
} deriving(Eq,Ord,Show)


--The type of a valid link diagram 
-- Wraps the link diagram data only for 
-- the data that constructs a valid link diagram.
--The consitancy conditions are discussed below.
data LinkDiagram = LinkDiagram LinkDiagramData

--Get the underlying link diagram data from a valid link diagram
linkDiagramData (LinkDiagram ldData) = ldData

--Returns if the given link diagram data is consistent and constructs a valid link
--In particular it checks that:
-- 
--These confirm it is a valid link 
-- We further require and required consistency conditions are met for looking up indices
-- Which are that and vertex, edges etc index mentioned in a vertex, edge, etc is in the respective table.
-- Ie the vertices, edges etc refered to by indices always exist
isValidLinkDiagram :: LinkDiagramData -> Bool
isValidLinkDiagram = undefined

linkDiagram :: LinkDiagramData -> Maybe LinkDiagram
linkDiagram ldData | isValidLinkDiagram ldData = Just $ LinkDiagram ldData
                   | otherwise = Nothing
