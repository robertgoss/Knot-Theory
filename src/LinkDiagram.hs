module LinkDiagram where

import qualified LinkDiagram.Internal as Internal

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

--Types for the various indices the constructor is not exposed so it is known
-- that any index can not be altered outside of a link
-- The vertices, edges, unknots, regions and components are of a link are indexed by
newtype VertexIndex = VertexIndex Int deriving(Eq,Ord,Show)
newtype EdgeIndex = EdgeIndex Int deriving(Eq,Ord,Show)
newtype UnknotIndex = UnknotIndex Int deriving(Eq,Ord,Show)
newtype RegionIndex = RegionIndex Int deriving(Eq,Ord,Show)
newtype ComponentIndex = ComponentIndex Int deriving(Eq,Ord,Show)


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

--The type of a valid link diagram 
-- Wraps the internal link diagram data only for 
-- the data that constructs a valid link diagram.
--The constructor for this is not exposed.
--The consitancy conditions are discussed below.
data LinkDiagram = LinkDiagram Internal.LinkDiagramData

--Get the underlying internal link diagram data from a valid link diagram
linkDiagramData (LinkDiagram ldData) = ldData

--Constructs a link diagram from an internal representation
-- This checks that everything is consistant and constructs a valid 
-- LinkDiagram so it is know that the link diagram is always consistant.
linkDiagram :: Internal.LinkDiagramData -> Maybe LinkDiagram
linkDiagram ldData | Internal.isValidLinkDiagram ldData = Just $ LinkDiagram ldData
                   | otherwise = Nothing

--Assessors 
--Access the main aspects of the link via the internal representation

--Index sets
--The sets of indices for each of the aspects of the link
-- This is the only way to construct such indices along with
-- those occouring within an aspect
-- so within a link they are known to refer to an aspect
--We wrap the internal indices
vertexIndices :: LinkDiagram -> Set.Set VertexIndex
vertexIndices = Set.fromList . map VertexIndex . IMap.keys . Internal.crossings . linkDiagramData

edgeIndices :: LinkDiagram -> Set.Set EdgeIndex
edgeIndices =Set.fromList . map EdgeIndex . IMap.keys . Internal.edges . linkDiagramData

unknotIndices :: LinkDiagram -> Set.Set UnknotIndex
unknotIndices = Set.fromList . map UnknotIndex . IMap.keys . Internal.unknots . linkDiagramData

regionIndices :: LinkDiagram -> Set.Set RegionIndex
regionIndices = Set.fromList . map RegionIndex . IMap.keys . Internal.regions . linkDiagramData

componentIndices :: LinkDiagram -> Set.Set ComponentIndex
componentIndices = Set.fromList . map ComponentIndex . IMap.keys . Internal.components . linkDiagramData

