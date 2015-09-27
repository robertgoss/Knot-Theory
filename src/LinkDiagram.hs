module LinkDiagram where

import qualified LinkDiagram.Internal as Internal

import qualified LinkDiagram.Crossing as Crossing
import qualified LinkDiagram.Edge as Edge
import qualified LinkDiagram.Unknot as Unknot
import qualified LinkDiagram.Region as Region
import qualified LinkDiagram.Component as Component

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


--Types of the knot aspects with the diagram's indices.

--The type of internal crossings using the diagram's index
type Crossing = Crossing.Crossing EdgeIndex

--The type of internal edges using diagram's indices
type Edge = Edge.Edge VertexIndex RegionIndex ComponentIndex

--The type of internal unknotted components using diagram's index
type Unknot = Unknot.Unknot RegionIndex

--The type of internal regions using diagram's indices
type Region = Region.Region EdgeIndex UnknotIndex

--The type of internal connected component using diagram's indices
type Component = Component.Component UnknotIndex EdgeIndex


--The type of a valid link diagram 
-- Wraps the internal link diagram data only for 
-- the data that constructs a valid link diagram.
--The constructor for this is not exposed.
--The consitancy conditions are discussed below.
data LinkDiagram = LinkDiagram Internal.LinkDiagramData

--Get the underlying internal link diagram data from a valid link diagram
linkDiagramData :: LinkDiagram -> Internal.LinkDiagramData
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

