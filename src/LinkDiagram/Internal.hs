module LinkDiagram.Internal where

import qualified LinkDiagram.Crossing as Crossing
import qualified LinkDiagram.Edge as Edge
import qualified LinkDiagram.Unknot as Unknot
import qualified LinkDiagram.Region as Region
import qualified LinkDiagram.Component as Component

import qualified Data.IntMap as IMap

--The internal construction of a link diagram in terms of
-- maps of integers to the various aspects crossings, edges etc.
--This is kept internal to not expose the underlying data structure and so (within any link) 
-- any constructed index is known to correspond to some aspect.

--We use maps and indices for the internal construction to avoid the required infinite recursion
-- and cycles in the data structures. See for example the definition of haskell graphs.

--Helper types to indicate what the various indices refer to.
-- The vertices, edges, unknots, regions and components are of a link are indexed by
type VertexIndex =  Int
type EdgeIndex = Int
type UnknotIndex = Int
type RegionIndex = Int
type ComponentIndex = Int

--The type of internal crossings using the integer index
type Crossing = Crossing.Crossing EdgeIndex

--The type of internal edges using integer indices
type Edge = Edge.Edge VertexIndex RegionIndex ComponentIndex

--The type of internal unknotted components using interger index
type Unknot = Unknot.Unknot RegionIndex


--The type of internal regions using integer indices
type Region = Region.Region EdgeIndex UnknotIndex

--The type of internal connected component using integer indices
type Component = Component.Component UnknotIndex EdgeIndex


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


--Returns if the given link diagram data is consistent and constructs a valid link
--In particular it checks that:
-- 
--These confirm it is a valid link 
-- We further require and required consistency conditions are met for looking up indices
-- Which are that and vertex, edges etc index mentioned in a vertex, edge, etc is in the respective table.
-- Ie the vertices, edges etc refered to by indices always exist
isValidLinkDiagram :: LinkDiagramData -> Bool
isValidLinkDiagram = undefined
