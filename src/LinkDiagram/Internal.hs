module LinkDiagram.Internal where

import qualified LinkDiagram.Crossing as Crossing
import qualified LinkDiagram.Edge as Edge
import qualified LinkDiagram.Unknot as Unknot
import qualified LinkDiagram.Region as Region
import qualified LinkDiagram.Component as Component

import qualified Data.IntMap as IMap

import Data.Maybe(fromJust,isNothing)

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
--In particular it checks the following relations:
-- For each crossing:
--  - Each edge mentioned in the crossing meets the crossing once and each loop twice.
--  - The first edge in the crossing is incomming to the crossing.
--  - The local regions on either side of neighbouring edges are the same.
-- For each edge:
--  - Each crossing the edge meets contains the edge.
--  - Each region the edge meets has that edge in one of its boundaries.
--  - The 2 regions which meet the edge are distinct.
--  - The component of the region is a path containing the edge.
-- For each unknot
--  - Each region the unknot meets has that unknot in one of its boundaries.
--  - The 2 regions which meet the unknot are distinct.
--  - One of the regions is bounded only be this unknot.
--  - Their exists a unique component which is this unknot.
-- For each region
--  - Each unknot in the boundary of the region meets this unknot.
--  - Each edge in a boundary of the region meets the region.
--  - No two edges in different boundaries of the region can be equal.
-- For each component
--  - If this is a path component then for each edge in the path has this as a component
--  - If this is a path component then for any sequential pair of edges in the path
--       the end crossing of the first and start crossing of the second are the same
--       and the edges meet at the crossing as opposite.
--These confirm it is a valid link 
-- We further require and required consistency conditions are met for looking up indices
-- Which are that and vertex, edges etc index mentioned in a vertex, edge, etc is in the respective table.
-- Ie the vertices, edges etc refered to by indices always exist
isValidLinkDiagram :: LinkDiagramData -> Bool
isValidLinkDiagram = undefined

--Check that each corssing in the link is valid as above.
--
isValidCrossings :: LinkDiagramData -> Bool
isValidCrossings link = all isValidCrossing . IMap.assocs $ crossings link
  where --Lookup an edge in the link from it's index wraps in maybe for failure
        lookupEdge edgeIndex = IMap.lookup edgeIndex $ edges link
        --Determine if a given crossing is valid
        isValidCrossing (vertexIndex,crossing) 
                    | isNothing crossingEdgesM = False -- We require all of the indices to be in the map.
                    -- The first incoming edge must be incoming so it's end crossing must be this crossing
                    | Edge.endCrossing firstIncomingEdge /= vertexIndex = False
                    --If an all edges which meets the crossing must meet it the correct number of times
                    | not . all edgeMeetingNumbersMatch $ zip edgeIndices crossingEdges = False
                    --The region meetings must be valid
                    | not regionMeetingValid = False
                    | otherwise = True
           where --Get the 4 edges that meet the crossing clockwise from the first undercrossing 
                 --with some repeats due to loops
                 -- This is wrapped in a maybe and is nothing if any of the indices fail
                 edgeIndices = Crossing.edgeIndices crossing
                 crossingEdgesM = sequence $ map lookupEdge edgeIndices
                 --This is valid as we guard against nothing
                 crossingEdges = fromJust crossingEdgesM
                 --The first incomming edge
                 firstIncomingEdge = head crossingEdges
                 --If a given edge meeting number with a crossing is the same as the meeting number of a crossing to an edge.
                 edgeMeetingNumbersMatch (edgeIndex, edge) = (Crossing.edgeMeetingNumber crossing edgeIndex) 
                                                             == (Edge.crossingMeetingNumber edge vertexIndex)
                 --Do the region meetings match as required
                 -- Sort out the connections of edges based on if the crossing is positive or negative 
                 -- Ie if the second edge is incoming or outgoing . positive is outgoing.
                 positive = Edge.startCrossing (crossingEdges !! 1) == vertexIndex
                 regionMeetingValid | positive =  Edge.leftRegion (crossingEdges !! 0) == Edge.leftRegion (crossingEdges !! 1)
                                               && Edge.rightRegion (crossingEdges !! 0) == Edge.leftRegion (crossingEdges !! 3)
                                               && Edge.leftRegion (crossingEdges !! 2) == Edge.rightRegion (crossingEdges !! 1)
                                               && Edge.rightRegion (crossingEdges !! 2) == Edge.rightRegion (crossingEdges !! 3)

                                    | otherwise = Edge.leftRegion (crossingEdges !! 0) == Edge.rightRegion (crossingEdges !! 1)
                                               && Edge.rightRegion (crossingEdges !! 0) == Edge.rightRegion (crossingEdges !! 3)
                                               && Edge.leftRegion (crossingEdges !! 2) == Edge.leftRegion (crossingEdges !! 1)
                                               && Edge.rightRegion (crossingEdges !! 2) == Edge.leftRegion (crossingEdges !! 3)

--Check that each corssing in the link is valid as above.
--
isValidEdges :: LinkDiagramData -> Bool
isValidEdges link = all isValidEdge . IMap.assocs $ edges link
  where --Lookup a crossing, component or region in the link from it's index wraps in maybe for failure
        lookupCrossing vertexIndex = IMap.lookup vertexIndex $ crossings link
        lookupRegion regionIndex = IMap.lookup regionIndex $ regions link
        lookupComponent componentIndex = IMap.lookup componentIndex $ components link
        --Determine if a given edge is valid
        isValidEdge (edgeIndex, edge)
                    | isNothing crossingsM = False --Both crossings indexed shoud be in the link
                    | isNothing regionsM = False --Both regions indexed shoud be in the link
                    | isNothing componentM = False -- The component indexed should be non zero
                    | leftRegion == rightRegion = False -- The left and right regions should be distinct
                    | not edgeInComponentPath = False -- The edge should be in the path of it's component
                    | not $ edgeIndex `elem` (Crossing.edgeIndices startCrossing) = False -- The edge should be in it's start crossing
                    | not $ edgeIndex `elem` (Crossing.edgeIndices endCrossing) = False -- The edge should be in it's end crossing
                    | not $ Region.edgeInBounds leftRegion edgeIndex = False -- The edge should bound its left region
                    | not $ Region.edgeInBounds rightRegion edgeIndex = False -- The edge should bound its right region
                    | otherwise = True -- All conditions met
           where --Test we can lookup both start and end crossings
                 -- This is wrapped in a maybe and is nothing if any of the indices fail
                 crossingsM = sequence $ map lookupCrossing [Edge.startCrossing edge, Edge.endCrossing edge]
                 --This is safe as we guard against failure
                 [startCrossing, endCrossing] = fromJust crossingsM
                 --Test we can lookup both left and right regions
                 -- This is wrapped in a maybe and is nothing if any of the indices fail
                 regionsM = sequence $ map lookupRegion [Edge.leftRegion edge, Edge.rightRegion edge]
                 --This is safe as we guard against failure
                 [leftRegion, rightRegion] = fromJust regionsM
                 --Test we can lookup the indexed component
                 -- This is wrapped in a maybe and is nothing if any of the indices fail
                 componentM = lookupComponent $ Edge.component edge
                 --This is safe as we guard against failure
                 component = fromJust componentM
                 --This component is a path and it contains this edgeIndex
                 edgeInComponentPath = case component of
                                          (Component.UnknottedComponent _) -> False
                                          (Component.PathComponent pathIndices) -> edgeIndex `elem` pathIndices

