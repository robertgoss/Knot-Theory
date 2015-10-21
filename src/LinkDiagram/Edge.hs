module LinkDiagram.Edge where

--The type of information associated to an edge in terms of the indexs of 
-- The 2 crossings and 2 regions that it meets and the component that it is in.

-- The 2 vertices which the edge is going from / to
-- The 2 regions that it borders on the left/right according to
-- it's orientation.
-- The connected component that it is a part of.
data Edge vertexIndex regionIndex componentIndex = Edge {
  startCrossing :: vertexIndex,
  endCrossing :: vertexIndex,
  leftRegion :: regionIndex,
  rightRegion :: regionIndex,
  component :: componentIndex
}deriving(Eq,Ord,Show)


--Change the index types used in edge
-- Both index changing functions need to be bijective.
indexChange :: (vertexIndex1 -> vertexIndex2)
            -> (regionIndex1 -> regionIndex2)
            -> (componentIndex1 -> componentIndex2)
            -> Edge vertexIndex1 regionIndex1 componentIndex1
            -> Edge vertexIndex2 regionIndex2 componentIndex2
indexChange vertexChange regionChange componentChange edge
            = Edge {
              startCrossing = vertexChange $ startCrossing edge,
              endCrossing = vertexChange $ endCrossing edge,
              leftRegion = regionChange $ leftRegion edge,
              rightRegion = regionChange $ rightRegion edge,
              component = componentChange $ component edge
            }

--The number of times the given edge meets the crossing with the given index. Is either 0, 1 or 2.
crossingMeetingNumber :: (Eq vertexIndex) => Edge vertexIndex regionIndex componentIndex -> vertexIndex -> Int
crossingMeetingNumber edge crossingIndex = length $ filter (==crossingIndex) [startCrossing edge, endCrossing edge]

--Does the given edge meet a region with given index
meetsRegion :: (Eq regionIndex) => regionIndex -> Edge vertexIndex regionIndex componentIndex -> Bool
meetsRegion regionIndex edge = (regionIndex == leftRegion edge) || (regionIndex == rightRegion edge)