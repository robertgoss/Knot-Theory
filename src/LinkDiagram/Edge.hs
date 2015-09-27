module LinkDiagram.Edge where

--The type of information associated to an edge in terms of the indexs of 
-- The 2 crossings and 2 regions that it meets and the component that it is in.

-- The 2 vertices which the edge is going from / to
-- The 2 regions that it borders on the left/right according to
-- it's orientation.
-- The connected component that it is a part of.
data Edge vertexIndex regionIndex componentIndex = Edge {
  edgeStartCross :: vertexIndex,
  edgeEndCross :: vertexIndex,
  edgeLeftRegion :: regionIndex,
  edgeRightRegion :: regionIndex,
  edgeComponent :: componentIndex
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
              edgeStartCross = vertexChange $ edgeStartCross edge,
              edgeEndCross = vertexChange $ edgeEndCross edge,
              edgeLeftRegion = regionChange $ edgeLeftRegion edge,
              edgeRightRegion = regionChange $ edgeRightRegion edge,
              edgeComponent = componentChange $ edgeComponent edge
            }