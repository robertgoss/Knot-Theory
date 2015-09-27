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
