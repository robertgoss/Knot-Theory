module LinkDiagram.Region where

import qualified Data.Set as Set

--The type of information associated to a region in terms of the index types of the
-- edges and unknotted components that bound it.
-- A region is bounded either by a series of polygons formed
-- Either for a set of edges or from an unknot
data Region edgeIndex unknotIndex = Region {
  regionEdges :: Set.Set (Set.Set edgeIndex),
  regionUnknots :: Set.Set unknotIndex
} deriving(Eq,Ord,Show)