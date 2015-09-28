{-# LANGUAGE InstanceSigs #-}
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

--Returns if a given edgeIndex is in the boundary of the given region
--Is true is any bound set contains edge.
edgeInBounds :: (Ord edgeIndex) => Region edgeIndex unknotIndex -> edgeIndex -> Bool
edgeInBounds region edgeIndex = any (Set.member edgeIndex) . Set.toList $ regionEdges region

--Change the index types used in region
-- Both index changing functions need to be bijective.
-- Pass the index changing function though the set constructors.
indexChange :: (Ord edgeIndex1,Ord edgeIndex2,Ord unknotIndex1,Ord unknotIndex2)
            => (edgeIndex1->edgeIndex2) -> (unknotIndex1 -> unknotIndex2) 
            -> Region edgeIndex1 unknotIndex1 -> Region edgeIndex2 unknotIndex2
indexChange edgeChange unknotChange region = Region {regionEdges = regionEdges', regionUnknots = regionUnknots'}
   where regionEdges' = Set.map (Set.map edgeChange) $ regionEdges region
         regionUnknots' = Set.map unknotChange $ regionUnknots region