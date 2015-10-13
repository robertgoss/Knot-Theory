{-# LANGUAGE DeriveFunctor #-}
module LinkDiagram.Unknot where

--The type of information associated to an unknotted component of a link in terms of the indices of the regions it bounds

-- It is defined by the 2 regions that it bounds and oreinted by which region is on the left / right respectivly
data Unknot regionIndex componentIndex = Unknot {
  leftRegion :: regionIndex,
  rightRegion :: regionIndex,
  component :: componentIndex
} deriving(Eq,Ord,Show,Functor)

--Change the index types used in unknot
-- The index changing function needs to be bijective.
indexChange :: (regionIndex1 -> regionIndex2) -> (componentIndex1 -> componentIndex2)
            -> Unknot regionIndex1 componentIndex1 -> Unknot regionIndex2 componentIndex2
indexChange changeRegion changeComponent unknot = Unknot {
                                                    leftRegion = changeRegion (leftRegion unknot),
                                                    rightRegion = changeRegion (rightRegion unknot),
                                                    component = changeComponent (component unknot)
                                                   }

--Does the given unknot meet a region with given index
meetsRegion :: (Eq regionIndex) => regionIndex -> Unknot regionIndex componentIndex -> Bool
meetsRegion regionIndex unknot = (regionIndex == leftRegion unknot) || (regionIndex == rightRegion unknot)