{-# LANGUAGE DeriveFunctor #-}
module LinkDiagram.Unknot where

--The type of information associated to an unknotted component of a link in terms of the indices of the regions it bounds

-- It is defined by the 2 regions that it bounds and oreinted by which region is on the left / right respectivly
data Unknot regionIndex = Unknot {
  unknotLeftRegion :: regionIndex,
  unknotRightRegion :: regionIndex
} deriving(Eq,Ord,Show,Functor)

--Change the index types used in unknot
-- The index changing function needs to be bijective.
indexChange :: (regionIndex1 -> regionIndex2) -> Unknot regionIndex1 -> Unknot regionIndex2
indexChange = fmap