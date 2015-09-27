module LinkDiagram.Unknot where

--The type of information associated to an unknotted component of a link in terms of the indices of the regions it bounds

-- It is defined by the 2 regions that it bounds and oreinted by which region is on the left / right respectivly
data Unknot regionIndex = Unknot {
  unknotLeftRegion :: regionIndex,
  unknotRightRegion :: regionIndex
} deriving(Eq,Ord,Show)
