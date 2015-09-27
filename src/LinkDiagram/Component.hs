module LinkDiagram.Component where

--The type of a link component in terms of the type of the
-- unknot component or edge indices that compose it.
-- This is either an unknotted component
-- Or given by a oriented path of edges 
data Component unknotIndex edgeIndex
               = UnknottedComponent unknotIndex
               | PathComponent [edgeIndex] deriving(Eq,Ord,Show)