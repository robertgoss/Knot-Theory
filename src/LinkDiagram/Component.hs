module LinkDiagram.Component where

import Data.Bifunctor

--The type of a link component in terms of the type of the
-- unknot component or edge indices that compose it.
-- This is either an unknotted component
-- Or given by a oriented path of edges 
data Component unknotIndex edgeIndex
               = UnknottedComponent unknotIndex
               | PathComponent [edgeIndex] deriving(Eq,Ord,Show)

instance Bifunctor Component where
    first unknotChange (UnknottedComponent unknotIndex) = UnknottedComponent (unknotChange unknotIndex)
    first _ (PathComponent edges) = PathComponent edges

    second edgeChange (PathComponent edges) = PathComponent $ map edgeChange edges
    second _ (UnknottedComponent unknotIndex) = UnknottedComponent unknotIndex

--Change the index types used in component
-- Both index changing functions need to be bijective.
indexChange :: (unknotIndex1 -> unknotIndex2)  -> (edgeIndex1->edgeIndex2)
               -> Component unknotIndex1 edgeIndex1 -> Component unknotIndex2 edgeIndex2
indexChange = bimap

--Get if a component is a path component
isPath :: Component unknotIndex edgeIndex -> Bool
isPath (PathComponent _) = True
isPath _ = False

--Unsafe given a component gets the edges if it is a path component
--Otherwise it will fail
pathUnsafe :: Component unknotIndex edgeIndex -> [edgeIndex]
pathUnsafe (PathComponent es) = es
pathUnsafe _ = error "Path unsafe called on non path component"