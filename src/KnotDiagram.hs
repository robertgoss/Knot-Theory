module KnotDiagram where

import LinkDiagram
import qualified Data.Set as Set

--A knot diagram is a link diagram garenteed to have only one component.
data KnotDiagram = KnotDiagram LinkDiagram

toLinkDiagram :: KnotDiagram -> LinkDiagram
toLinkDiagram (KnotDiagram lDia) = lDia

fromLinkDiagram :: LinkDiagram -> Maybe KnotDiagram
fromLinkDiagram lDia | Set.size (componentIndices lDia) == 1 
                                   = Just $ KnotDiagram lDia
                     | otherwise = Nothing