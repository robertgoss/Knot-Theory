{-# LANGUAGE MultiParamTypeClasses #-}
module Morphisms.Link where

import qualified Data.IntMap as IMap
import qualified Data.Set as Set


import Morphisms.Category
import LinkDiagram
import Indices



--The datatype test of a isomorphism of link diagrams
--Is a map of all the aspects of a link.
--The constructor is hidden inside a validator
--Operates on the internal data state
--Contains both the source and target link.
data LinkDiagramIsomorphismData = LinkDiagramIso {
    crossingIMap :: IMap.IntMap VertexIndex,
    edgeIMap :: IMap.IntMap EdgeIndex,
    unknotIMap :: IMap.IntMap UnknotIndex,
    regionIMap :: IMap.IntMap RegionIndex,
    componentIMap :: IMap.IntMap ComponentIndex
} deriving(Eq,Ord,Show)

--Combine 2 maps in an unsafe manner
combineMaps :: LinkDiagramIsomorphismData -> LinkDiagramIsomorphismData -> LinkDiagramIsomorphismData
combineMaps isoData1 isoData2 = LinkDiagramIso {
                                    crossingIMap = combineCrossings,
                                    edgeIMap = combineEdges,
                                    unknotIMap = combineUnknots,
                                    regionIMap = combineRegions,
                                    componentIMap = combineComponents
                                }
    where combineIMaps iMap1 iMap2 = IMap.map ((iMap1 IMap.!) . unwrap) iMap2
          combineCrossings = combineIMaps (crossingIMap isoData1) (crossingIMap isoData2)
          combineEdges = combineIMaps (edgeIMap isoData1) (edgeIMap isoData2)
          combineUnknots = combineIMaps (unknotIMap isoData1) (unknotIMap isoData2)
          combineRegions = combineIMaps (regionIMap isoData1) (regionIMap isoData2)
          combineComponents = combineIMaps (componentIMap isoData1) (componentIMap isoData2)
          
--The identity map of a link
identityMaps :: LinkDiagram -> LinkDiagramIsomorphismData
identityMaps link = LinkDiagramIso {
                                    crossingIMap = identityMap (vertexIndices link),
                                    edgeIMap = identityMap (edgeIndices link),
                                    unknotIMap = identityMap (unknotIndices link),
                                    regionIMap = identityMap (regionIndices link),
                                    componentIMap = identityMap (componentIndices link)
                                }
          where --Takes a set of values t a map taking that set to itself
                identityMap :: (Index a) => Set.Set a -> IMap.IntMap a
                identityMap set = IMap.fromList . map doubleUnwrap $ Set.toList set
                   where doubleUnwrap x = (unwrap x,x)
                   
--The inverse maps of a link
inverseMaps :: LinkDiagramIsomorphismData -> LinkDiagramIsomorphismData
inverseMaps linkMaps = LinkDiagramIso {
                                         crossingIMap = inverseIMap (crossingIMap linkMaps),
                                         edgeIMap = inverseIMap (edgeIMap linkMaps),
                                         unknotIMap = inverseIMap (unknotIMap linkMaps),
                                         regionIMap = inverseIMap (regionIMap linkMaps),
                                         componentIMap = inverseIMap (componentIMap linkMaps)
                                      }
          where --Inverse of an IMap add unwrapping of index
                inverseIMap :: (Index a) => IMap.IntMap a -> IMap.IntMap a
                inverseIMap = IMap.fromList . map swapUnwrap . IMap.toList
                  where swapUnwrap (x,y) = (unwrap y,unsafeWrap x) --This is fine as x comes from an unwrap

--Wrapper constructor around a Link diagram isomorphism to indicate that this 
-- Isomorphism is valid include the source and target links
data LinkDiagramIsomorphism = LinkDiagramIsomorphism {
  mappingData :: LinkDiagramIsomorphismData,
  sourceLink :: LinkDiagram,
  targetLink :: LinkDiagram
} deriving(Eq,Ord)

--Get underlying data from a diagram isomorphism              
linkDiagramIsomorphismData :: LinkDiagramIsomorphism -> LinkDiagramIsomorphismData
linkDiagramIsomorphismData (LinkDiagramIsomorphism d _ _) = d 

--Mappers to map indices from one link to another
--The indices are not checked that they are in the correct source link
crossingMap :: LinkDiagramIsomorphism -> VertexIndex -> VertexIndex
crossingMap (LinkDiagramIsomorphism d _ _) vIndex = crossingIMap d IMap.! (unwrap vIndex)

edgeMap :: LinkDiagramIsomorphism -> EdgeIndex -> EdgeIndex
edgeMap (LinkDiagramIsomorphism d _ _) eIndex = edgeIMap d IMap.! (unwrap eIndex)

unknotMap :: LinkDiagramIsomorphism -> UnknotIndex -> UnknotIndex
unknotMap (LinkDiagramIsomorphism d _ _) uIndex = unknotIMap d IMap.! (unwrap uIndex)

regionMap :: LinkDiagramIsomorphism -> RegionIndex -> RegionIndex
regionMap (LinkDiagramIsomorphism d _ _) rIndex = regionIMap d IMap.! (unwrap rIndex)

componentMap :: LinkDiagramIsomorphism -> ComponentIndex -> ComponentIndex
componentMap (LinkDiagramIsomorphism d _ _) cIndex = componentIMap d IMap.! (unwrap cIndex)

--Instances 
--Make an instance of both morphism and isomorphism from category.

instance Morphism LinkDiagram LinkDiagramIsomorphism where
  source = sourceLink
  target = targetLink
  identity link = LinkDiagramIsomorphism {
                                sourceLink = link,
                                targetLink = link,
                                mappingData = identityMaps link
                            }
  composeUnsafe iso1 iso2 = LinkDiagramIsomorphism {
                                sourceLink = sourceLink iso2,
                                targetLink = targetLink iso1,
                                mappingData = combineMaps (mappingData iso1) (mappingData iso2)
                            }
                            
instance Isomorphism LinkDiagram LinkDiagramIsomorphism where
  invert iso = LinkDiagramIsomorphism {
                   sourceLink = targetLink iso,
                   targetLink = sourceLink iso,
                   mappingData = inverseMaps (mappingData iso)
               }