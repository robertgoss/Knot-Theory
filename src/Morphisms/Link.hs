{-# LANGUAGE MultiParamTypeClasses #-}
module Morphisms.Link where

import qualified Data.IntMap as IMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(mapMaybe)
import Data.List(inits,tails)

import Morphisms.Category
import LinkDiagram
import Indices

import qualified LinkDiagram.Internal as Int
import qualified LinkDiagram.Component as Component
import qualified LinkDiagram.Edge as Edge
import qualified LinkDiagram.Unknot as Unknot
import qualified LinkDiagram.Region as Region

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
  sourceLink :: LinkDiagram,
  targetLink :: LinkDiagram,
  mappingData :: LinkDiagramIsomorphismData
} deriving(Eq,Ord)

--Get underlying data from a diagram isomorphism              
linkDiagramIsomorphismData :: LinkDiagramIsomorphism -> LinkDiagramIsomorphismData
linkDiagramIsomorphismData (LinkDiagramIsomorphism _ _ d) = d 

--Mappers to map indices from one link to another
--The indices are not checked that they are in the correct source link
crossingMap :: LinkDiagramIsomorphism -> VertexIndex -> VertexIndex
crossingMap (LinkDiagramIsomorphism _ _ d) vIndex = crossingIMap d IMap.! (unwrap vIndex)

edgeMap :: LinkDiagramIsomorphism -> EdgeIndex -> EdgeIndex
edgeMap (LinkDiagramIsomorphism _ _ d) eIndex = edgeIMap d IMap.! (unwrap eIndex)

unknotMap :: LinkDiagramIsomorphism -> UnknotIndex -> UnknotIndex
unknotMap (LinkDiagramIsomorphism _ _ d) uIndex = unknotIMap d IMap.! (unwrap uIndex)

regionMap :: LinkDiagramIsomorphism -> RegionIndex -> RegionIndex
regionMap (LinkDiagramIsomorphism _ _ d) rIndex = regionIMap d IMap.! (unwrap rIndex)

componentMap :: LinkDiagramIsomorphism -> ComponentIndex -> ComponentIndex
componentMap (LinkDiagramIsomorphism _ _ d) cIndex = componentIMap d IMap.! (unwrap cIndex)

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
               
--Get the number of isomorphisms between 2 link diagrams
-- This is mapping data between two link diagrams that
--Each map is a bijection.
--Obeys all adjaceny rules.
--We first take a mapping of components and then alignments of the edges in 
-- each components and then construct all maps from this alignment then filter on
-- adjacency rules
isomorphismsBetweenLinkDiagrams :: LinkDiagram -> LinkDiagram -> [LinkDiagramIsomorphism]
isomorphismsBetweenLinkDiagrams link1 link2 = validIsos
  where alignments = getAlignmentsBetweenLinkDiagrams linkD1 linkD2
        potentialMappings = mapMaybe (constructMappingsFromAlignment linkD1 linkD2) alignments
        validMappings = filter (validAdjacency linkD1 linkD2) potentialMappings
        validIsos = map (LinkDiagramIsomorphism link1 link2) validMappings
        linkD1 = linkDiagramData link1
        linkD2 = linkDiagramData link2
    

--Sub computations for isomorphismsBetweenLinkDiagrams
--Construct an alignment between 2 links consists of a bijection of path and 
-- unknot components and for each path component an alignment which is a 
-- bijection of the cycles of edges in the paths. If the the mapped paths components
-- have different numbers of edges it is not valid.
type LinkAlignment = (IMap.IntMap Int.ComponentIndex, IMap.IntMap Int.EdgeIndex)

getAlignmentsBetweenLinkDiagrams :: Int.LinkDiagramData -> Int.LinkDiagramData -> [LinkAlignment]
getAlignmentsBetweenLinkDiagrams linkData1 linkData2 = concatMap makeAlignmentsFromCMap componentMaps
  where componentIndices1 = IMap.keys $ Int.components linkData1
        componentIndices2 = IMap.keys $ Int.components linkData2
        --Get all bijections between component indices
        -- Taking unknots to unknots and paths to paths with the same number of edges
        componentMaps = allValidBijections validComponentMap componentIndices1 componentIndices2
        --See if the component map is valid must be of the same type after lookup
        -- path components must be the same length
        validComponentMap index1 index2
          = case (Int.components linkData1 IMap.! index1,Int.components linkData2 IMap.! index2) of
                  (Component.UnknottedComponent _,Component.UnknottedComponent _) -> True
                  (Component.PathComponent es1,Component.PathComponent es2) -> length es1 == length es2
                  _ -> False
        --Given a validation function that says if 2 elements can be connected find all valid bijections
        allValidBijections :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [IMap.IntMap Int]
        allValidBijections _ [] [] = [IMap.empty]
        allValidBijections _ _ [] = [] --Mapping to the empty set from non empty cannot be bijective
        allValidBijections _ [] _ = [] --Mapping from the empty set to non empty cannot be bijective
        allValidBijections validation (sourceElem:rest) targetElems = concatMap partialValidBijections validTargets
          where validTargets = filter (validation sourceElem) targetElems
                partialValidBijections targetElem = map (IMap.insert sourceElem targetElem) partials
                   where remainingTargets = filter (/= targetElem) targetElems
                         partials = allValidBijections validation rest remainingTargets
        --Given a valid component map construct all alignments by getting all alignments of edges
        makeAlignmentsFromCMap :: IMap.IntMap Int.ComponentIndex -> [LinkAlignment]
        makeAlignmentsFromCMap componentMapping = map alignmentFromPathAlign pathAligns
          where pathComponentIndices = filter (Component.isPath . (IMap.!) (Int.components linkData1)) $ IMap.keys componentMapping
                --Pairs of the edges associated to each path mapped pair of path components
                pathPairs = map pathPair pathComponentIndices
                pathPair index1 = (Component.pathUnsafe component1, Component.pathUnsafe component2)
                   where index2 = (IMap.!) componentMapping (unwrap index1)
                         component1 = (IMap.!) (Int.components linkData1) (unwrap index1)
                         component2 = (IMap.!) (Int.components linkData2) (unwrap index2)
                --Get all products of alignments of edges
                pathAligns :: [[[(Int.EdgeIndex, Int.EdgeIndex)]]]
                pathAligns = allProducts $ map alignments pathPairs
                --Construct a alignment from a path alignment
                --Make an edge map from each path alignment then take the union
                alignmentFromPathAlign :: [[(Int.EdgeIndex, Int.EdgeIndex)]] -> LinkAlignment
                alignmentFromPathAlign pathAlign = (componentMapping, edgeMapping)
                  where edgeMapping = IMap.unions $ map (IMap.fromList . map indexUnwrap) pathAlign
                        indexUnwrap (i1,i2) = (unwrap i1,i2)
                --Given a list of elements take all combinations of elements in these lists
                allProducts :: [[a]] -> [[a]]
                allProducts [] = [[]]
                allProducts (xs:rest) = [x:prod | x <- xs, prod <- allProducts rest ]
        --Given 2 equal lengths lists get the list of alignments
        -- So pairs which respect cyclic ordering
        alignments :: ([a],[a]) -> [[(a,a)]]
        alignments (xs,ys) = map (zip xs) $ tail cycles
          where cycles = zipWith (++) (tails ys) (inits ys) --get all presentations of ys respecting cyclic ordering
        



--Given an alignment construct a mapping which restricts to this alignment 
-- on just the edge and component maps
--If this cannot be done return Nothing
constructMappingsFromAlignment :: Int.LinkDiagramData -> Int.LinkDiagramData 
                                          -> LinkAlignment -> Maybe LinkDiagramIsomorphismData
constructMappingsFromAlignment linkData1 linkData2 (componentIMap', edgeIMap') = Just iso
  where --The the induced map of edges and components from the maps of indices
        componentAssocs = map getBothComps $ IMap.toList componentIMap'
        getBothComps (i1,i2) = ((Int.components linkData1 IMap.! i1), (Int.components linkData2 IMap.! i2))
        edgeAssocs = map getBothEdges $ IMap.toList edgeIMap'
        getBothEdges (i1,i2) = (((Int.edges linkData1) IMap.! i1), (Int.edges linkData2 IMap.! i2))
        --Use the component map to make the map of unknots
        --Get the components of unknot indices and pull out index map
        unknotIMap' = IMap.fromList $ mapMaybe unknotPair componentAssocs
        --Wrap in maybe to only deal with unknot components
        --Assumed well formed map so unknotUnsafe is ok. 
        unknotPair (c1,c2) = if Component.isUnknot c1 then Just (Component.unknotUnsafe c1,Component.unknotUnsafe c2)
                                                      else Nothing
        unknotAssocs = map getBothUnknots $ IMap.toList unknotIMap'
        getBothUnknots (i1,i2) = (((Int.unknots linkData1) IMap.! i1), (Int.unknots linkData2 IMap.! i2))
        --Every region meets at least one edge or unknot use this to construct
        --the region map
        regionEdgeIMap = IMap.fromList $  concatMap edgePairToRegions edgeAssocs
        regionUnknotIMap = IMap.fromList $  concatMap unknotPairToRegions unknotAssocs
        regionIMap' = IMap.union regionEdgeIMap regionUnknotIMap
        --Given a mapping of 2 edges / unknots the left/right regions should map.
        edgePairToRegions (edge1,edge2) = [(Edge.leftRegion edge1,Edge.leftRegion edge2)
                                           ,(Edge.rightRegion edge1,Edge.rightRegion edge2)]
        unknotPairToRegions (u1,u2) = [(Unknot.leftRegion u1,Unknot.leftRegion u2),
                                       (Unknot.rightRegion u1,Unknot.rightRegion u2)]
        --Every crossing must meet at least one edge
        crossingIMap' = IMap.fromList $  concatMap edgePairToCrossings edgeAssocs
        --Given 2 mapped edges the start/end crossings should map.
        edgePairToCrossings (edge1,edge2) = [(Edge.startCrossing edge1,Edge.startCrossing edge2),
                                             (Edge.endCrossing edge1,Edge.endCrossing edge2)]
        --Construct link diagram isomorphism from these maps
        iso = LinkDiagramIso {
          crossingIMap = wrapIMap crossingIMap',
          edgeIMap = wrapIMap edgeIMap',
          unknotIMap = wrapIMap unknotIMap',
          regionIMap = wrapIMap regionIMap',
          componentIMap = wrapIMap componentIMap'
        }
        --Wrap maps to linkdiagram index
        wrapIMap :: (Index index) => IMap.IntMap Int -> IMap.IntMap index
        wrapIMap = IMap.map Indices.unsafeWrap

--Given a set of maps check all the adjacency relations hold
validAdjacency :: Int.LinkDiagramData -> Int.LinkDiagramData -> LinkDiagramIsomorphismData -> Bool
validAdjacency linkData1 linkData2 mappingData' = allValidCrossing && allValidRegion
  where --Maps of indices induced from mapping data
        mapRegionIndex rIn = Indices.unwrap $ regionIMap mappingData' IMap.! rIn
        mapUnknotIndex uIn = Indices.unwrap $ unknotIMap mappingData' IMap.! uIn
        mapEdgeIndex eIn = Indices.unwrap $ edgeIMap mappingData' IMap.! eIn
        mapCrossingIndex cIn = Indices.unwrap $ crossingIMap mappingData' IMap.! cIn
        --Crossing is valid if mapping the crossing data is the same as the mapped crossing
        allValidCrossing = all validCrossing . IMap.keys $ Int.crossings linkData1 
        validCrossing cIn = mapCrossingData cIn == mapCrossing cIn
        mapCrossingData cIn = fmap mapEdgeIndex $ Int.crossings linkData1 IMap.! cIn
        mapCrossing cIn = Int.crossings linkData2 IMap.! (mapCrossingIndex cIn)
        --Region is valid if mapping the region data is the same as the mapped region
        allValidRegion = all validRegion . IMap.keys $ Int.regions linkData1 
        validRegion rIn = mapRegionData rIn == mapRegion rIn
        mapRegionData rIn = Region.indexChange mapEdgeIndex mapUnknotIndex $ Int.regions linkData1 IMap.! rIn
        mapRegion rIn = Int.regions linkData2 IMap.! (mapRegionIndex rIn)