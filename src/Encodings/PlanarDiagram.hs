module Encodings.PlanarDiagram where

import qualified KnotDiagram as KD
import qualified LinkDiagram as LD
import qualified LinkDiagram.Internal as Internal
import qualified LinkDiagram.Crossing as Crossing
import qualified LinkDiagram.Edge as Edge
import qualified LinkDiagram.Region as Region
import qualified LinkDiagram.Component as Component

import Control.Applicative((<$>))
import qualified Data.IntMap as IMap

import Data.Maybe(isNothing, fromJust)
import qualified Data.Set as Set
import Control.Arrow ((&&&))
import Data.List (foldl')

--Constructors

--Planar Diagram
--A helper type for a basic planar diagram for format see reference
-- It is not assumed that the edges are enumerated in ascending order
type PlanarDiagram = [(Int,Int,Int,Int)]

--Construct a KnotDiagram from a given planar diagram. As not all 
-- planar diagrams define valid knots the result is wrapped in a maybe
fromPlanarDiagram :: PlanarDiagram -> Maybe KD.KnotDiagram
fromPlanarDiagram planarDiagram
     | isNothing crossingsMaybe = Nothing
     | isNothing basicEdgesOrientedMaybe = Nothing
     | not validRegions = Nothing
     | otherwise = LD.linkDiagram diagramData >>= KD.fromLinkDiagram
  where  --Construct crossings from diagram with possibility of failure 
         crossingsMaybe = crossingsFromPlanarDiagram planarDiagram
         --Get crossings out of maybe for ease of use later 
         -- Is safe as we guard against a nothing in the main function
         -- Done for simplicity of further calls
         crossings' = fromJust crossingsMaybe
         --Construct the basic oriented edges from crossings with possibility of failure
         -- Does not include region data yet
         basicEdgesOrientedMaybe = orientedEdgeBasicFromCrossings crossings'
         --Get basicEdgeOriented  out of maybe for ease of use later 
         -- Is safe as we guard against a nothing in the main function
         -- Done for simplicity of further calls
         basicEdgesOriented = fromJust basicEdgesOrientedMaybe
         --Construct the regions here as a from regionIndices to the set
         -- Of edges that this region meets
         -- We need to validate that these regions are correct
         edgeRegions = edgeRegionsFromCrossingsAndOrienttions crossings' basicEdgesOriented
         --Construct the edgeMap and regionMap in the forms from the knot diagram
         regionsList = IMap.map (map edgeFromSide) edgeRegions
         --Validate regions (So both sides of an edge cannot appear in the same region)
         validRegions = IMap.null $ IMap.filter hasDuplicates regionsList
         hasDuplicates xs = length xs /= Set.size (Set.fromList xs)
         --Construct regions as sset as we know no duplicates due to guard
         regions' = IMap.map makeEdgeRegion regionsList
         --Make a region consisting one a unique boundary of edges (as this is a knot)
         makeEdgeRegion edgeList = Region.Region (Set.singleton (Set.fromList edgeList)) Set.empty
         --Construct the edge types used in knot diagram
         --Set a default component index for the (unique component)
         componentIndex = 0
         edges' = IMap.mapWithKey fullEdge basicEdgesOriented
         fullEdge eIndex (e1,e2) = Edge.Edge e1 e2 
                                        (regionIndex (LeftSide eIndex))
                                        (regionIndex (RightSide eIndex))
                                        componentIndex
         --Get the index of the region that meets a given side
         regionIndex side = head . IMap.keys $ IMap.filter (elem side) edgeRegions
         --Construct a walk around the knot to create the (unique) component
         walk = knotWalk crossings' edges'
         components' = IMap.singleton componentIndex $ Component.PathComponent walk
         --Create the underlying data type.
         diagramData = Internal.LinkDiagramData {
                           Internal.crossings = crossings',
                           Internal.edges = edges',
                           Internal.unknots = IMap.empty, --No unknot in non trivial 
                           Internal.regions = regions',
                           Internal.components = components'
                        }
 
--Helper functions for constructing a knot diagram from a planar diagram

-- Given a planar diagram constructs a map from vertices to the defined 
-- crossings if this is possible.
crossingsFromPlanarDiagram :: PlanarDiagram -> Maybe (IMap.IntMap Internal.Crossing)
crossingsFromPlanarDiagram pD = IMap.fromList <$> crossingAssocList
  where  --Enumerate the crossings from 1 for the map 
         crossingAssocList = fmap (zip [1..]) crossingList
         --Construct crossings from diagram use mapM to join monads
         crossingList = mapM Crossing.fromEdges pD

--Helper types in construction of edges
-- First give an edge by it's 2 end vertices (not neccesarily oriented)
-- The second indicates that the end points are oriented.
type EdgeBasic = (Internal.VertexIndex, Internal.VertexIndex)
type OrientedEdgeBasic = (Internal.VertexIndex, Internal.VertexIndex)

--From a map of crossings get out the basic edges if this is possible
--Get all the possible edges then construct a maybe association list and send to map.
edgeBasicFromCrossings :: IMap.IntMap Internal.Crossing -> Maybe (IMap.IntMap EdgeBasic)
edgeBasicFromCrossings crossings' = fmap IMap.fromList . mapM getBasicEdge  $ Set.toList edgeIndices
    where --Get a set of all the indices of edges that appear
          edgeIndices = Set.fromList . concat . IMap.elems $ fmap Crossing.edgeIndices crossings'
          --For an edge index get the crossing indices that it appears in
          getCrossingIndices edgeIndex = IMap.keys $ IMap.filter (crossingHasEdge edgeIndex) crossings'
          crossingHasEdge edgeIndex crossing = edgeIndex `elem` Crossing.edgeIndices crossing
          --For an edge index construct the pair of edgeIndex and 
          -- The basic edge return nothing if not well formed
          getBasicEdge edgeIndex = case getCrossingIndices edgeIndex of
                     [v] -> Just (edgeIndex,(v, v))
                     [v1,v2] -> Just (edgeIndex,(v1,v2))
                     _ -> Nothing
                     
--From a map of crossings get out the oriented basic edges if this is possible
orientedEdgeBasicFromCrossings :: IMap.IntMap Internal.Crossing -> Maybe (IMap.IntMap OrientedEdgeBasic)
orientedEdgeBasicFromCrossings crossings'
              | IMap.null crossings' = Just IMap.empty -- No crossings badly defined diagram
              --Only one crossing must be a double loop
              | IMap.size crossings'==1 = case snd $ IMap.findMin crossings' of 
                                              (Crossing.DoubleLoopL _ _) -> edgeMap'
                                              (Crossing.DoubleLoopR _ _) -> edgeMap'
                                              _ -> Nothing
              --Guard against an inability to form the unordered map.
              | isNothing edgeMap' = Nothing
              --Make sure walk is valid
              | isNothing walk' = Nothing
              --Make sure all  edges are in oriented edge map
              | IMap.size edgeMap /= IMap.size orientedEdgeMap = Nothing
              | otherwise = Just orientedEdgeMap                              
   where --Get the unoriented edge map from the crossings 
         --This can fail so we guard against that for when we use the unwrapped version.
         edgeMap' = edgeBasicFromCrossings crossings'
         edgeMap = fromJust edgeMap'
         --Orient the edges choose the first incoming edge at the first crossing
         --And orient from there if the first incoming is a loop it gets the next
         -- proper edge
         -- We have guarded against a double loop and no crossings already
         firstCrossingI = head $ IMap.keys crossings'
         firstEdgeI = fromJust $ firstIncoming (crossings' IMap.! firstCrossingI)
         --Get the path of edges and vertices in walk
         walk' = walkAlongKnot (firstCrossingI, firstEdgeI) [(firstCrossingI, firstEdgeI)]
         --Walk along knot by getting the next edge at the current crossing and findind
         -- the crossing it connects to 
         -- We exclude loops in this endevour.
         walkAlongKnot (curCrossingI, curEdgeI) curPath
            | isNothing nextEdgeI' = Nothing --Cannot find the next edge 
            | nextPair == (firstCrossingI, firstEdgeI) = Just curPath-- Have reached start
            | nextPair `elem` curPath = Nothing -- Have a loop not reaching start
            | otherwise = walkAlongKnot nextPair (nextPair:curPath)
           where nextEdgeI' = nextProperEdgeIndex (crossings' IMap.! curCrossingI) curEdgeI
                 --Unwrapped nextEdge guarded against  above
                 nextEdgeI = fromJust nextEdgeI'
                 --Can assume edge exists from cons of edgeMap
                 nextEdge = edgeMap IMap.! nextEdgeI
                 nextCrossingI = if curCrossingI == fst nextEdge then snd nextEdge else fst nextEdge
                 nextPair = (nextCrossingI, nextEdgeI)
         --If walk exists we use to orient each edge that appears in it
         -- These oriented edges are added to the map along with loops (Which are oriented at crossing)
         walk = fromJust walk'
         orientedProperEdges = IMap.fromList $ map (snd &&& orientedEdge) walk
         --Given an edge and the index of the crossing it goes into orient the knot
         -- so this is second
         orientedEdge (crossingI,edgeI) = if c1==crossingI then (c2,c1) else (c1,c2)
            where (c1, c2) = edgeMap IMap.! edgeI
         loops = IMap.filter isLoop edgeMap
            where isLoop (a,b) = a == b
         orientedEdgeMap = IMap.union loops orientedProperEdges

--Data structure to represent the 2 sides of an edge
data EdgeSide = LeftSide Internal.EdgeIndex | RightSide Internal.EdgeIndex deriving(Eq,Ord,Show)

edgeFromSide :: EdgeSide -> Internal.EdgeIndex
edgeFromSide (LeftSide e) = e
edgeFromSide (RightSide e) = e

--To each region index assign the list of edges sides that meet this region
-- This list is not assumed to be ordered in any particular way and can fail
-- If the graph defined by the edges and crossings is not planar
edgeRegionsFromCrossingsAndOrienttions :: IMap.IntMap Internal.Crossing -> IMap.IntMap EdgeBasic -> IMap.IntMap [EdgeSide]
edgeRegionsFromCrossingsAndOrienttions crossings' edgeMap' = IMap.map Set.toList regionMapSets --Use sets to make finding edges quicker
  --The strategy is to make a region id for each edge side and then
  -- combine them at each crossing
   where regionMapSets = foldl' combineRegions initialMap $ IMap.elems crossings'
         initialMap = IMap.fromList . zip [1..] . map Set.singleton . concat $ [ [LeftSide e,RightSide e] | e <- edgeIndices]
         edgeIndices = IMap.keys edgeMap'
         --Helper function combines 2 regions with the given region indices in the given map
         combine2Regions region1 region2 regionMap = IMap.insertWith Set.union region1 region2Edges $ IMap.delete region2 regionMap
            where region2Edges = regionMap IMap.! region2 
         --As combine 2 region but the edges are given
         combine2RegionsEdges edge1 edge2 regionMap | region1 == region2 = regionMap
                                                    | otherwise = combine2Regions region1 region2 regionMap
              where region1 = head . IMap.keys $ IMap.filter (Set.member edge1) regionMap
                    region2 = head . IMap.keys $ IMap.filter (Set.member edge2) regionMap
         --Given a regionMap and an EdgeSide find the regionIndex bounding the given EdgeSide
         --Combine the regions at a given crossing
         --Combine depending on the type of crossing / loops
         --Use diagrams to compute local picture.
         combineRegions :: IMap.IntMap (Set.Set EdgeSide) -> Internal.Crossing -> IMap.IntMap (Set.Set EdgeSide)
         combineRegions regionMap c@(Crossing.Crossing e1 e2 e3 e4) --For a generic crossing we need to switch on the orientation of the crossbar
               | posCrossing c    = combine2RegionsEdges (LeftSide e1) (RightSide e2)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e4)
                                  . combine2RegionsEdges (LeftSide e3) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e3) (LeftSide e4) $ regionMap
                                  
               | otherwise        = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide e4)
                                  . combine2RegionsEdges (LeftSide e3) (RightSide e2)
                                  . combine2RegionsEdges (RightSide e3) (RightSide e4) $ regionMap
                                  
         combineRegions regionMap (Crossing.LoopCrossingTL loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (RightSide loop)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e2)
                                  . combine2RegionsEdges (LeftSide e2) (RightSide loop) $ regionMap
         combineRegions regionMap (Crossing.LoopCrossingTR loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide loop)
                                  . combine2RegionsEdges (RightSide e2) (LeftSide loop) $ regionMap
         combineRegions regionMap (Crossing.LoopCrossingBL loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (RightSide loop)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e2)
                                  . combine2RegionsEdges (LeftSide e2) (RightSide loop) $ regionMap
         combineRegions regionMap (Crossing.LoopCrossingBR loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide loop)
                                  . combine2RegionsEdges (RightSide e2) (LeftSide loop) $ regionMap
                                  
         combineRegions regionMap (Crossing.DoubleLoopL loop1 loop2)
                                  = combine2RegionsEdges (RightSide loop1) (LeftSide loop2) regionMap
         combineRegions regionMap (Crossing.DoubleLoopR loop1 loop2)
                                  = combine2RegionsEdges (LeftSide loop1) (RightSide loop2) regionMap
         --Helper function is the given (basic) simple crossing positive
         -- see if the endpoint of edge 2 the start point of edge 4
         posCrossing (Crossing.Crossing _ e2 _ e4) = snd edge2 == fst edge4
           where edge2 = edgeMap' IMap.! e2
                 edge4 = edgeMap' IMap.! e4     
         posCrossing _ = False --Function not designed for other edge types.
         
         
-- Edge functions

--Get an ordered 'walk' around the knot
--Here we take a vertex and edge map that are assumed to be correctly formatted
-- Ie all there refs match as in a knot.
-- This is a cycle of edges which go around the knot in it's prefered orientation
knotWalk :: IMap.IntMap Internal.Crossing -> IMap.IntMap Internal.Edge -> [Internal.EdgeIndex]
knotWalk vMap eMap | IMap.null eMap = [] --Walk around empty knot is empty
                   --Go around knot until startEdge is met again
                   --Need to take the tail of walk and add startEdgeI to start
                   -- so takeWhile can stop at the first instance of startEdgeI
                   | otherwise = startEdgeI : takeWhile (/=startEdgeI) (tail walk)
  where startEdgeI = fst . IMap.findMin $ eMap
        walk = iterate nextEdgeI startEdgeI
        --Compute the next edge index from a given edge index
        -- Get the crossing at the endpoint and use helper function.
        nextEdgeI curEdgeI = nextEdgeIndex curCrossing curEdgeI
          where curEdge = eMap IMap.! curEdgeI
                (Edge.Edge _ curCrossingI _ _ _) = curEdge
                curCrossing = vMap IMap.! curCrossingI
                


--Helper functions

--Helper function returns the first incoming edge index that isn't a loop at this crossing
-- Fails for double loops
firstIncoming :: Internal.Crossing -> Maybe Internal.EdgeIndex
firstIncoming (Crossing.Crossing e _ _ _) = Just e
firstIncoming (Crossing.LoopCrossingTL _ e _) = Just e
firstIncoming (Crossing.LoopCrossingTR _ e _) = Just e
firstIncoming (Crossing.LoopCrossingBL _ _ e) = Just e
firstIncoming (Crossing.LoopCrossingBR _ e _) = Just e
firstIncoming (Crossing.DoubleLoopL _ _) = Nothing
firstIncoming (Crossing.DoubleLoopR _ _) = Nothing

--Helper function returns the next proper edge ie the next edge in a walk
-- around the knot that isn't a loop. Fails for a double loop and
-- if the given edge isnt a proper (potentially) incoming edge of this crossing
nextProperEdgeIndex :: Internal.Crossing -> Internal.EdgeIndex -> Maybe Internal.EdgeIndex
nextProperEdgeIndex (Crossing.Crossing e1 e2 e3 e4) eIn
                       | eIn == e1 = Just e3
                       | eIn == e2 = Just e4
                       | eIn == e4 = Just e2
                       | otherwise = Nothing
nextProperEdgeIndex (Crossing.LoopCrossingTL _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (Crossing.LoopCrossingTR _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (Crossing.LoopCrossingBL _ e1 e2) eIn = if e2 == eIn then Just e1 else Nothing
nextProperEdgeIndex (Crossing.LoopCrossingBR _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (Crossing.DoubleLoopL _ _) _ = Nothing
nextProperEdgeIndex (Crossing.DoubleLoopR _ _) _ = Nothing

--Helper function returns the next proper edge ie the next edge in a walk
-- around the knot includes edges that are loops.
-- Assumes edge incoming and raises an error otherwise. 
nextEdgeIndex :: Internal.Crossing -> Internal.EdgeIndex -> Internal.EdgeIndex
nextEdgeIndex (Crossing.Crossing e1 e2 e3 e4) eIn
                       | eIn == e1 = e3
                       | eIn == e2 = e4
                       | eIn == e4 = e2
                       | otherwise = error "Non incoming edge passed to next edge index"
nextEdgeIndex (Crossing.LoopCrossingTL loop e1 e2) eIn
                       | eIn == e1 = loop  
                       | eIn == loop = e2
                       | otherwise = error "Non incoming edge passed to next edge index"
nextEdgeIndex (Crossing.LoopCrossingTR loop e1 e2) eIn
                       | eIn == e1 = loop  
                       | eIn == loop = e2  
                       | otherwise = error "Non incoming edge passed to next edge index"
nextEdgeIndex (Crossing.LoopCrossingBL loop e1 e2) eIn
                       | eIn == e2 = loop  
                       | eIn == loop = e1     
                       | otherwise = error "Non incoming edge passed to next edge index" 
nextEdgeIndex (Crossing.LoopCrossingBR loop e1 e2) eIn
                       | eIn == e1 = loop  
                       | eIn == loop = e2
                       | otherwise = error "Non incoming edge passed to next edge index"
nextEdgeIndex (Crossing.DoubleLoopL loop1 loop2) eIn = if eIn == loop1 then loop2 else loop1
nextEdgeIndex (Crossing.DoubleLoopR loop1 loop2) eIn = if eIn == loop1 then loop2 else loop1
              
swapEdge :: Internal.EdgeIndex -> Internal.EdgeIndex -> Internal.Crossing -> Maybe Internal.Crossing
swapEdge eOld eNew crossing = Crossing.fromEdges swappedEdges
   where [e1,e2,e3,e4] = Crossing.edgeIndices crossing
         swappedEdges | e1 == eOld = (eNew,e2,e3,e4)
                      | e2 == eOld = (e1,eNew,e3,e4)
                      | e3 == eOld = (e1,e2,eNew,e4)
                      | e4 == eOld = (e1,e2,e3,eNew)
                      | otherwise = (e1,e2,e3,e4)