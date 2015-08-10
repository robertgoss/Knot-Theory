module KnotDiagram where

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import Data.List(foldl')
import Control.Applicative((<$>))
import Control.Arrow((&&&))

import Data.Maybe(fromJust,isNothing)

--Helper types to indicate what the various indices refer to
-- The vertices, edges and regions are of a knot are indexed
type VertexIndex = Int
type EdgeIndex = Int
type RegionIndex = Int


--The type of information associated to a crossing
-- This is split by loops - ie edges that start and end 
-- at this crossing as these are harder to orient in code.
data Crossing = Crossing EdgeIndex EdgeIndex EdgeIndex EdgeIndex
                --Standard crossing with 4 distinct edges oriented clockwise 
                -- From the incoming undercrossing edge
                | LoopCrossingTL EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingTR EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingBL EdgeIndex EdgeIndex EdgeIndex
                | LoopCrossingBR EdgeIndex EdgeIndex EdgeIndex
                --A crossing with a single loop and in the 
                -- given quadrant labeled with the first edge index
                -- The remaining 2 distinct edges are given clockwise 
                | DoubleLoopL EdgeIndex EdgeIndex
                | DoubleLoopR EdgeIndex EdgeIndex
                -- A crossing that contains 2 loops the first contains
                -- the incomming undercrossing edge right or left
                -- indicate if the outgoing part of this edge if left or right
                
                
--Helper function gets the 4 edges that meet at this crossing as a 4 element list
-- Not done as a tuple for convenience later but may separate out.
crossingEdges :: Crossing -> [EdgeIndex]
crossingEdges (Crossing e1 e2 e3 e4) = [e1,e2,e3,e4]
crossingEdges (LoopCrossingTL loop e1 e2) = [e1,loop,loop,e2]
crossingEdges (LoopCrossingTR loop e1 e2) = [e1,e2,loop,loop]
crossingEdges (LoopCrossingBL loop e1 e2) = [loop,loop,e1,e2]
crossingEdges (LoopCrossingBR loop e1 e2) = [loop,e1,e2,loop]
crossingEdges (DoubleLoopL loop1 loop2) = [loop1,loop1,loop2,loop2]
crossingEdges (DoubleLoopR loop1 loop2) = [loop1,loop2,loop2,loop1]

--Helper function constructs a crossing from a tuple of 4 edgeindices
-- Can fail and return nothing if the loops/ edges given do not (locally)
-- form a valid crossing
crossingFromEdges :: (EdgeIndex,EdgeIndex,EdgeIndex,EdgeIndex) -> Maybe Crossing
crossingFromEdges (e1,e2,e3,e4) --Switch on number of loops to save laborious checks in canonical case
    | distinctNum == 4 = Just $ Crossing e1 e2 e3 e4 --All edges distinct
    | distinctNum == 3 = crossingFromSingleLoop -- We have a single loop
    | distinctNum == 2 = crossingFromDoubleLoop -- We have a 2 loops
    | otherwise = Nothing -- we have an impossible situation
  where distinctNum = Set.size $ Set.fromList [e1,e2,e3,e4]
        --We have a single loop find which quadrant we are in
        crossingFromSingleLoop
          | e1==e2 = Just $ LoopCrossingBL e1 e3 e4
          | e2==e3 = Just $ LoopCrossingTL e2 e1 e4
          | e3==e4 = Just $ LoopCrossingTR e3 e1 e2
          | e4==e1 = Just $ LoopCrossingBR e4 e2 e3
          | otherwise = Nothing --An imposible local situation
        --We have a double loop check that they form correctly left or right
        --Check that they both form loops (not 3 of the same type etc)
        crossingFromDoubleLoop
         | e1==e2 && e3==e4 = Just $ DoubleLoopL e1 e3
         | e1==e4 && e2==e3 = Just $ DoubleLoopR e1 e2
         | otherwise = Nothing -- An imposible local situation
        
--Helper function returns the first incoming edge index that isn't a loop at this crossing
-- Fails for double loops
firstIncoming :: Crossing -> Maybe EdgeIndex
firstIncoming (Crossing e _ _ _) = Just e
firstIncoming (LoopCrossingTL _ e _) = Just e
firstIncoming (LoopCrossingTR _ e _) = Just e
firstIncoming (LoopCrossingBL _ _ e) = Just e
firstIncoming (LoopCrossingBR _ e _) = Just e
firstIncoming (DoubleLoopL _ _) = Nothing
firstIncoming (DoubleLoopR _ _) = Nothing

--Helper function returns the next proper edge ie the next edge in a walk
-- around the knot that isn't a loop. Fails for a double loop and
-- if the given edge isnt a proper edge (potentially) incoming edge of this crossing
nextProperEdgeIndex :: Crossing -> EdgeIndex -> Maybe EdgeIndex
nextProperEdgeIndex (Crossing e1 e2 e3 e4) eIn
                       | eIn == e1 = Just e3
                       | eIn == e2 = Just e4
                       | eIn == e4 = Just e2
                       | otherwise = Nothing
nextProperEdgeIndex (LoopCrossingTL _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (LoopCrossingTR _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (LoopCrossingBL _ e1 e2) eIn = if e2 == eIn then Just e1 else Nothing
nextProperEdgeIndex (LoopCrossingBR _ e1 e2) eIn = if e1 == eIn then Just e2 else Nothing
nextProperEdgeIndex (DoubleLoopL _ _) _ = Nothing
nextProperEdgeIndex (DoubleLoopR _ _) _ = Nothing
               

--The type of information associated to an edge
-- The 2 vertices which the edge is going from / to
-- and the 2 regions that it borders on the left/right according to
-- it's orientation.
data Edge = Edge VertexIndex VertexIndex RegionIndex RegionIndex

--The type of information associated to an edge
-- The list of edges that border this region organised clockwise.
data Region = Region [EdgeIndex]

--The basic type of a knot diagram
-- Associates data to the crossings, edges and regions. 
data KnotDiagram = KnotDiagram {
  crossings :: IMap.IntMap Crossing,
  edges :: IMap.IntMap Edge,
  regions :: IMap.IntMap Region
}


--Constructors

--Planar Diagram
--A helper type for a basic planar diagram for format see reference
-- It is not assumed that the edges are enumerated in ascending order
type PlanarDiagram = [(Int,Int,Int,Int)]

--Construct a KnotDiagram from a given planar diagram. As not all 
-- planar diagrams define valid knots the result is wrapped in a maybe
fromPlanarDiagram :: PlanarDiagram -> Maybe KnotDiagram
fromPlanarDiagram planarDiagram
     | isNothing crossings' = Nothing
     | isNothing basicEdgesOriented' = Nothing
     | not validRegions = Nothing
     | otherwise = Just KnotDiagram { crossings = crossings,
                                      edges = edges,
                                      regions = regions
                                    }
  where  --Construct crossings from diagram with possibility of failure 
         crossings' = crossingsFromPlanarDiagram planarDiagram
         --Get crossings out of maybe for ease of use later 
         -- Is safe as we guard against a nothing in the main function
         -- Done for simplicity of further calls
         crossings = fromJust crossings'
         --Construct the basic oriented edges from crossings with possibility of failure
         -- Does not include region data yet
         basicEdgesOriented' = orientedEdgeBasicFromCrossings crossings
         --Get basicEdgeOriented  out of maybe for ease of use later 
         -- Is safe as we guard against a nothing in the main function
         -- Done for simplicity of further calls
         basicEdgesOriented = fromJust basicEdgesOriented'
         --Construct the regions here as a from regionIndices to the set
         -- Of edges that this region meets
         -- We need to validate that these regions are correct
         edgeRegions = edgeRegionsFromCrossingsAndOrienttions crossings basicEdgesOriented
         --Construct the edgeMap and regionMap in the forms from the knot diagram
         regions = IMap.map (Region . map edgeFromSide) edgeRegions
         --Validate regions (So both sides of an edge cannot appear in the same region)
         validRegions = IMap.null $ IMap.filter hasDuplicates regions
         hasDuplicates (Region xs) = length xs == Set.size (Set.fromList xs)
         --Construct the edge types used in knot diagram
         edges = IMap.mapWithKey fullEdge basicEdgesOriented
         fullEdge eIndex (e1,e2) = Edge e1 e2 
                                        (regionIndex (LeftSide eIndex))
                                        (regionIndex (RightSide eIndex))
         --Get the index of the region that meets a given side
         regionIndex side = head . IMap.keys $ IMap.filter (elem side) edgeRegions
         
 
--Helper functions for constructing a knot diagram from a planar diagram

-- Given a planar diagram constructs a map from vertices to the defined 
-- crossings if this is possible.
crossingsFromPlanarDiagram :: PlanarDiagram -> Maybe (IMap.IntMap Crossing)
crossingsFromPlanarDiagram pD = IMap.fromList <$> crossingAssocList
  where  --Enumerate the crossings from 1 for the map 
         crossingAssocList = fmap (zip [1..]) crossingList
         --Construct crossings from diagram use mapM to join monads
         crossingList = mapM crossingFromEdges pD

--Helper types in construction of edges
-- First give an edge by it's 2 end vertices (not neccesarily oriented)
-- The second indicates that the end points are oriented.
type EdgeBasic = (VertexIndex, VertexIndex)
type OrientedEdgeBasic = (VertexIndex, VertexIndex)

--From a map of crossings get out the basic edges if this is possible
--Get all the possible edges then construct a maybe association list and send to map.
edgeBasicFromCrossings :: IMap.IntMap Crossing -> Maybe (IMap.IntMap EdgeBasic)
edgeBasicFromCrossings crossings' = fmap IMap.fromList . mapM getBasicEdge  $ Set.toList edgeIndices
    where --Get a set of all the indices of edges that appear
          edgeIndices = Set.fromList . concat . IMap.elems $ fmap crossingEdges crossings'
          --For an edge index get the crossing indices that it appears in
          getCrossingIndices edgeIndex = IMap.keys $ IMap.filter (crossingHasEdge edgeIndex) crossings'
          crossingHasEdge edgeIndex crossing = edgeIndex `elem` crossingEdges crossing
          --For an edge index construct the pair of edgeIndex and 
          -- The basic edge return nothing if not well formed
          getBasicEdge edgeIndex = case getCrossingIndices edgeIndex of
                     [v] -> Just (edgeIndex,(v, v))
                     [v1,v2] -> Just (edgeIndex,(v1,v2))
                     _ -> Nothing
                     
--From a map of crossings get out the oriented basic edges if this is possible
orientedEdgeBasicFromCrossings :: IMap.IntMap Crossing -> Maybe (IMap.IntMap OrientedEdgeBasic)
orientedEdgeBasicFromCrossings crossings'
              | IMap.null crossings' = Just IMap.empty -- No crossings badly defined diagram
              --Only one crossing must be a double loop
              | IMap.size crossings'==1 = case snd $ IMap.findMin crossings' of 
                                              (DoubleLoopL _ _) -> edgeMap'
                                              (DoubleLoopR _ _) -> edgeMap'
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
data EdgeSide = LeftSide EdgeIndex | RightSide EdgeIndex deriving(Eq,Ord)

edgeFromSide :: EdgeSide -> EdgeIndex
edgeFromSide (LeftSide e) = e
edgeFromSide (RightSide e) = e

--To each region index assign the list of edges sides that meet this region
-- This list is not assumed to be ordered in any particular way and can fail
-- If the graph defined by the edges and crossings is not planar
edgeRegionsFromCrossingsAndOrienttions :: IMap.IntMap Crossing -> IMap.IntMap EdgeBasic -> IMap.IntMap [EdgeSide]
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
         combineRegions :: IMap.IntMap (Set.Set EdgeSide) -> Crossing -> IMap.IntMap (Set.Set EdgeSide)
         combineRegions regionMap c@(Crossing e1 e2 e3 e4) --For a generic crossing we need to switch on the orientation of the crossbar
               | posCrossing c    = combine2RegionsEdges (LeftSide e1) (RightSide e2)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e4)
                                  . combine2RegionsEdges (LeftSide e3) (LeftSide e4)
                                  . combine2RegionsEdges (RightSide e3) (LeftSide e1) $ regionMap
               | otherwise        = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide e4)
                                  . combine2RegionsEdges (LeftSide e3) (RightSide e2)
                                  . combine2RegionsEdges (RightSide e3) (RightSide e4) $ regionMap
         combineRegions regionMap (LoopCrossingTL loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (RightSide loop)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e2)
                                  . combine2RegionsEdges (LeftSide e2) (RightSide loop) $ regionMap
         combineRegions regionMap (LoopCrossingTR loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide loop)
                                  . combine2RegionsEdges (RightSide e2) (LeftSide loop) $ regionMap
         combineRegions regionMap (LoopCrossingBL loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (RightSide loop)
                                  . combine2RegionsEdges (RightSide e1) (RightSide e2)
                                  . combine2RegionsEdges (LeftSide e2) (RightSide loop) $ regionMap
         combineRegions regionMap (LoopCrossingBR loop e1 e2)
                                  = combine2RegionsEdges (LeftSide e1) (LeftSide e2)
                                  . combine2RegionsEdges (RightSide e1) (LeftSide loop)
                                  . combine2RegionsEdges (RightSide e2) (LeftSide loop) $ regionMap
         combineRegions regionMap (DoubleLoopL loop1 loop2)
                                  = combine2RegionsEdges (RightSide loop1) (LeftSide loop2) regionMap
         combineRegions regionMap (DoubleLoopR loop1 loop2)
                                  = combine2RegionsEdges (LeftSide loop1) (RightSide loop2) regionMap
         --Helper function is the given (basic) simple crossing positive
         -- see if the endpoint of edge 2 the start point of edge 4
         posCrossing (Crossing _ e2 _ e4) = snd edge2 == fst edge4
           where edge2 = edgeMap' IMap.! e2
                 edge4 = edgeMap' IMap.! e4     
         posCrossing _ = False --Function not designed for other edge types.