{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module KnotMorphism where

import KnotDiagram

import qualified Data.IntMap as IMap
import qualified Data.Set as Set
import Data.Maybe(isJust, fromJust, mapMaybe)
import Data.List(inits,tails)

--Outline definition of a morphism in a category
--Functions shared by all knot morphisms 
--Only requirement on objects is Eq to assert source and target are
-- The same in composition
class (Eq a) => Morphism a b | b -> a where
   source :: b -> a
   target :: b -> a
   --Return the identity morphism for an object
   identity :: a -> b
   --Composition is done in reverse order in the same way as functional
   -- composition so the source of (f `compose` g) is the source of g!
   --Composition uses maybe as for 2 morphisms to be composible
   -- There source and target must align. 
   compose :: b -> b -> Maybe b
   compose f g | target g == source f = Just $ composeUnsafe f g
               | otherwise = Nothing
   composeUnsafe :: b -> b -> b
   composeUnsafe a b = fromJust $ compose a b 
   
--Outline definition of a isomorphism in a category
-- A morphism with an inverse.
class (Morphism a b) => Isomorphism a b where
   invert :: b -> b   


data KnotDiagramIsomorphism = KnotDiagIso {
   knotDiagIsoSource :: KnotDiagram,
   knotDiagIsoTarget :: KnotDiagram,
   --An isomorphism of knot diagrams can be defined be an association 
   -- of the crossings of the source to the target diagram
   knotDiagIsoCrossingMap :: IMap.IntMap VertexIndex,
   --The relations between the edges of the source and target diagrams
   -- induced by the above map of crossings
   knotDiagIsoEdgeMap :: IMap.IntMap EdgeIndex,
   --The relations between the regions of the source and target diagrams
   -- induced by the above map of crossings
   knotDiagIsoRegionMap :: IMap.IntMap RegionIndex
} deriving(Eq,Ord,Show)

instance Morphism KnotDiagram KnotDiagramIsomorphism where
    source = knotDiagIsoSource
    target = knotDiagIsoTarget
    --Identity. All maps are identity
    identity knot = KnotDiagIso knot knot cMap eMap rMap
       where cMap = identityMap $ crossings knot
             eMap = identityMap $ edges knot
             rMap = identityMap $ regions knot
             identityMap = IMap.fromList . map dup . IMap.keys
             dup x = (x,x)
    --Compose the maps
    composeUnsafe morp1 morp2 = KnotDiagIso knot1 knot2 cMap eMap rMap 
       where knot1 = source morp2
             knot2 = target morp1
             cMap = knotDiagIsoCrossingMap morp1 `compMaps` knotDiagIsoCrossingMap morp2
             eMap = knotDiagIsoEdgeMap morp1 `compMaps` knotDiagIsoEdgeMap morp2
             rMap = knotDiagIsoRegionMap morp1 `compMaps` knotDiagIsoRegionMap morp2
             compMaps iMap1 iMap2 = IMap.fromList [(i,iMap1 IMap.! j) | (i,j)<-IMap.assocs iMap2]
             
instance Isomorphism KnotDiagram KnotDiagramIsomorphism where
     --Invert individual maps
     invert morp = KnotDiagIso s t cMap eMap rMap
        where s = target morp
              t = source morp
              cMap = invMap $ knotDiagIsoCrossingMap morp
              eMap = invMap $ knotDiagIsoEdgeMap morp
              rMap = invMap $ knotDiagIsoRegionMap morp
              invMap = IMap.fromList . map swp . IMap.toList
              swp (a,b) = (b,a)
              
--Returns all possible diagram isomorphisms between a knot diagram and itself.
knotDiagramAllAutomorphisms :: KnotDiagram -> [KnotDiagramIsomorphism]
knotDiagramAllAutomorphisms knot = knotDiagramsAllIsomorphisms knot knot

--Returns all possible diagram isomorphisms between two knot diagrams
knotDiagramsAllIsomorphisms :: KnotDiagram -> KnotDiagram -> [KnotDiagramIsomorphism]
knotDiagramsAllIsomorphisms knot1 knot2 
                       --Isomorphic diagrams must have an identical number of edges
                       --We guard against this not being the case as it makes it easier to form maps
                      | IMap.size (edges knot1) /= IMap.size (edges knot2) = []
                      | otherwise = mapMaybe (isoFromAlignment knot1 knot2) alignments
    --Try to align 2 walks around the knots. 
    --Such an alignment is possible as we have guarded against a differing number of edges
    -- Each isomorphism is generate by such an alignment
  where walk1 = knotWalk knot1
        walk2 = knotWalk knot2
        --Alignments of 2 walks by fixing walk1 and changing the starting point of walk 2
        alignments = zip (repeat walk1) (cycles walk2)
         --All cyclic permutations of a list
         -- Take tail otherwise identity is repeated.
        cycles xs = tail $ zipWith (++) (tails xs) (inits xs) 

--Try to form iso from alignment of walks
--Helper function from above
-- Broken off for readability.
isoFromAlignment :: KnotDiagram -> KnotDiagram -> ([EdgeIndex],[EdgeIndex]) -> Maybe KnotDiagramIsomorphism
isoFromAlignment knot1 knot2 (walk1', walk2') | not assertAllCrossings = Nothing
                                              | not assertAllRegions  = Nothing
                                              -- If the region and corssing maps constructed are valid construct isomorphic map
                                              | otherwise = Just KnotDiagIso {
                                                 knotDiagIsoSource = knot1,
                                                 knotDiagIsoTarget = knot2,
                                                 knotDiagIsoCrossingMap = cMap,
                                                 knotDiagIsoEdgeMap = eMap,
                                                 knotDiagIsoRegionMap = rMap
                                                }
    where --Construct edge map taking the edges of walk 1 to walk 2
          eMap = IMap.fromList $ zip walk1' walk2'
          --Convienience function to map edges to edges
          eFun = (eMap IMap.!)
          --The actual edges in both walks
          edges1 = map (edges knot1 IMap.!) walk1'
          edges2 = map (edges knot2 IMap.!) walk2'
          --Extend such a map to crossings and regions
          -- As the first crossing and right and left most regions to an edge are preserved under 
          -- isomorphism
          cMap = IMap.fromList $ zip (map edgeStartCross edges1) (map edgeStartCross edges2)    
          --Convienience function to map crossings to crossings
          cFun = (cMap IMap.!)      
          rMapLeft = IMap.fromList $ zip (map edgeLeftRegion edges1) (map edgeLeftRegion edges2) 
          rMapRight = IMap.fromList $ zip (map edgeRightRegion edges1) (map edgeRightRegion edges2) 
          rMap = IMap.union rMapLeft rMapRight
          --Convienience function to map regions to regions
          rFun = (rMap IMap.!)  
          --Check for consistency
          --This is an isomorphism if the map of crossings is correct under edges
          --Assert that it is the same to get crossing edges by mapping crossing index and getting edges
          -- As getting edges and mapping edges
          assertCrossing cI = crossingEdges (crossings knot2 IMap.! cFun cI) 
                                == (map eFun . crossingEdges) (crossings knot1 IMap.! cI)
          assertAllCrossings = all assertCrossing . IMap.keys $ crossings knot1
          --Assert that it is the same to get region edges by mapping region index and getting edges
          -- As getting edges and mapping edges
          assertRegionLHS rI = regionEdges (regions knot2 IMap.! rFun rI) 
          assertRegionRHS rI = (Set.map eFun . regionEdges) (regions knot1 IMap.! rI)
          assertRegion regionI = assertRegionLHS regionI == assertRegionRHS regionI
          assertAllRegions = all assertRegion . IMap.keys $ regions knot1
          
--If 2 knot diagrams are isomorphic returns an isomorphism between then else nothing       
knotDiagramsIsomorphism :: KnotDiagram -> KnotDiagram -> Maybe KnotDiagramIsomorphism
knotDiagramsIsomorphism knot1 knot2 = case knotDiagramsAllIsomorphisms knot1 knot2 of
                                              [] -> Nothing
                                              (iso:_) -> Just iso

--Returns if 2 knot diagrams are isomorphic as knot diagrams
knotDiagramsIsomorphic :: KnotDiagram -> KnotDiagram -> Bool
knotDiagramsIsomorphic knot1 knot2 = isJust $ knotDiagramsIsomorphism knot1 knot2

--Takes a knot diagram and gives and isomorphism to a knot with vertex etc indexes in 
-- the range [1..vertex num] etc. 
--This is an order preserving map between indices.
toOrderedMorphism :: KnotDiagram -> KnotDiagramIsomorphism
toOrderedMorphism knot = KnotDiagIso {
                            knotDiagIsoSource = knot,
                            knotDiagIsoTarget = orderedKnot,
                            knotDiagIsoCrossingMap = cMap,
                            knotDiagIsoEdgeMap = eMap,
                            knotDiagIsoRegionMap = rMap
                         }
     where cMap = mapReordering $ crossings knot
           eMap = mapReordering $ edges knot
           rMap = mapReordering $ regions knot
           mapReordering m = IMap.fromList $ zip (IMap.keys m) [1..]
           newCs = reorderedMap $ crossings knot
           newEs = reorderedMap $ edges knot
           newRs = reorderedMap $ regions knot
           reorderedMap = IMap.fromList . zip [1..] . IMap.elems
           orderedKnot = KnotDiagram newCs newEs newRs
           
           
--A datatype for a single reidermiester move 
-- It can to be assumed that all the indices are valid as the constructor is not
-- Exported.
data ReidermeisterMove = --A R0 move which is a diagram isomorphism
                         R0 KnotDiagramIsomorphism
                         --A R1 move which is a diagram, an edge,a bool to say if we twist right or left
                         -- of the oriented edge and a bool to say if the crossing is positive or negative.
                       | R1 KnotDiagram EdgeIndex Bool Bool
                         --The inverse to a R1 move defined by a diagram and a region 
                         -- Bounded by only one edge  
                       | R1Inv KnotDiagram RegionIndex
                         --A R2 move defined by a diagram and 2 edges bounding the same region
                         -- which the R2 move crosses.
                       | R2 KnotDiagram EdgeIndex EdgeIndex
                         --The inverse to a R2 move defined by a diagram and a region 
                         -- Bounded by 2 edges which are separable.
                       | R2Inv KnotDiagram RegionIndex
                         --A R3 move defined by a diagram, a region bounded by 3 edges
                         -- In the R3 form and a bounding edge to cross over.
                       | R3 KnotDiagram RegionIndex EdgeIndex
                       
reidermeisterSource :: ReidermeisterMove -> KnotDiagram
reidermeisterSource (R0 iso) = source iso
reidermeisterSource (R1 s _ _ _) = s
reidermeisterSource (R1Inv s _) = s
reidermeisterSource (R2 s _ _) = s
reidermeisterSource (R2Inv s _) = s
reidermeisterSource (R3 s _ _) = s
    
--Helper function given a map gives a list of indices not in the map
newIndices :: IMap.IntMap a -> [Int]
newIndices map | IMap.null map = [1..] -- For a null map we may not be able to find a maximum
               | otherwise = [maxIndex+1..]
  where maxIndex = fst $ IMap.findMax map
    
reidermeisterTarget :: ReidermeisterMove -> KnotDiagram 
reidermeisterTarget (R0 iso) = target iso
--For labels see diagrams 1->
--Perform an R1 move on the given diagram
-- edgeIn assumed valid
reidermeisterTarget (R1 knot edgeIn orient sign) = KnotDiagram crossings' edges' regions'
  where edge = edges knot IMap.! edgeIn
        --The crossing index at the end of the edge
        endCrossingIn = edgeEndCross edge
        --The region index into which the twist will be put depends on orientation
        -- And the opposite region
        regionTwistIn = if orient then edgeLeftRegion edge else edgeRightRegion edge
        regionOtherIn = if orient then edgeRightRegion edge else edgeLeftRegion edge
        --new indices for the new edges region and crossing
        newCrossingIn = head . newIndices $ crossings knot
        [newEdge1In, newEdge2In] = take 2 . newIndices $ crossings knot
        newRegionIn = head . newIndices $ regions knot
        --New edges region and crossing
        -- Region of the interior of the loop
        newRegion = Region $ Set.fromList [newEdge1In]
        --New crossing at base of twist depends on sign and orientation.
        newCrossingM = case (orient, sign) of
                       (True, True)->crossingFromEdges (newEdge1In, newEdge1In, newEdge2In, endCrossingIn)
                       (True, False)->crossingFromEdges (endCrossingIn, newEdge1In, newEdge1In, newEdge2In)
                       (False, True)->crossingFromEdges (endCrossingIn, newEdge2In, newEdge1In, newEdge1In)
                       (False, False)->crossingFromEdges (newEdge1In, endCrossingIn, newEdge2In, newEdge1In)
        newCrossing = fromJust newCrossingM
        --New edges making up the twist and connecting twist to end crossing
        -- On the twisted loop the orientation effects the ordering of the edges
        newEdge1 = if orient then Edge newCrossingIn newCrossingIn newRegionIn regionTwistIn
                             else Edge newCrossingIn newCrossingIn regionTwistIn newRegionIn
        newEdge2 = Edge newCrossingIn endCrossingIn (edgeLeftRegion edge) (edgeRightRegion edge)
        --Modify exisiting edges, end crossing and regions
        --On the end crossing swap the incoming edge edgeIn with newEdge2In
        modifyEndCrossing = fromJust . swapEdge edgeIn newEdge2In $ crossings knot IMap.! endCrossingIn
        --On the given edge change the end crossing from endCrossingIn to newCrossingIn
        modifyEdge = edge { edgeEndCross = newCrossingIn }
        --On the twisting region add both new edges
        newEdgesSet = Set.fromList [newEdge1In, newEdge2In] 
        modifyTwistRegion = Region $ newEdgesSet `Set.union` regionEdges (regions knot IMap.! regionTwistIn)
        --For other region just insert second new edge
        modifyOtherRegion = Region $ newEdge2In `Set.insert` regionEdges (regions knot IMap.! regionOtherIn)
        --Insert / update crossings, edges and regions maps
        crossings' = IMap.insert endCrossingIn modifyEndCrossing 
                     . IMap.insert newCrossingIn newCrossing
                     $ crossings knot
        edges' = IMap.insert edgeIn modifyEdge
                 . IMap.insert newEdge1In newEdge1
                 . IMap.insert newEdge2In newEdge2
                 $ edges knot
        regions' = IMap.insert regionTwistIn modifyTwistRegion
                   . IMap.insert regionOtherIn modifyOtherRegion
                   . IMap.insert newRegionIn newRegion
                   $ regions knot
--Stub out other reidermeiser moves.
reidermeisterTarget _ = undefined -- 
                   
                   
--A morphism of knot diagrams that is a isomorphism of knots
  -- Is a sequence of Reidermiester moves such that the source and target
  -- match up.
  -- We cache the source and target of the morphism.
data KnotIsomorphism = KnotIso {
  knotIsoSource :: KnotDiagram,
  knotIsoTarget :: KnotDiagram,
  knotIsoMoves :: [ReidermeisterMove]
}
           
