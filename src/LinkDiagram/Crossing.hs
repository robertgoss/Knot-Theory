{-# LANGUAGE DeriveFunctor #-}
module LinkDiagram.Crossing where

--The type of information associated to a crossing in terms of the indices of the edges
-- This takes into account loops, it edges that start and end at the same crossing 
-- at this crossing as these are harder to orient in code.
--Deals with the local situation at the edges
data Crossing edgeIndex 
              = Crossing edgeIndex edgeIndex edgeIndex edgeIndex
                --Standard crossing with 4 distinct edges oriented clockwise 
                -- From the incoming undercrossing edge
                | LoopCrossingTL edgeIndex edgeIndex edgeIndex
                | LoopCrossingTR edgeIndex edgeIndex edgeIndex
                | LoopCrossingBL edgeIndex edgeIndex edgeIndex
                | LoopCrossingBR edgeIndex edgeIndex edgeIndex
                --A crossing with a single loop and in the 
                -- given quadrant labeled with the first edge index
                -- The remaining 2 distinct edges are given clockwise 
                | DoubleLoopL edgeIndex edgeIndex
                | DoubleLoopR edgeIndex edgeIndex
                -- A crossing that contains 2 loops the first contains
                -- the incomming undercrossing edge right or left
                -- indicate if the outgoing part of this edge if left or right
        deriving(Eq,Ord,Show,Functor)

--Change the index types used in crossing
-- The index changing function needs to be bijective.
indexChange :: (edgeIndex1 -> edgeIndex2) -> Crossing edgeIndex1 -> Crossing edgeIndex2
indexChange = fmap

--Get the edge indices appearing in the crossing in clockwise order from the 
-- first undercrossing edge. There may be duplicates due to loops.
--This is how the crossing appears in a planar diagram.
edgeIndices :: Crossing edgeindex -> [edgeindex]
edgeIndices (Crossing e1 e2 e3 e4) = [e1,e2,e3,e4]
edgeIndices (LoopCrossingTL loop e1 e2) = [e1,loop,loop,e2]
edgeIndices (LoopCrossingTR loop e1 e2) = [e1,e2,loop,loop]
edgeIndices (LoopCrossingBL loop e1 e2) = [loop,loop,e1,e2]
edgeIndices (LoopCrossingBR loop e1 e2) = [loop,e1,e2,loop]
edgeIndices (DoubleLoopL loop1 loop2) = [loop1,loop1,loop2,loop2]
edgeIndices (DoubleLoopR loop1 loop2) = [loop1,loop2,loop2,loop1]

--The number of times the crossing meets an edge given by an index. Is given by 0, 1 or 2
edgeMeetingNumber :: (Eq edgeindex) => Crossing edgeindex -> edgeindex -> Int
edgeMeetingNumber crossing edgeIndex = length . filter (==edgeIndex) $ edgeIndices crossing 

--If a given incoming and outgoing edge meet as opposites in the given crossing
oppositeEdges :: (Eq edgeIndex) => Crossing edgeIndex -> edgeIndex -> edgeIndex -> Bool
oppositeEdges (Crossing e1 e2 e3 e4) inEdge outEdge
                      --Posible combinations on in out opposite pairs - note that e1 is always incoming
                      | e1 == inEdge && e3 == outEdge = True
                      | e2 == inEdge && e4 == outEdge = True
                      | e4 == inEdge && e2 == outEdge = True
                      | otherwise = False
oppositeEdges (LoopCrossingTL loop e1 e2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | e1 == inEdge && loop == outEdge = True
                      | loop == inEdge && e2 == outEdge = True
                      | otherwise = False
oppositeEdges (LoopCrossingTR loop e1 e2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | e1 == inEdge && loop == outEdge = True
                      | loop == inEdge && e2 == outEdge = True
                      | otherwise = False
oppositeEdges (LoopCrossingBL loop e1 e2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | loop == inEdge && e1 == outEdge = True
                      | e2 == inEdge && loop == outEdge = True
                      | otherwise = False
oppositeEdges (LoopCrossingBR loop e1 e2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | loop == inEdge && e2 == outEdge = True
                      | e1 == inEdge && loop == outEdge = True
                      | otherwise = False
oppositeEdges (DoubleLoopL loop1 loop2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | loop1 == inEdge && loop2 == outEdge = True
                      | loop2 == inEdge && loop1 == outEdge = True
                      | otherwise = False
oppositeEdges (DoubleLoopR loop1 loop2) inEdge outEdge
                      --Posible combinations on in out pairs - note that only 2 possible pairings due to loop
                      | loop1 == inEdge && loop2 == outEdge = True
                      | loop2 == inEdge && loop1 == outEdge = True
                      | otherwise = False