module Indices where

--A class for various classes that well be used for indices and labelling
-- to pay well with underlying int maps without losing typing
-- note we do not a a wrapper for ints just an unwrapper
class Index a where
  unwrap :: a -> Int
  
instance Index Int where
  unwrap = id