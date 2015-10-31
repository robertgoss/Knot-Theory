module Indices where

--A class for various classes that well be used for indices and labelling
-- to pay well with underlying int maps without losing typing
-- note the wrapper for int allows us to break type safty
-- and the indices generated in this way are not assured to be genuine.
-- Unless we do (unsafeWrap . unwrap)
class Index a where
  unwrap :: a -> Int
  unsafeWrap :: Int -> a
  
instance Index Int where
  unwrap = id
  unsafeWrap = id