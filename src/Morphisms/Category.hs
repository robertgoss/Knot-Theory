{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Morphisms.Category where

import Data.Maybe(fromJust)

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