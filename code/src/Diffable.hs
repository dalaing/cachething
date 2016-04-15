{-# LANGUAGE TypeFamilies #-}
module Diffable where

class Diffable d where
  type Patch d :: *
  diff :: d -> d -> Patch d
  -- keeps on rolling
  -- - overwrites to apply an add that already exists
  -- - ignores removes or changes to things that don't exist
  patch :: Patch d -> d -> d
  -- fails if the patch doesn't make sense
  -- - adding something when it already exists
  -- - removing or changing something that doesn't exist
  -- TODO a better error type would be good
  patchMay :: Patch d -> d -> Maybe d

