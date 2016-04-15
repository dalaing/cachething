{-# LANGUAGE MultiParamTypeClasses #-}
module MapLike where

import Id

class MapLike m v where
  member :: Id v -> m -> Bool
  filter :: (v -> Bool) -> m -> m
  insert :: HasId v => v -> m -> m
  lookup :: Id v -> m -> Maybe v
  delete :: Id v -> m -> m

