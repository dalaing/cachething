{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module ObjectCache where

import MapLike
import Diffable

import Id

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

type ObjectCache a = M.Map (Id a) a

instance MapLike (ObjectCache a) a where
  member = M.member
  filter = M.filter
  insert x = M.insert (getId x) x
  lookup = M.lookup
  delete = M.delete

data ObjectDiff a =
    Added (Id a) a
  | Removed (Id a)
  | Changed (Id a) a a
  deriving (Eq, Show)

instance Eq a => Diffable (ObjectCache a) where
  type Patch (ObjectCache a) = [ObjectDiff a] 

  diff o1 o2 = findAdded ++ findRemoved ++ findChanged
    where
      findAdded = map mkAdded . toList $ M.keysSet o2 `S.difference` M.keysSet o1
      mkAdded x = Added x (o2 M.! x)

      findRemoved = map Removed . toList $ M.keysSet o1 `S.difference` M.keysSet o2

      findChanged = mkChanged =<< toList (M.keysSet o1 `S.intersection` M.keysSet o2)
      mkChanged x = let
          v1 = o1 M.! x
          v2 = o2 M.! x
        in
          if v1 == v2
             then []
             else [Changed x v1 v2]

  patch p o = foldr patch1 o p
    where
      patch1 = undefined

  patchMay p o = foldrM patchMay1 o p
    where
      patchMay1 = undefined

{-
data JsonDiff a = JAdded (Id a) B.ByteString
                | JRemoved (Id a)
                | JChanged (Id a) B.ByteString
-}
