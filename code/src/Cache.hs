{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache where

import Prelude hiding (lookup, filter)

import Data.Type.Equality
import Data.Type.Bool
import Data.Proxy

import Id
import ObjectCache
import MapLike
import Diffable

data Cache (o :: [*]) where
  CEmpty :: Cache '[]
  CAdd :: ObjectCache a -> Cache o -> Cache (a ': o)

instance (h ~ (x == z), MapLikeHelper x y z h) => MapLike (Cache (x ': y)) z where
  member   = member' (Proxy :: Proxy h)
  filter   = filter' (Proxy :: Proxy h)
  insert   = insert' (Proxy :: Proxy h)
  lookup   = lookup' (Proxy :: Proxy h)
  delete   = delete' (Proxy :: Proxy h)

class MapLikeHelper x y z h where
  member' ::            Proxy h -> Id z -> Cache (x ': y) -> Bool
  filter' ::            Proxy h -> (z -> Bool) -> Cache (x ': y) -> Cache (x ': y)
  insert' :: HasId z => Proxy h -> z -> Cache (x ': y) -> Cache (x ': y)
  lookup' ::            Proxy h -> Id z -> Cache (x ': y) -> Maybe z
  delete' ::            Proxy h -> Id z -> Cache (x ': y) -> Cache (x ': y)

instance (True ~ (x == z) , MapLike (ObjectCache x) z) => MapLikeHelper x y z True where
  member' _ i (CAdd oc _) = member i oc
  filter' _ p (CAdd oc c) = CAdd (filter p oc) c
  insert' _ x (CAdd oc c) = CAdd (insert x oc) c
  lookup' _ i (CAdd oc _) = lookup i oc
  delete' _ i (CAdd oc c) = CAdd (delete i oc) c

instance (False ~ (x == z), MapLike (Cache y) z) => MapLikeHelper x y z False where
  member' _ i (CAdd _ c)  = member i c
  filter' _ p (CAdd oc c) = CAdd oc (filter p c)
  insert' _ x (CAdd oc c) = CAdd oc (insert x c)
  lookup' _ i (CAdd _  c) = lookup i c
  delete' _ i (CAdd oc c) = CAdd oc (delete i c)

data Diff (a :: [*]) where
  DEmpty :: Diff '[]
  DAdd :: (Eq a, Show a) => [ObjectDiff a] -> Diff o -> Diff (a ': o) 

instance Show (Diff a) where
    show DEmpty = ""
    show (DAdd h t) = 
      let 
        next = show t
      in
        if null next 
          then show h
        else 
          show h ++ " " ++ show t

instance (Eq a, Show a, Diffable (Cache o), Patch (Cache o) ~ Diff o) => Diffable (Cache (a ': o)) where
  type Patch (Cache (a ': o)) = Diff (a ': o)
  diff (CAdd o1 n1) (CAdd o2 n2) = DAdd (diff o1 o2) (diff n1 n2)
  patch (DAdd d1 d2) (CAdd c1 c2) = CAdd (patch d1 c1) (patch d2 c2)
  patchMay (DAdd d1 d2) (CAdd c1 c2) = CAdd <$> patchMay d1 c1 <*> patchMay d2 c2

instance Diffable (Cache '[]) where
  type Patch (Cache '[]) = Diff '[]
  diff _ _ = DEmpty
  patch _ _ = CEmpty
  patchMay _ _ = Just CEmpty

