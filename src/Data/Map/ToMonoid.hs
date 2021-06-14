{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Map.ToMonoid (insert, (!), lookup, ) where

import qualified Data.Map as Map
import Data.Monoid ( Monoid(mempty) )
import Data.Semigroup (Semigroup((<>)))
import Data.Ord ( Ord )
import Data.Function (flip)
import Data.Eq(Eq(..))

newtype Map k a = Mmap {dmap :: Map.Map k a}
  deriving newtype (Eq, Ord)

insert :: (Monoid a, Ord k) => k -> a -> Map k a -> Map k a
insert k a (Mmap m) = Mmap (Map.insertWith (<>) k a m)

(!) :: (Monoid a, Ord k) => Map k a -> k -> a
m ! k = Map.findWithDefault mempty k (dmap m)

lookup :: (Monoid a, Ord k) => k -> Map k a -> a
lookup = flip (!)

instance (Semigroup a, Ord k) => Semigroup (Map k a) where
  Mmap l <> Mmap r = Mmap (Map.unionWith (<>) l r) 

instance (Monoid a, Ord k) => Monoid (Map k a) where
  mempty = Mmap mempty