{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Data.Primitive.Contiguous
  ( C.Contiguous
  , C.Element
  , C.Mutable
  , C.Always
    -- * Primitives
  , C.empty
  , new
  , index
  , index#
  , indexM
  , read
  , write
  , resize
  , C.size
  , C.sizeMutable
  , C.unsafeFreeze
  , copy
  , copyMutable
  , clone
  , cloneMutable
  , C.foldr
  , C.equals
  , C.unlift
  , C.lift
    -- * Synthetic Functions
  , C.map
  , C.foldl'
  , C.foldr'
  , C.foldMap'
  ) where

import Prelude hiding (map,read)

import "contiguous" Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import Control.Exception (ArrayException(..),toException)
import Control.Monad.ST (ST)
import GHC.Exts (raise#)
import GHC.Stack (HasCallStack,prettyCallStack,callStack)

import qualified "contiguous" Data.Primitive.Contiguous as C
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = raise# (toException $ IndexOutOfBounds $ "Data.Primitive.Contiguous." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

check# :: HasCallStack => String -> Bool -> (# a #) -> (# a #)
check# _      True  x = x
check# errMsg False _ = raise# (toException $ IndexOutOfBounds $ "Data.Primitive.Contiguous." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

new :: (HasCallStack, Contiguous arr, Element arr b) => Int -> ST s (Mutable arr s b)
new n = check "new: negative size" (n>=0) (C.new n)

index :: (HasCallStack, Contiguous arr, Element arr b) => arr b -> Int -> b
index arr i = check ("index: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
  (i >= 0 && i < sz)
  (C.index arr i)
  where
  sz = C.size arr

index# :: (HasCallStack, Contiguous arr, Element arr b) => arr b -> Int -> (# b #)
index# arr i = check# ("index#: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
  (i >= 0 && i < sz)
  (C.index# arr i)
  where
  sz = C.size arr

indexM :: (HasCallStack, Contiguous arr, Element arr b, Monad m) => arr b -> Int -> m b
indexM arr i = check ("indexM: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
  (i >= 0 && i < sz)
  (C.indexM arr i)
  where
  sz = C.size arr

read :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> ST s b
read marr i = do
  sz <- C.sizeMutable marr
  check ("read: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
    (i >= 0 && i < sz)
    (C.read marr i)

write :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> b -> ST s ()
write marr i x = do
  sz <- C.sizeMutable marr
  check ("write: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
    (i >= 0 && i < sz)
    (C.write marr i x)

resize :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> ST s (Mutable arr s b)
resize marr n = check "resize: negative size" (n>=0) (C.resize marr n)

copy :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> arr b -> Int -> Int -> ST s ()
copy marr s1 arr s2 l = do
  sz <- C.sizeMutable marr
  check "copy: index range out of bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=C.size arr && (s1+l)<=sz)
    (C.copy marr s1 arr s2 l)

copyMutable :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> Mutable arr s b -> Int -> Int -> ST s ()
copyMutable marr1 s1 marr2 s2 l = do
  siz1 <- C.sizeMutable marr1
  siz2 <- C.sizeMutable marr2
  let explain = L.concat
        [ "[dst size: "
        , show siz1
        , ", dst off: " 
        , show s1
        , ", src size: "
        , show siz2
        , ", src off: " 
        , show s2
        , ", copy size: "
        , show l
        , "]"
        ]
  check ("copyMutableArray: index range out of bounds " ++ explain)
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (C.copyMutable marr1 s1 marr2 s2 l)

clone :: (HasCallStack, Contiguous arr, Element arr b) => arr b -> Int -> Int -> arr b
clone arr s l = check "clone: index range out of bounds"
  (s>=0 && l>=0 && (s+l)<=C.size arr)
  (C.clone arr s l)

cloneMutable :: (HasCallStack, Contiguous arr, Element arr b) => Mutable arr s b -> Int -> Int -> ST s (Mutable arr s b)
cloneMutable marr s l = do
  siz <- C.sizeMutable marr
  check "cloneMutable: index range out of bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (C.cloneMutable marr s l)


