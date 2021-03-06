{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , C.null
  , C.replicateM
  , C.rnf
  , new
  , index
  , index#
  , indexM
  , read
  , write
  , resize
  , C.size
  , C.sizeMutable
  , freeze
  , C.unsafeFreeze
  , copy
  , copyMutable
  , clone
  , cloneMutable
  , thaw
  , C.equals
  , C.unlift
  , C.lift
    -- * Synthetic Functions
  , C.singleton
  , C.doubleton
  , C.tripleton
  , C.append
  , C.map
  , C.map'
  , C.imap
  , C.foldr
  , C.foldl'
  , C.foldr'
  , C.foldMap
  , C.foldMap'
  , C.foldlM'
  , C.traverse
  , C.traverse_
  , C.itraverse_
  , C.imapMutable'
  , C.foldlMap'
  , C.ifoldlMap'
  , C.ifoldlMap1'
  , C.traverseP
  , unsafeFromListN
  , unsafeFromListReverseN
  , C.liftHashWithSalt
  , C.same
  , filter
  ) where

import Prelude hiding (map,read,filter)

import "contiguous" Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import Control.Exception (ArrayException(..),toException)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.ST (ST,runST)
import Data.Primitive (MutablePrimArray,readPrimArray,writePrimArray,newPrimArray)
import Data.Word (Word8)
import GHC.Exts (Int(I#),raise#,dataToTag#)
import GHC.Stack (HasCallStack,prettyCallStack,callStack)

import qualified "contiguous" Data.Primitive.Contiguous as C
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = raise# (toException $ IndexOutOfBounds $ "Data.Primitive.Contiguous." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

check# :: HasCallStack => String -> Bool -> (# a #) -> (# a #)
check# _      True  x = x
check# errMsg False _ = raise# (toException $ IndexOutOfBounds $ "Data.Primitive.Contiguous." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

new :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Int -> m (Mutable arr (PrimState m) b)
new n = check "new: negative size" (n>=0) (C.new n)

thaw :: (HasCallStack, Contiguous arr, PrimMonad m, Element arr b) => arr b -> Int -> Int -> m (Mutable arr (PrimState m) b)
thaw arr off len = check ("thaw: out of bounds [off=" ++ show off ++ ",len=" ++ show len ++ "]")
  (off >= 0 && len >= 0 && off + len <= sz)
  (C.thaw arr off len)
  where
  sz = C.size arr

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

read :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> m b
read marr i = do
  sz <- C.sizeMutable marr
  check ("read: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
    (i >= 0 && i < sz)
    (C.read marr i)

write :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> b -> m ()
write marr i x = do
  sz <- C.sizeMutable marr
  check ("write: out of bounds [ix=" ++ show i ++ ",sz=" ++ show sz ++ "]")
    (i >= 0 && i < sz)
    (C.write marr i x)

resize :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> m (Mutable arr (PrimState m) b)
resize marr n = check "resize: negative size" (n>=0) (C.resize marr n)

freeze :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> Int -> m (arr b)
freeze marr off len = do
  sz <- C.sizeMutable marr
  let explain = L.concat
        [ "[sz="
        , show sz
        , ",off="
        , show off
        , ",len="
        , show len
        , "]"
        ]
  check ("freeze: index range out of bounds " ++ explain)
    (off>=0 && (off+len)<=sz)
    (C.freeze marr off len)

copy :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> arr b -> Int -> Int -> m ()
copy marr s1 arr s2 l = do
  sz <- C.sizeMutable marr
  let explain = L.concat
        [ "[dst size: "
        , show sz
        , ", dst off: " 
        , show s1
        , ", src size: "
        , show (C.size arr)
        , ", src off: " 
        , show s2
        , ", copy size: "
        , show l
        , "]"
        ]
  check ("copy: index range out of bounds " ++ explain)
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=C.size arr && (s1+l)<=sz)
    (C.copy marr s1 arr s2 l)

copyMutable :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> Mutable arr (PrimState m) b -> Int -> Int -> m ()
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
clone arr s l = check ("clone: index range out of bounds [ix=" ++ show s ++ ",len=" ++ show l ++ ",sz=" ++ show (C.size arr) ++ "]")
  (s>=0 && l>=0 && (s+l)<=C.size arr)
  (C.clone arr s l)

cloneMutable :: (HasCallStack, Contiguous arr, Element arr b, PrimMonad m) => Mutable arr (PrimState m) b -> Int -> Int -> m (Mutable arr (PrimState m) b)
cloneMutable marr s l = do
  siz <- C.sizeMutable marr
  check "cloneMutable: index range out of bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (C.cloneMutable marr s l)

unsafeFromListN :: (Contiguous arr, Element arr a)
  => Int -- ^ length of list
  -> [a] -- ^ list
  -> arr a
unsafeFromListN expected xs =
  let actual = length xs
   in check
        ("unsafeFromListN: given length " ++ show expected ++ " did not match actual length " ++ show actual)
        (actual == expected)
        (C.unsafeFromListN expected xs)

unsafeFromListReverseN :: (Contiguous arr, Element arr a)
  => Int -- ^ length of list
  -> [a] -- ^ list
  -> arr a
unsafeFromListReverseN expected xs =
  let actual = length xs
   in check
        ("unsafeFromListReverseN: given length " ++ show expected ++ " did not match actual length " ++ show actual)
        (actual == expected)
        (C.unsafeFromListReverseN expected xs)

-- | Drop elements that do not satisfy the predicate.
filter :: (Contiguous arr, Element arr a)
  => (a -> Bool)
  -> arr a
  -> arr a
filter p arr = ifilter (\_ a -> p a) arr
{-# inline filter #-}

-- | Drop elements that do not satisfy the predicate which
--   is applied to values and their indices.
ifilter :: (Contiguous arr, Element arr a)
  => (Int -> a -> Bool)
  -> arr a
  -> arr a
ifilter p arr = runST $ do
  marr :: MutablePrimArray s Word8 <- newPrimArray sz
  let go1 :: Int -> Int -> ST s Int
      go1 !ix !numTrue = if ix < sz
        then do
          atIx <- indexM arr ix
          let !keep = p ix atIx
          let !keepTag = I# (dataToTag# keep)
          writePrimArray marr ix (fromIntegral keepTag)
          go1 (ix + 1) (numTrue + keepTag)
        else pure numTrue
  numTrue <- go1 0 0
  if numTrue == sz
    then pure arr
    else do
      marrTrues <- new numTrue
      let go2 !ixSrc !ixDst = if ixDst < numTrue
            then do
              atIxKeep <- readPrimArray marr ixSrc
              if isTrue atIxKeep
                then do
                  atIxVal <- C.indexM arr ixSrc
                  C.write marrTrues ixDst atIxVal
                  go2 (ixSrc + 1) (ixDst + 1)
                else go2 (ixSrc + 1) ixDst
            else pure ()
      go2 0 0
      C.unsafeFreeze marrTrues
  where
    !sz = C.size arr
{-# inline ifilter #-}

isTrue :: Word8 -> Bool
isTrue 0 = False
isTrue _ = True
