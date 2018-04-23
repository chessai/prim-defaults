{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | The defaults provided here use other methods from the Prim typeclass.
-- If you are write a Prim instances for a data type that is a product, the
-- only sensible way to implement setByteArray# and setOffAddr# is to iterate.
-- There is no way to use memset in these situations
module Prim.Default
  ( defaultSetByteArray#
  , defaultSetOffAddr#
  , defaultPeekElemOff
  , defaultPokeElemOff
  ) where

import Data.Primitive
import Control.Monad.ST
import Control.Monad.Primitive
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.Int (Int(..))

{-# INLINE defaultSetByteArray# #-}
defaultSetByteArray# :: forall s a. Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
defaultSetByteArray# arr# i# len# ident = internal_ (go 0)
  where
  !len = I# len#
  !i = I# i#
  !arr = MutableByteArray arr#
  go :: Int -> ST s ()
  go !ix = if ix < len
    then writeByteArray arr (i + ix) ident >> go (i + 1)
    else return ()

{-# INLINE defaultSetOffAddr# #-}
defaultSetOffAddr# :: forall s a. Prim a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
defaultSetOffAddr# addr# i# len# ident = internal_ (go 0)
  where
  !len = I# len#
  !i = I# i#
  !addr = Addr addr#
  go :: Int -> ST s ()
  go !ix = if ix < len
    then writeOffAddr addr (i + ix) ident >> go (i + 1)
    else return ()

{-# INLINE defaultPeekElemOff #-}
defaultPeekElemOff :: Prim a => Ptr a -> Int -> IO a
defaultPeekElemOff (Ptr addr#) (I# i#) = primitive $ \s# ->
  readOffAddr# addr# i# s#

{-# INLINE defaultPokeElemOff #-}
defaultPokeElemOff :: Prim a => Ptr a -> Int -> a -> IO ()
defaultPokeElemOff (Ptr addr#) (I# i#) a = primitive_ $ \s# ->
  writeOffAddr# addr# i# a s#

{-# INLINE internal_ #-}
internal_ :: PrimBase m => m () -> State# (PrimState m) -> State# (PrimState m)
internal_ m s = case internal m s of
  (# s', () #) -> s'
