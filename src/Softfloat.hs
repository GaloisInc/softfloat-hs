module Softfloat
  ( module Softfloat.Internal
  , F32Result(..)
  , f32Mul
  ) where

import Control.Concurrent.MVar
import Data.Word
import Foreign.Storable
import System.IO.Unsafe

import Softfloat.Internal

softfloatLock :: MVar ()
softfloatLock = unsafePerformIO (newMVar ())
{-# NOINLINE softfloatLock #-}

data F32Result = F32Result
  { f32Val       :: Word32
  , f32Inexact   :: Bool
  , f32Underflow :: Bool
  , f32Overflow  :: Bool
  , f32Infinite  :: Bool
  , f32Invalid   :: Bool
  }

f32Mul :: Word32 -> Word32 -> F32Result
f32Mul fa fb =
  unsafePerformIO $
  withMVar softfloatLock $ \_ ->
  do res <- f32_mul fa fb
     poke exceptionFlags 0x0
     return F32Result {
       f32Val = res,
       f32Inexact = False,
       f32Underflow = False,
       f32Overflow = False,
       f32Infinite = False,
       f32Invalid = False
       }
