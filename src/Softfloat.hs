
{-|
Module      : Softfloat
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This library provides a pure function interface to John Hauser's softfloat C library,
supported by underlying impure FFI calls.
-}

module Softfloat
  (
    -- * Result of floating-point computations
    F16Result(..)
  , F32Result(..)
  , F64Result(..)
  , ExceptionFlags(..)

    -- * Fixed-width integer -> floating point conversion

  , ui32ToF16
  , ui32ToF32
  , ui32ToF64

  , i32ToF16
  , i32ToF32
  , i32ToF64

  , ui64ToF16
  , ui64ToF32
  , ui64ToF64

  , i64ToF16
  , i64ToF32
  , i64ToF64

  -- * Floating point operations
  , f32Mul
  ) where

import Control.Concurrent.MVar
import Data.Bits
import Data.Word
import Foreign.Storable
import System.IO.Unsafe

import Softfloat.Internal

softfloatLock :: MVar ()
softfloatLock = unsafePerformIO (newMVar ())
{-# NOINLINE softfloatLock #-}

-- | 16-bit floating point result.
data F16Result = F16Result Word16 ExceptionFlags

-- | 32-bit floating point result.
data F32Result = F32Result Word32 ExceptionFlags

-- | 64-bit floating point result.
data F64Result = F64Result Word64 ExceptionFlags

-- | Exception flags returned by a floating point computation.
data ExceptionFlags = ExceptionFlags
  { inexact   :: Bool
  , underflow :: Bool
  , overflow  :: Bool
  , infinite  :: Bool
  , invalid   :: Bool
  } deriving (Show)

-- The doSoftfloatXX wrappers perform unsafe IO calls to the underlying Haskell FFI
-- calls in Softfloat.Internal. These functions handle the obtaining of the
-- softfloatLock MVar to guard against data races with the global variables, and they
-- also access the exception flags and return them as part of the result.
doSoftfloat16 :: IO Word16 -> F16Result
doSoftfloat16 ioRes = unsafePerformIO $ withMVar softfloatLock $ \_ -> do
  poke exceptionFlags 0x0
  res <- ioRes
  flags <- peek exceptionFlags
  return $ F16Result res $ ExceptionFlags
    (flags .&. 0x1  == 1)
    (flags .&. 0x2  == 1)
    (flags .&. 0x4  == 1)
    (flags .&. 0x8  == 1)
    (flags .&. 0x10 == 1)

doSoftfloat32 :: IO Word32 -> F32Result
doSoftfloat32 ioRes = unsafePerformIO $ withMVar softfloatLock $ \_ -> do
  poke exceptionFlags 0x0
  res <- ioRes
  flags <- peek exceptionFlags
  return $ F32Result res $ ExceptionFlags
    (flags .&. 0x1  == 1)
    (flags .&. 0x2  == 1)
    (flags .&. 0x4  == 1)
    (flags .&. 0x8  == 1)
    (flags .&. 0x10 == 1)

doSoftfloat64 :: IO Word64 -> F64Result
doSoftfloat64 ioRes = unsafePerformIO $ withMVar softfloatLock $ \_ -> do
  poke exceptionFlags 0x0
  res <- ioRes
  flags <- peek exceptionFlags
  return $ F64Result res $ ExceptionFlags
    (flags .&. 0x1  == 1)
    (flags .&. 0x2  == 1)
    (flags .&. 0x4  == 1)
    (flags .&. 0x8  == 1)
    (flags .&. 0x10 == 1)

-- | Unsigned 32-bit integer to 16-bit float.
ui32ToF16 :: Word32 -> F16Result
ui32ToF16 a = doSoftfloat16 (ui32_to_f16 a)

-- | Unsigned 32-bit integer to 32-bit float.
ui32ToF32 :: Word32 -> F32Result
ui32ToF32 a = doSoftfloat32 (ui32_to_f32 a)

-- | Unsigned 32-bit integer to 64-bit float.
ui32ToF64 :: Word32 -> F64Result
ui32ToF64 a = doSoftfloat64 (ui32_to_f64 a)

-- | Signed 32-bit integer to 16-bit float.
i32ToF16 :: Word32 -> F16Result
i32ToF16 a = doSoftfloat16 (i32_to_f16 a)

-- | Signed 32-bit integer to 32-bit float.
i32ToF32 :: Word32 -> F32Result
i32ToF32 a = doSoftfloat32 (i32_to_f32 a)

-- | Signed 32-bit integer to 64-bit float.
i32ToF64 :: Word32 -> F64Result
i32ToF64 a = doSoftfloat64 (i32_to_f64 a)

-- | Unsigned 64-bit integer to 16-bit float.
ui64ToF16 :: Word64 -> F16Result
ui64ToF16 a = doSoftfloat16 (ui64_to_f16 a)

-- | Unsigned 64-bit integer to 32-bit float.
ui64ToF32 :: Word64 -> F32Result
ui64ToF32 a = doSoftfloat32 (ui64_to_f32 a)

-- | Unsigned 64-bit integer to 64-bit float.
ui64ToF64 :: Word64 -> F64Result
ui64ToF64 a = doSoftfloat64 (ui64_to_f64 a)

-- | Signed 64-bit integer to 16-bit float.
i64ToF16 :: Word64 -> F16Result
i64ToF16 a = doSoftfloat16 (i64_to_f16 a)

-- | Signed 64-bit integer to 32-bit float.
i64ToF32 :: Word64 -> F32Result
i64ToF32 a = doSoftfloat32 (i64_to_f32 a)

-- | Signed 64-bit integer to 64-bit float.
i64ToF64 :: Word64 -> F64Result
i64ToF64 a = doSoftfloat64 (i64_to_f64 a)

-- | Multiplication of 32-bit floats.
f32Mul :: Word32 -> Word32 -> F32Result
f32Mul fa fb = doSoftfloat32 (f32_mul fa fb)
