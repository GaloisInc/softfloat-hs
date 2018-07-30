
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
  , Ui16Result(..)
  , Ui32Result(..)
  , Ui64Result(..)
  , I16Result(..)
  , I32Result(..)
  , I64Result(..)
  , ExceptionFlags(..)

  -- * Rounding
  , RoundingMode(..)

    -- * Fixed-width integer to floating point conversions
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

  -- * Floating point to fixed-width integer conversions
  , f32ToUi32

  -- * Floating point operations
  , f32Mul
  ) where

import Control.Concurrent
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Storable
import System.IO.Unsafe

import Softfloat.Internal

-- softfloatLock :: MVar ()
-- softfloatLock = unsafePerformIO (newMVar ())
-- {-# NOINLINE softfloatLock #-}

-- | 16-bit unsigned integer result.
data Ui16Result = Ui16Result Word16 ExceptionFlags

-- | 32-bit unsigned integer result.
data Ui32Result = Ui32Result Word32 ExceptionFlags

-- | 32-bit unsigned integer result.
data Ui64Result = Ui64Result Word64 ExceptionFlags

-- | 16-bit signed integer result.
data I16Result = I16Result Int16 ExceptionFlags

-- | 32-bit signed integer result.
data I32Result = I32Result Int32 ExceptionFlags

-- | 32-bit signed integer result.
data I64Result = I64Result Int64 ExceptionFlags

-- | 16-bit floating point result.
data F16Result = F16Result Word16 ExceptionFlags

-- | 32-bit floating point result.
data F32Result = F32Result Word32 ExceptionFlags

-- | 64-bit floating point result.
data F64Result = F64Result Word64 ExceptionFlags

-- | Data type for specifying rounding mode to a floating point computation.
data RoundingMode = RoundNearEven
                  | RoundMinMag
                  | RoundMin
                  | RoundMax
                  | RoundNearMaxMag
                  | RoundOdd
  deriving (Enum)

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
doSoftfloat :: (a -> ExceptionFlags -> b) -> RoundingMode -> IO a -> b
doSoftfloat constructor rm ioRes = unsafePerformIO $ runInBoundThread $ do
  poke exceptionFlags 0x0
  poke roundingMode (fromIntegral $ fromEnum rm)
  res <- ioRes
  flags <- peek exceptionFlags
  return $ constructor res $ ExceptionFlags
    (flags .&. 0x1  == 1)
    (flags .&. 0x2  == 1)
    (flags .&. 0x4  == 1)
    (flags .&. 0x8  == 1)
    (flags .&. 0x10 == 1)

doSoftfloatF16 :: RoundingMode -> IO Word16 -> F16Result
doSoftfloatF16 = doSoftfloat F16Result

doSoftfloatF32 :: RoundingMode -> IO Word32 -> F32Result
doSoftfloatF32 = doSoftfloat F32Result

doSoftfloatF64 :: RoundingMode -> IO Word64 -> F64Result
doSoftfloatF64 = doSoftfloat F64Result

doSoftfloatUi32 :: RoundingMode -> IO Word32 -> Ui32Result
doSoftfloatUi32 = doSoftfloat Ui32Result

----------------------------------------------------------------------
-- Integer to float conversions

-- | Unsigned 32-bit integer to 16-bit float.
ui32ToF16 :: RoundingMode -> Word32 -> F16Result
ui32ToF16 rm a = doSoftfloatF16 rm (ui32_to_f16 a)

-- | Unsigned 32-bit integer to 32-bit float.
ui32ToF32 :: RoundingMode -> Word32 -> F32Result
ui32ToF32 rm a = doSoftfloatF32 rm (ui32_to_f32 a)

-- | Unsigned 32-bit integer to 64-bit float.
ui32ToF64 :: RoundingMode -> Word32 -> F64Result
ui32ToF64 rm a = doSoftfloatF64 rm (ui32_to_f64 a)

-- | Signed 32-bit integer to 16-bit float.
i32ToF16 :: RoundingMode -> Int32 -> F16Result
i32ToF16 rm a = doSoftfloatF16 rm (i32_to_f16 a)

-- | Signed 32-bit integer to 32-bit float.
i32ToF32 :: RoundingMode -> Int32 -> F32Result
i32ToF32 rm a = doSoftfloatF32 rm (i32_to_f32 a)

-- | Signed 32-bit integer to 64-bit float.
i32ToF64 :: RoundingMode -> Int32 -> F64Result
i32ToF64 rm a = doSoftfloatF64 rm (i32_to_f64 a)

-- | Unsigned 64-bit integer to 16-bit float.
ui64ToF16 :: RoundingMode -> Word64 -> F16Result
ui64ToF16 rm a = doSoftfloatF16 rm (ui64_to_f16 a)

-- | Unsigned 64-bit integer to 32-bit float.
ui64ToF32 :: RoundingMode -> Word64 -> F32Result
ui64ToF32 rm a = doSoftfloatF32 rm (ui64_to_f32 a)

-- | Unsigned 64-bit integer to 64-bit float.
ui64ToF64 :: RoundingMode -> Word64 -> F64Result
ui64ToF64 rm a = doSoftfloatF64 rm (ui64_to_f64 a)

-- | Signed 64-bit integer to 16-bit float.
i64ToF16 :: RoundingMode -> Int64 -> F16Result
i64ToF16 rm a = doSoftfloatF16 rm (i64_to_f16 a)

-- | Signed 64-bit integer to 32-bit float.
i64ToF32 :: RoundingMode -> Int64 -> F32Result
i64ToF32 rm a = doSoftfloatF32 rm (i64_to_f32 a)

-- | Signed 64-bit integer to 64-bit float.
i64ToF64 :: RoundingMode -> Int64 -> F64Result
i64ToF64 rm a = doSoftfloatF64 rm (i64_to_f64 a)

----------------------------------------------------------------------
-- Float to integer conversions
f32ToUi32 :: RoundingMode -> Word32 -> Ui32Result
f32ToUi32 rm fa = doSoftfloatUi32 rm (f32_to_ui32 fa (fromIntegral $ fromEnum rm) 0x1)

----------------------------------------------------------------------
-- 32-bit operations


-- | Multiplication of 32-bit floats.
f32Mul :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Mul rm fa fb = doSoftfloatF32 rm (f32_mul fa fb)
