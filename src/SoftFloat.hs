
{-|
Module      : SoftFloat
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : None (yet)
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This library provides a pure function interface to John Hauser's softfloat C library,
supported by underlying impure FFI calls.
-}

module SoftFloat
  (
    -- * Result of floating-point computations
    Result(..)
  , F16Result
  , F32Result
  , F64Result
  , Ui32Result
  , Ui64Result
  , I32Result
  , I64Result
  , CBoolResult
  , ExceptionFlags

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
  , f16ToUi32
  , f16ToUi64
  , f16ToI32
  , f16ToI64

  , f32ToUi32
  , f32ToUi64
  , f32ToI32
  , f32ToI64

  , f64ToUi32
  , f64ToUi64
  , f64ToI32
  , f64ToI64

  -- * Floating point to floating point conversions
  , f16ToF32
  , f16ToF64
  , f32ToF16
  , f32ToF64
  , f64ToF16
  , f64ToF32

  -- * 16-bit Floating point operations
  , f16RoundToInt
  , f16Add
  , f16Sub
  , f16Mul
  , f16MulAdd
  , f16Div
  , f16Rem
  , f16Sqrt
  , f16Eq
  , f16Le
  , f16Lt
  , f16EqSignaling
  , f16LeQuiet
  , f16LtQuiet
  , f16IsSignalingNaN

  -- * 32-bit Floating point operations
  , f32RoundToInt
  , f32Add
  , f32Sub
  , f32Mul
  , f32Div
  , f32Rem
  , f32Sqrt
  , f32Eq
  , f32Le
  , f32Lt
  , f32EqSignaling
  , f32LeQuiet
  , f32LtQuiet
  , f32IsSignalingNaN

  -- * 64-bit Floating point operations
  , f64RoundToInt
  , f64Add
  , f64Sub
  , f64Mul
  , f64Div
  , f64Rem
  , f64Sqrt
  , f64Eq
  , f64Le
  , f64Lt
  , f64EqSignaling
  , f64LeQuiet
  , f64LtQuiet
  , f64IsSignalingNaN

  ) where

import Control.Concurrent
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import System.IO.Unsafe

import SoftFloat.Internal

-- softfloatLock :: MVar ()
-- softfloatLock = unsafePerformIO (newMVar ())
-- {-# NOINLINE softfloatLock #-}

-- | Result of a floating point operation.
data Result a = Result a ExceptionFlags

-- | 32-bit unsigned integer result.
type Ui32Result = Result Word32

-- | 64-bit unsigned integer result.
type Ui64Result = Result Word64

-- | 32-bit signed integer result.
type I32Result = Result Int32

-- | 64-bit signed integer result.
type I64Result = Result Int64

-- | 16-bit floating point result.
type F16Result = Result Word16

-- | 32-bit floating point result.
type F32Result = Result Word32

-- | 64-bit floating point result.
type F64Result = Result Word64

-- | Boolean result.
type CBoolResult = Result CBool

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

-- The doSoftFloatXX wrappers perform unsafe IO calls to the underlying Haskell FFI
-- calls in SoftFloat.Internal. These functions handle the obtaining of the
-- softfloatLock MVar to guard against data races with the global variables, and they
-- also access the exception flags and return them as part of the result.
doSoftFloat :: RoundingMode -> IO a -> Result a
doSoftFloat rm ioRes = unsafePerformIO $ runInBoundThread $ do
  poke exceptionFlags 0x0
  poke roundingMode (fromIntegral $ fromEnum rm)
  res <- ioRes
  flags <- peek exceptionFlags
  return $ Result res $ ExceptionFlags
    (flags .&. 0x1  /= 0x0)
    (flags .&. 0x2  /= 0x0)
    (flags .&. 0x4  /= 0x0)
    (flags .&. 0x8  /= 0x0)
    (flags .&. 0x10 /= 0x0)

----------------------------------------------------------------------
-- Integer to float conversions

-- | Unsigned 32-bit integer to 16-bit float.
ui32ToF16 :: RoundingMode -> Word32 -> F16Result
ui32ToF16 rm a = doSoftFloat rm (ui32_to_f16 a)

-- | Unsigned 32-bit integer to 32-bit float.
ui32ToF32 :: RoundingMode -> Word32 -> F32Result
ui32ToF32 rm a = doSoftFloat rm (ui32_to_f32 a)

-- | Unsigned 32-bit integer to 64-bit float.
ui32ToF64 :: RoundingMode -> Word32 -> F64Result
ui32ToF64 rm a = doSoftFloat rm (ui32_to_f64 a)

-- | Signed 32-bit integer to 16-bit float.
i32ToF16 :: RoundingMode -> Int32 -> F16Result
i32ToF16 rm a = doSoftFloat rm (i32_to_f16 a)

-- | Signed 32-bit integer to 32-bit float.
i32ToF32 :: RoundingMode -> Int32 -> F32Result
i32ToF32 rm a = doSoftFloat rm (i32_to_f32 a)

-- | Signed 32-bit integer to 64-bit float.
i32ToF64 :: RoundingMode -> Int32 -> F64Result
i32ToF64 rm a = doSoftFloat rm (i32_to_f64 a)

-- | Unsigned 64-bit integer to 16-bit float.
ui64ToF16 :: RoundingMode -> Word64 -> F16Result
ui64ToF16 rm a = doSoftFloat rm (ui64_to_f16 a)

-- | Unsigned 64-bit integer to 32-bit float.
ui64ToF32 :: RoundingMode -> Word64 -> F32Result
ui64ToF32 rm a = doSoftFloat rm (ui64_to_f32 a)

-- | Unsigned 64-bit integer to 64-bit float.
ui64ToF64 :: RoundingMode -> Word64 -> F64Result
ui64ToF64 rm a = doSoftFloat rm (ui64_to_f64 a)

-- | Signed 64-bit integer to 16-bit float.
i64ToF16 :: RoundingMode -> Int64 -> F16Result
i64ToF16 rm a = doSoftFloat rm (i64_to_f16 a)

-- | Signed 64-bit integer to 32-bit float.
i64ToF32 :: RoundingMode -> Int64 -> F32Result
i64ToF32 rm a = doSoftFloat rm (i64_to_f32 a)

-- | Signed 64-bit integer to 64-bit float.
i64ToF64 :: RoundingMode -> Int64 -> F64Result
i64ToF64 rm a = doSoftFloat rm (i64_to_f64 a)

----------------------------------------------------------------------
-- Float to integer conversions
f16ToUi32 :: RoundingMode -> Word16 -> Ui32Result
f16ToUi32 rm fa = doSoftFloat rm (f16_to_ui32 fa (fromIntegral $ fromEnum rm) 0x1)

f16ToUi64 :: RoundingMode -> Word16 -> Ui64Result
f16ToUi64 rm fa = doSoftFloat rm (f16_to_ui64 fa (fromIntegral $ fromEnum rm) 0x1)

f16ToI32 :: RoundingMode -> Word16 -> I32Result
f16ToI32 rm fa = doSoftFloat rm (f16_to_i32 fa (fromIntegral $ fromEnum rm) 0x1)

f16ToI64 :: RoundingMode -> Word16 -> I64Result
f16ToI64 rm fa = doSoftFloat rm (f16_to_i64 fa (fromIntegral $ fromEnum rm) 0x1)

f32ToUi32 :: RoundingMode -> Word32 -> Ui32Result
f32ToUi32 rm fa = doSoftFloat rm (f32_to_ui32 fa (fromIntegral $ fromEnum rm) 0x1)

f32ToUi64 :: RoundingMode -> Word32 -> Ui64Result
f32ToUi64 rm fa = doSoftFloat rm (f32_to_ui64 fa (fromIntegral $ fromEnum rm) 0x1)

f32ToI32 :: RoundingMode -> Word32 -> I32Result
f32ToI32 rm fa = doSoftFloat rm (f32_to_i32 fa (fromIntegral $ fromEnum rm) 0x1)

f32ToI64 :: RoundingMode -> Word32 -> I64Result
f32ToI64 rm fa = doSoftFloat rm (f32_to_i64 fa (fromIntegral $ fromEnum rm) 0x1)

f64ToUi32 :: RoundingMode -> Word64 -> Ui32Result
f64ToUi32 rm fa = doSoftFloat rm (f64_to_ui32 fa (fromIntegral $ fromEnum rm) 0x1)

f64ToUi64 :: RoundingMode -> Word64 -> Ui64Result
f64ToUi64 rm fa = doSoftFloat rm (f64_to_ui64 fa (fromIntegral $ fromEnum rm) 0x1)

f64ToI32 :: RoundingMode -> Word64 -> I32Result
f64ToI32 rm fa = doSoftFloat rm (f64_to_i32 fa (fromIntegral $ fromEnum rm) 0x1)

f64ToI64 :: RoundingMode -> Word64 -> I64Result
f64ToI64 rm fa = doSoftFloat rm (f64_to_i64 fa (fromIntegral $ fromEnum rm) 0x1)

----------------------------------------------------------------------
-- Float to float conversions

f16ToF32 :: RoundingMode -> Word16 -> F32Result
f16ToF32 rm fa = doSoftFloat rm (f16_to_f32 fa)

f16ToF64 :: RoundingMode -> Word16 -> F64Result
f16ToF64 rm fa = doSoftFloat rm (f16_to_f64 fa)

f32ToF16 :: RoundingMode -> Word32 -> F16Result
f32ToF16 rm fa = doSoftFloat rm (f32_to_f16 fa)

f32ToF64 :: RoundingMode -> Word32 -> F64Result
f32ToF64 rm fa = doSoftFloat rm (f32_to_f64 fa)

f64ToF16 :: RoundingMode -> Word64 -> F16Result
f64ToF16 rm fa = doSoftFloat rm (f64_to_f16 fa)

f64ToF32 :: RoundingMode -> Word64 -> F32Result
f64ToF32 rm fa = doSoftFloat rm (f64_to_f32 fa)

----------------------------------------------------------------------
-- 16-bit operations

f16RoundToInt :: RoundingMode -> Word16 -> F16Result
f16RoundToInt rm fa = doSoftFloat rm (f16_roundToInt fa (fromIntegral $ fromEnum rm) 0x1)

f16Add :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Add rm fa fb = doSoftFloat rm (f16_add fa fb)

f16Sub :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Sub rm fa fb = doSoftFloat rm (f16_sub fa fb)

f16Mul :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Mul rm fa fb = doSoftFloat rm (f16_mul fa fb)

f16MulAdd :: RoundingMode -> Word16 -> Word16 -> Word16 -> F16Result
f16MulAdd rm fa fb fc = doSoftFloat rm (f16_mulAdd fa fb fc)

f16Div :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Div rm fa fb = doSoftFloat rm (f16_div fa fb)

f16Rem :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Rem rm fa fb = doSoftFloat rm (f16_rem fa fb)

f16Sqrt :: RoundingMode -> Word16 -> F16Result
f16Sqrt rm fa = doSoftFloat rm (f16_sqrt fa)

f16Eq :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16Eq rm fa fb = doSoftFloat rm (f16_eq fa fb)

f16Le :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16Le rm fa fb = doSoftFloat rm (f16_le fa fb)

f16Lt :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16Lt rm fa fb = doSoftFloat rm (f16_lt fa fb)

f16EqSignaling :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16EqSignaling rm fa fb = doSoftFloat rm (f16_eq_signaling fa fb)

f16LeQuiet :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16LeQuiet rm fa fb = doSoftFloat rm (f16_le_quiet fa fb)

f16LtQuiet :: RoundingMode -> Word16 -> Word16 -> CBoolResult
f16LtQuiet rm fa fb = doSoftFloat rm (f16_lt_quiet fa fb)

f16IsSignalingNaN :: RoundingMode -> Word16 -> CBoolResult
f16IsSignalingNaN rm fa = doSoftFloat rm (f16_isSignalingNaN fa)

----------------------------------------------------------------------
-- 32-bit operations

f32RoundToInt :: RoundingMode -> Word32 -> F32Result
f32RoundToInt rm fa = doSoftFloat rm (f32_roundToInt fa (fromIntegral $ fromEnum rm) 0x1)

f32Add :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Add rm fa fb = doSoftFloat rm (f32_add fa fb)

f32Sub :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Sub rm fa fb = doSoftFloat rm (f32_sub fa fb)

f32Mul :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Mul rm fa fb = doSoftFloat rm (f32_mul fa fb)

f32MulAdd :: RoundingMode -> Word32 -> Word32 -> Word32 -> F32Result
f32MulAdd rm fa fb fc = doSoftFloat rm (f32_mulAdd fa fb fc)

f32Div :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Div rm fa fb = doSoftFloat rm (f32_div fa fb)

f32Rem :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Rem rm fa fb = doSoftFloat rm (f32_rem fa fb)

f32Sqrt :: RoundingMode -> Word32 -> F32Result
f32Sqrt rm fa = doSoftFloat rm (f32_sqrt fa)

f32Eq :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32Eq rm fa fb = doSoftFloat rm (f32_eq fa fb)

f32Le :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32Le rm fa fb = doSoftFloat rm (f32_le fa fb)

f32Lt :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32Lt rm fa fb = doSoftFloat rm (f32_lt fa fb)

f32EqSignaling :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32EqSignaling rm fa fb = doSoftFloat rm (f32_eq_signaling fa fb)

f32LeQuiet :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32LeQuiet rm fa fb = doSoftFloat rm (f32_le_quiet fa fb)

f32LtQuiet :: RoundingMode -> Word32 -> Word32 -> CBoolResult
f32LtQuiet rm fa fb = doSoftFloat rm (f32_lt_quiet fa fb)

f32IsSignalingNaN :: RoundingMode -> Word32 -> CBoolResult
f32IsSignalingNaN rm fa = doSoftFloat rm (f32_isSignalingNaN fa)

----------------------------------------------------------------------
-- 64-bit operations

f64RoundToInt :: RoundingMode -> Word64 -> F64Result
f64RoundToInt rm fa = doSoftFloat rm (f64_roundToInt fa (fromIntegral $ fromEnum rm) 0x1)

f64Add :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Add rm fa fb = doSoftFloat rm (f64_add fa fb)

f64Sub :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Sub rm fa fb = doSoftFloat rm (f64_sub fa fb)

f64Mul :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Mul rm fa fb = doSoftFloat rm (f64_mul fa fb)

f64MulAdd :: RoundingMode -> Word64 -> Word64 -> Word64 -> F64Result
f64MulAdd rm fa fb fc = doSoftFloat rm (f64_mulAdd fa fb fc)

f64Div :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Div rm fa fb = doSoftFloat rm (f64_div fa fb)

f64Rem :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Rem rm fa fb = doSoftFloat rm (f64_rem fa fb)

f64Sqrt :: RoundingMode -> Word64 -> F64Result
f64Sqrt rm fa = doSoftFloat rm (f64_sqrt fa)

f64Eq :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64Eq rm fa fb = doSoftFloat rm (f64_eq fa fb)

f64Le :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64Le rm fa fb = doSoftFloat rm (f64_le fa fb)

f64Lt :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64Lt rm fa fb = doSoftFloat rm (f64_lt fa fb)

f64EqSignaling :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64EqSignaling rm fa fb = doSoftFloat rm (f64_eq_signaling fa fb)

f64LeQuiet :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64LeQuiet rm fa fb = doSoftFloat rm (f64_le_quiet fa fb)

f64LtQuiet :: RoundingMode -> Word64 -> Word64 -> CBoolResult
f64LtQuiet rm fa fb = doSoftFloat rm (f64_lt_quiet fa fb)

f64IsSignalingNaN :: RoundingMode -> Word64 -> CBoolResult
f64IsSignalingNaN rm fa = doSoftFloat rm (f64_isSignalingNaN fa)
