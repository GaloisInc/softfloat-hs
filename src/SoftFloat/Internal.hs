{-|
Module      : SoftFloat.Internal
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides the underlying impure FFI calls to softfloat.
-}

module SoftFloat.Internal
  ( -- * Global variables
    exceptionFlags
  , roundingMode

    -- * Integer to float conversion
  , ui32_to_f16
  , ui32_to_f32
  , ui32_to_f64

  , ui64_to_f16
  , ui64_to_f32
  , ui64_to_f64

  , i32_to_f16
  , i32_to_f32
  , i32_to_f64

  , i64_to_f16
  , i64_to_f32
  , i64_to_f64

  -- * Float to integer conversions

  , f16_to_ui32
  , f16_to_ui64
  , f16_to_i32
  , f16_to_i64

  , f32_to_ui32
  , f32_to_ui64
  , f32_to_i32
  , f32_to_i64

  , f64_to_ui32
  , f64_to_ui64
  , f64_to_i32
  , f64_to_i64

  -- * Float to float conversions

  , f16_to_f32
  , f16_to_f64
  , f32_to_f16
  , f32_to_f64
  , f64_to_f16
  , f64_to_f32

  -- * 16-bit floating point operations
  , f16_roundToInt
  , f16_add
  , f16_sub
  , f16_mul
  , f16_mulAdd
  , f16_div
  , f16_rem
  , f16_sqrt
  , f16_eq
  , f16_le
  , f16_lt
  , f16_eq_signaling
  , f16_le_quiet
  , f16_lt_quiet
  , f16_isSignalingNaN

  -- * 32-bit floating point operations
  , f32_roundToInt
  , f32_add
  , f32_sub
  , f32_mul
  , f32_mulAdd
  , f32_div
  , f32_rem
  , f32_sqrt
  , f32_eq
  , f32_le
  , f32_lt
  , f32_eq_signaling
  , f32_le_quiet
  , f32_lt_quiet
  , f32_isSignalingNaN

  -- * 64-bit floating point operations
  , f64_roundToInt
  , f64_add
  , f64_sub
  , f64_mul
  , f64_mulAdd
  , f64_div
  , f64_rem
  , f64_sqrt
  , f64_eq
  , f64_le
  , f64_lt
  , f64_eq_signaling
  , f64_le_quiet
  , f64_lt_quiet
  , f64_isSignalingNaN
  ) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

-- | Pointer to 'softfloat_exceptionFlags' global variable. Most floating point
-- operations update this variable.
exceptionFlags :: Ptr Word8
exceptionFlags = undefined
-- | Pointer to 'softfloat_roundingMode global variable. Most floating point
-- operations implicitly use this variable.
roundingMode   :: Ptr Word8
roundingMode = undefined

-- Integer to float conversion routines

ui32_to_f16 :: Word32 -> IO Word16
ui32_to_f16 = undefined
ui32_to_f32 :: Word32 -> IO Word32
ui32_to_f32 = undefined
ui32_to_f64 :: Word32 -> IO Word64
ui32_to_f64 = undefined

ui64_to_f16 :: Word64 -> IO Word16
ui64_to_f16 = undefined
ui64_to_f32 :: Word64 -> IO Word32
ui64_to_f32 = undefined
ui64_to_f64 :: Word64 -> IO Word64
ui64_to_f64 = undefined

i32_to_f16 :: Int32 -> IO Word16
i32_to_f16 = undefined
i32_to_f32 :: Int32 -> IO Word32
i32_to_f32 = undefined
i32_to_f64 :: Int32 -> IO Word64
i32_to_f64 = undefined

i64_to_f16 :: Int64 -> IO Word16
i64_to_f16 = undefined
i64_to_f32 :: Int64 -> IO Word32
i64_to_f32 = undefined
i64_to_f64 :: Int64 -> IO Word64
i64_to_f64 = undefined

-- Float to integer conversion routines

f16_to_ui32 :: Word16 -> Word8 -> CBool -> IO Word32
f16_to_ui32 = undefined
f16_to_ui64 :: Word16 -> Word8 -> CBool -> IO Word64
f16_to_ui64 = undefined
f16_to_i32  :: Word16 -> Word8 -> CBool -> IO Int32
f16_to_i32 = undefined
f16_to_i64  :: Word16 -> Word8 -> CBool -> IO Int64
f16_to_i64 = undefined

f32_to_ui32 :: Word32 -> Word8 -> CBool -> IO Word32
f32_to_ui32 = undefined
f32_to_ui64 :: Word32 -> Word8 -> CBool -> IO Word64
f32_to_ui64 = undefined
f32_to_i32  :: Word32 -> Word8 -> CBool -> IO Int32
f32_to_i32 = undefined
f32_to_i64  :: Word32 -> Word8 -> CBool -> IO Int64
f32_to_i64 = undefined

f64_to_ui32 :: Word64 -> Word8 -> CBool -> IO Word32
f64_to_ui32 = undefined
f64_to_ui64 :: Word64 -> Word8 -> CBool -> IO Word64
f64_to_ui64 = undefined
f64_to_i32  :: Word64 -> Word8 -> CBool -> IO Int32
f64_to_i32 = undefined
f64_to_i64  :: Word64 -> Word8 -> CBool -> IO Int64
f64_to_i64 = undefined

-- Float to float conversion routines

f16_to_f32 :: Word16 -> IO Word32
f16_to_f32 = undefined
f16_to_f64 :: Word16 -> IO Word64
f16_to_f64 = undefined
f32_to_f16 :: Word32 -> IO Word16
f32_to_f16 = undefined
f32_to_f64 :: Word32 -> IO Word64
f32_to_f64 = undefined
f64_to_f16 :: Word64 -> IO Word16
f64_to_f16 = undefined
f64_to_f32 :: Word64 -> IO Word32
f64_to_f32 = undefined


-- 16-bit operations
f16_roundToInt :: Word16 -> Word8 -> CBool -> IO Word16
f16_roundToInt = undefined
f16_add  :: Word16 -> Word16 -> IO Word16
f16_add = undefined
f16_sub  :: Word16 -> Word16 -> IO Word16
f16_sub = undefined
f16_mul  :: Word16 -> Word16 -> IO Word16
f16_mul = undefined
f16_mulAdd  :: Word16 -> Word16 -> Word16 -> IO Word16
f16_mulAdd = undefined
f16_div  :: Word16 -> Word16 -> IO Word16
f16_div = undefined
f16_rem  :: Word16 -> Word16 -> IO Word16
f16_rem = undefined
f16_sqrt :: Word16 -> IO Word16
f16_sqrt = undefined
f16_eq   :: Word16 -> Word16 -> IO CBool
f16_eq = undefined
f16_le   :: Word16 -> Word16 -> IO CBool
f16_le = undefined
f16_lt   :: Word16 -> Word16 -> IO CBool
f16_lt = undefined
f16_eq_signaling :: Word16 -> Word16 -> IO CBool
f16_eq_signaling = undefined
f16_le_quiet     :: Word16 -> Word16 -> IO CBool
f16_le_quiet = undefined
f16_lt_quiet     :: Word16 -> Word16 -> IO CBool
f16_lt_quiet = undefined
f16_isSignalingNaN :: Word16 -> IO CBool
f16_isSignalingNaN = undefined

f32_roundToInt :: Word32 -> Word8 -> CBool -> IO Word32
f32_roundToInt = undefined
f32_add  :: Word32 -> Word32 -> IO Word32
f32_add = undefined
f32_sub  :: Word32 -> Word32 -> IO Word32
f32_sub = undefined
f32_mul  :: Word32 -> Word32 -> IO Word32
f32_mul = undefined
f32_mulAdd  :: Word32 -> Word32 -> Word32 -> IO Word32
f32_mulAdd = undefined
f32_div  :: Word32 -> Word32 -> IO Word32
f32_div = undefined
f32_rem  :: Word32 -> Word32 -> IO Word32
f32_rem = undefined
f32_sqrt :: Word32 -> IO Word32
f32_sqrt = undefined
f32_eq   :: Word32 -> Word32 -> IO CBool
f32_eq = undefined
f32_le   :: Word32 -> Word32 -> IO CBool
f32_le = undefined
f32_lt   :: Word32 -> Word32 -> IO CBool
f32_lt = undefined
f32_eq_signaling :: Word32 -> Word32 -> IO CBool
f32_eq_signaling = undefined
f32_le_quiet     :: Word32 -> Word32 -> IO CBool
f32_le_quiet = undefined
f32_lt_quiet     :: Word32 -> Word32 -> IO CBool
f32_lt_quiet = undefined
f32_isSignalingNaN :: Word32 -> IO CBool
f32_isSignalingNaN = undefined

f64_roundToInt :: Word64 -> Word8 -> CBool -> IO Word64
f64_roundToInt = undefined
f64_add  :: Word64 -> Word64 -> IO Word64
f64_add = undefined
f64_sub  :: Word64 -> Word64 -> IO Word64
f64_sub = undefined
f64_mul  :: Word64 -> Word64 -> IO Word64
f64_mul = undefined
f64_mulAdd  :: Word64 -> Word64 -> Word64 -> IO Word64
f64_mulAdd = undefined
f64_div  :: Word64 -> Word64 -> IO Word64
f64_div = undefined
f64_rem  :: Word64 -> Word64 -> IO Word64
f64_rem = undefined
f64_sqrt :: Word64 -> IO Word64
f64_sqrt = undefined
f64_eq   :: Word64 -> Word64 -> IO CBool
f64_eq = undefined
f64_le   :: Word64 -> Word64 -> IO CBool
f64_le = undefined
f64_lt   :: Word64 -> Word64 -> IO CBool
f64_lt = undefined
f64_eq_signaling :: Word64 -> Word64 -> IO CBool
f64_eq_signaling = undefined
f64_le_quiet     :: Word64 -> Word64 -> IO CBool
f64_le_quiet = undefined
f64_lt_quiet     :: Word64 -> Word64 -> IO CBool
f64_lt_quiet = undefined
f64_isSignalingNaN :: Word64 -> IO CBool
f64_isSignalingNaN = undefined
