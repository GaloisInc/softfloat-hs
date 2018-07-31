module Softfloat.Internal
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

  , f32_mul
  ) where

import Data.Int
import Data.Word
import Foreign.Ptr

foreign import ccall "softfloat.h &softfloat_exceptionFlags" exceptionFlags :: Ptr Word8
foreign import ccall "softfloat.h &softfloat_roundingMode"   roundingMode   :: Ptr Word8

-- Integer to float conversion routines

foreign import ccall "ui32_to_f16" ui32_to_f16 :: Word32 -> IO Word16
foreign import ccall "ui32_to_f32" ui32_to_f32 :: Word32 -> IO Word32
foreign import ccall "ui32_to_f64" ui32_to_f64 :: Word32 -> IO Word64

foreign import ccall "ui64_to_f16" ui64_to_f16 :: Word64 -> IO Word16
foreign import ccall "ui64_to_f32" ui64_to_f32 :: Word64 -> IO Word32
foreign import ccall "ui64_to_f64" ui64_to_f64 :: Word64 -> IO Word64

foreign import ccall "i32_to_f16" i32_to_f16 :: Int32 -> IO Word16
foreign import ccall "i32_to_f32" i32_to_f32 :: Int32 -> IO Word32
foreign import ccall "i32_to_f64" i32_to_f64 :: Int32 -> IO Word64

foreign import ccall "i64_to_f16" i64_to_f16 :: Int64 -> IO Word16
foreign import ccall "i64_to_f32" i64_to_f32 :: Int64 -> IO Word32
foreign import ccall "i64_to_f64" i64_to_f64 :: Int64 -> IO Word64

-- Float to integer conversion routines

foreign import ccall "f16_to_ui32" f16_to_ui32 :: Word16 -> Word8 -> Int -> IO Word32
foreign import ccall "f16_to_ui64" f16_to_ui64 :: Word16 -> Word8 -> Int -> IO Word64
foreign import ccall "f16_to_i32"  f16_to_i32  :: Word16 -> Word8 -> Int -> IO Int32
foreign import ccall "f16_to_i64"  f16_to_i64  :: Word16 -> Word8 -> Int -> IO Int64

foreign import ccall "f32_to_ui32" f32_to_ui32 :: Word32 -> Word8 -> Int -> IO Word32
foreign import ccall "f32_to_ui64" f32_to_ui64 :: Word32 -> Word8 -> Int -> IO Word64
foreign import ccall "f32_to_i32"  f32_to_i32  :: Word32 -> Word8 -> Int -> IO Int32
foreign import ccall "f32_to_i64"  f32_to_i64  :: Word32 -> Word8 -> Int -> IO Int64

foreign import ccall "f64_to_ui32" f64_to_ui32 :: Word64 -> Word8 -> Int -> IO Word32
foreign import ccall "f64_to_ui64" f64_to_ui64 :: Word64 -> Word8 -> Int -> IO Word64
foreign import ccall "f64_to_i32"  f64_to_i32  :: Word64 -> Word8 -> Int -> IO Int32
foreign import ccall "f64_to_i64"  f64_to_i64  :: Word64 -> Word8 -> Int -> IO Int64

-- Float to float conversion routines

foreign import ccall "f16_to_f32" f16_to_f32 :: Word16 -> IO Word32
foreign import ccall "f16_to_f64" f16_to_f64 :: Word16 -> IO Word64
foreign import ccall "f32_to_f16" f32_to_f16 :: Word32 -> IO Word16
foreign import ccall "f32_to_f64" f32_to_f64 :: Word32 -> IO Word64
foreign import ccall "f64_to_f16" f64_to_f16 :: Word64 -> IO Word16
foreign import ccall "f64_to_f32" f64_to_f32 :: Word64 -> IO Word32

-- float16_t f16_roundToInt( float16_t, uint_fast8_t, bool );
-- float16_t f16_add( float16_t, float16_t );
-- float16_t f16_sub( float16_t, float16_t );
-- float16_t f16_mul( float16_t, float16_t );
-- float16_t f16_mulAdd( float16_t, float16_t, float16_t );
-- float16_t f16_div( float16_t, float16_t );
-- float16_t f16_rem( float16_t, float16_t );
-- float16_t f16_sqrt( float16_t );
-- bool f16_eq( float16_t, float16_t );
-- bool f16_le( float16_t, float16_t );
-- bool f16_lt( float16_t, float16_t );
-- bool f16_eq_signaling( float16_t, float16_t );
-- bool f16_le_quiet( float16_t, float16_t );
-- bool f16_lt_quiet( float16_t, float16_t );
-- bool f16_isSignalingNaN( float16_t );

-- float32_t f32_roundToInt( float32_t, uint_fast8_t, bool );
-- float32_t f32_add( float32_t, float32_t );
-- float32_t f32_sub( float32_t, float32_t );
-- float32_t f32_mul( float32_t, float32_t );
-- float32_t f32_mulAdd( float32_t, float32_t, float32_t );
-- float32_t f32_div( float32_t, float32_t );
-- float32_t f32_rem( float32_t, float32_t );
-- float32_t f32_sqrt( float32_t );
-- bool f32_eq( float32_t, float32_t );
-- bool f32_le( float32_t, float32_t );
-- bool f32_lt( float32_t, float32_t );
-- bool f32_eq_signaling( float32_t, float32_t );
-- bool f32_le_quiet( float32_t, float32_t );
-- bool f32_lt_quiet( float32_t, float32_t );
-- bool f32_isSignalingNaN( float32_t );

-- float64_t f64_roundToInt( float64_t, uint_fast8_t, bool );
-- float64_t f64_add( float64_t, float64_t );
-- float64_t f64_sub( float64_t, float64_t );
-- float64_t f64_mul( float64_t, float64_t );
-- float64_t f64_mulAdd( float64_t, float64_t, float64_t );
-- float64_t f64_div( float64_t, float64_t );
-- float64_t f64_rem( float64_t, float64_t );
-- float64_t f64_sqrt( float64_t );
-- bool f64_eq( float64_t, float64_t );
-- bool f64_le( float64_t, float64_t );
-- bool f64_lt( float64_t, float64_t );
-- bool f64_eq_signaling( float64_t, float64_t );
-- bool f64_le_quiet( float64_t, float64_t );
-- bool f64_lt_quiet( float64_t, float64_t );
-- bool f64_isSignalingNaN( float64_t );

foreign import ccall "f32_mul" f32_mul :: Word32 -> Word32 -> IO Word32
