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
  , f32_to_ui32

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

foreign import ccall "f32_to_ui32" f32_to_ui32 :: Word32 -> Word8 -> Int -> IO Word32

-- uint_fast32_t f32_to_ui32( float32_t, uint_fast8_t, bool );
-- uint_fast64_t f32_to_ui64( float32_t, uint_fast8_t, bool );
-- int_fast32_t f32_to_i32( float32_t, uint_fast8_t, bool );
-- int_fast64_t f32_to_i64( float32_t, uint_fast8_t, bool );
-- uint_fast32_t f32_to_ui32_r_minMag( float32_t, bool );
-- uint_fast64_t f32_to_ui64_r_minMag( float32_t, bool );
-- int_fast32_t f32_to_i32_r_minMag( float32_t, bool );
-- int_fast64_t f32_to_i64_r_minMag( float32_t, bool );
-- float16_t f32_to_f16( float32_t );
-- float64_t f32_to_f64( float32_t );
-- #ifdef SOFTFLOAT_FAST_INT64
-- extFloat80_t f32_to_extF80( float32_t );
-- float128_t f32_to_f128( float32_t );
-- #endif
-- void f32_to_extF80M( float32_t, extFloat80_t * );
-- void f32_to_f128M( float32_t, float128_t * );
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

foreign import ccall "f32_mul" f32_mul :: Word32 -> Word32 -> IO Word32
