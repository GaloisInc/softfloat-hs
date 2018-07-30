module Softfloat
  ( module Softfloat.Internal
  , ExceptionFlags(..)
  , F16Result(..)
  , F32Result(..)
  , F64Result(..)
  , ui32ToF16
  , ui32ToF32
  , ui32ToF64
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

data ExceptionFlags = ExceptionFlags
  { inexact   :: Bool
  , underflow :: Bool
  , overflow  :: Bool
  , infinite  :: Bool
  , invalid   :: Bool
  } deriving (Show)

data F16Result = F16Result Word16 ExceptionFlags
data F32Result = F32Result Word32 ExceptionFlags
data F64Result = F64Result Word64 ExceptionFlags

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

ui32ToF16 :: Word32 -> F16Result
ui32ToF16 a = doSoftfloat16 (ui32_to_f16 a)

ui32ToF32 :: Word32 -> F32Result
ui32ToF32 a = doSoftfloat32 (ui32_to_f32 a)

ui32ToF64 :: Word32 -> F64Result
ui32ToF64 a = doSoftfloat64 (ui32_to_f64 a)

f32Mul :: Word32 -> Word32 -> F32Result
f32Mul fa fb = doSoftfloat32 (f32_mul fa fb)
