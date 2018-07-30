module Main where

import Foreign.Storable
import Numeric (showHex)
import Softfloat

printHex a = putStrLn $ "0x" ++ showHex a ""

main = do
  ef <- peek exceptionFlags
  printHex ef

  fa <- ui32_to_f32 0x2
  fb <- ui32_to_f32 0x3

  -- fres <- f32_mul fa fb
  -- printHex fres

  printHex (f32Val $ f32Mul fa fb)

  ef' <- peek exceptionFlags
  printHex ef'
