module Main where

import Foreign.Storable
import Numeric (showHex)
import Softfloat

printHex a = putStrLn $ "0x" ++ showHex a ""

main = do
  let F32Result fa fa_flags = ui32ToF32 RoundMin 0xffffffff
  printHex fa
  print fa_flags
