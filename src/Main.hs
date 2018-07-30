module Main where

import Foreign.Storable
import Numeric (showHex)
import Softfloat

printHex a = putStrLn $ "0x" ++ showHex a ""

main = do
  let F32Result fa _ = ui32ToF32 RoundMin 0x10000001
      Ui32Result a a_flags = f32ToUi32 RoundMin fa
  printHex a
  print a_flags
