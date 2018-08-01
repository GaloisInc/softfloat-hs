module Main where

import Foreign.Storable
import Numeric (showHex)
import Softfloat

printHex a = putStrLn $ "0x" ++ showHex a ""

main = do
  let F32Result fa fa_flags = ui32ToF32 RoundMin 1
      F32Result fb fb_flags = ui32ToF32 RoundMin 0
      F32Result fc fc_flags = f32Div RoundMax fa fb
  printHex fa
  print fa_flags
  printHex fb
  print fb_flags
  printHex fc
  print fc_flags
