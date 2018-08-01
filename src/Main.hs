module Main where

import Foreign.Storable
import Numeric (showHex)
import SoftFloat

printHex a = putStrLn $ "0x" ++ showHex a ""

main = do
  let F64Result fa fa_flags = ui64ToF64 RoundMin 1
      F64Result fb fb_flags = ui64ToF64 RoundMin 7
      F64Result fc fc_flags = f64Div RoundMin fa fb
  printHex fa
  print fa_flags
  printHex fb
  print fb_flags
  printHex fc
  print fc_flags
