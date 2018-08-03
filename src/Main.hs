module Main where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Numeric (showHex)
import SoftFloat
import SoftFloat.Internal

printHex a b = putStrLn $ "0x" ++ showHex a "" ++ ",  " ++ show b

main = do
  let F32Result fa fa_flags = ui32ToF32 RoundMin 0x1
      F32Result fb fb_flags = ui32ToF32 RoundMin 0x3
      F32Result fc fc_flags = f32Div RoundMin fa fb
      F32Result fd fd_flags = f32Mul RoundMax fb fc
  printHex fa fa_flags
  printHex fb fb_flags
  printHex fc fc_flags
  printHex fd fd_flags

