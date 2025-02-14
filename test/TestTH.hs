{-# LANGUAGE TemplateHaskell #-}
-- | This is a simplistic test case that does nothing except load the code from
-- @softfloat-hs@ using Template Haskell. Doing so is an effective smoke test
-- to prevent build issues like the ones encountered in #9, where code would
-- fail to load into Template Haskell due to undeclared dependencies.
module Main (main) where

import SoftFloat (RoundingMode(..))

$(RoundNearEven `seq` return [])

main :: IO ()
main = return ()
