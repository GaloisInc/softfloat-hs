module Main where

import           Ops
import           System.Process                 ( readProcess )
import           Data.List.Split
import           Control.Monad                  ( forM_, when )
import           System.Exit
import qualified Options.Applicative           as O
import           Data.Semigroup                 ( (<>) )
import qualified System.Random                 as R

floats :: [String]
floats = ["f16", "f32", "f64"]

ints :: [String]
ints = ["ui32", "i32", "i64", "ui64"]

ops :: [String]
ops =
  [ "add"
  , "sub"
  , "div"
  , "rem"
  , "sqrt"
  , "le"
  , "mulAdd"
  , "eq"
  , "lt"
  , "eq_signaling"
  , "le_quiet"
  , "lt_quiet"
  ]

intToFloat :: [String]
intToFloat = [ i ++ "_to_" ++ f | i <- ints, f <- floats ]

floatToInt :: [String]
floatToInt = [ f ++ "_to_" ++ i | i <- ints, f <- floats ]

floatToFloat :: [String]
floatToFloat =
  [ "f16_to_f32"
  , "f16_to_f64"
  , "f32_to_f16"
  , "f32_to_f64"
  , "f64_to_f16"
  , "f64_to_f32"
  ]

floatOps :: [String]
floatOps = [ f ++ "_" ++ op | f <- floats, op <- ops ]

arithmeticOps :: [String]
arithmeticOps = intToFloat ++ floatToFloat ++ floatToInt ++ floatOps

roundings :: [String]
roundings = ["near_even", "minMag", "min", "max", "near_maxMag", "odd"]

-- create all permutations of the tests
createArgs :: IO [[String]]
createArgs = do
  cmdOpts <- O.execParser opts
  s       <- mySeed cmdOpts
  let baseArgs = ["-exact", "-n", show $ tests cmdOpts, "-seed", show s]
  let args =
        [ [op] ++ ["-r" ++ rounding] ++ baseArgs
        | rounding <- roundings
        , op       <- arithmeticOps
        ]
  return args
 where
  opts = O.info
    (appOptions O.<**> O.helper)
    (O.fullDesc <> O.progDesc descString <> O.header
      "Tesftloat for softfloat-hs"
    )
  mySeed cmdOpts = if seed cmdOpts == 0
    then R.randomRIO (1, 65535)
    else return (seed cmdOpts)

data AppOptions = AppOptions
  { seed :: Int
  , tests    :: Int
  }

appOptions :: O.Parser AppOptions
appOptions =
  AppOptions
    <$> O.option
          O.auto
          (O.long "seed" <> O.short 's' <> O.metavar "S" <> O.value 0 <> O.help
            "Initial seed for the test generator"
          )
    <*> O.option
          O.auto
          (  O.long "tests"
          <> O.short 'n'
          <> O.metavar "TESTS"
          <> O.value 6133248
          <> O.help "Number of generated test vectors for each operation"
          )

descString :: String
descString =
  "Generate test vectors using testfloat and execute them agains softloat-hs, "
    ++ "optionally the hardware implementation."
    ++ "If seed is zero or non-specified, a random number is generated."

main :: IO ()
main = do
  progArgs <- createArgs
  _ <- readProcess "make" ["testfloat"] ""
  forM_ progArgs $ \args -> do
    testVectors <- readProcess "berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen" args []
    putStrLn $ "Generated test cases: " ++ show (length $ lines testVectors)
    let testCases = zip (lines testVectors) [1 ..] :: [(String, Integer)]
    forM_ testCases $ \(testData, testNumber) -> do
      let splitData = (splitOn " " testData)
      let operands  = init splitData
      let opArgs    = (take 2 args)
      let typeArg   = head args

      let op        = parseOp operands opArgs

      let expectedResult =
            readResult (drop (length splitData - 2) $ splitData) typeArg
      let sfResult = executeOp op

      when (sfResult /= expectedResult) $ do
        putStrLn $ "Test " ++ show testNumber ++ " fails: "
        putStrLn testData
        print op
        putStrLn $ "Expected results:     " ++ display expectedResult
        putStrLn $ "Softfloat-hs results: " ++ display sfResult
        exitFailure

