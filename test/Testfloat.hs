module Main where

import Ops
--import SoftFloat
--import Data.Word
import System.Process (readProcess)
import Data.List.Split
import Control.Monad (forM_)
--import Numeric (readHex, showIntAtBase)
--import Data.Char (intToDigit)
--import Data.Bits
import System.Exit

logFile :: String
logFile = "out.log"

genPath :: String
genPath = "lib/testfloat_gen"

baseArgs :: [String]
baseArgs = ["-level","1","-exact","-seed","1"]

-- full test:
-- TODO: add command line options, such as seed etc
-- TODO: merge with upstream
floats :: [String]
floats = ["f16", "f32", "f64"]

ints :: [String]
ints = ["ui32","i32","i64","ui64"]

ops  :: [String]
ops = ["add","sub","div","rem","sqrt","le","mulAdd",
       "eq","lt","eq_signaling","le_quiet","lt_quiet"]

intToFloat :: [String]
intToFloat = [ i ++ "_to_" ++ f | i <- ints, f <- floats]

floatToInt :: [String]
floatToInt = [ f ++ "_to_" ++ i | i <- ints, f <- floats]

floatToFloat :: [String]
floatToFloat = ["f16_to_f32", "f16_to_f64",
                "f32_to_f16","f32_to_f64",
                "f64_to_f16","f64_to_f32"]

floatOps :: [String]
floatOps = [ f ++ "_" ++ op | f <- floats, op <- ops]

arithmeticOps :: [String]
arithmeticOps = intToFloat ++ floatToFloat ++ floatToInt ++ floatOps

roundings :: [String]
roundings = ["near_even", "minMag","min","max","near_maxMag","odd"]

-- create all permutations of the tests
createArgs :: [[String]]
createArgs = [ [op] ++ ["-r" ++ rounding] ++ baseArgs 
            | rounding <- roundings, op <- arithmeticOps]

main :: IO ()
main = do
    forM_ createArgs $ \(args) -> do
        putStrLn $ show args
        _ <- readProcess "lib/run_testfloat_gen.sh" ([genPath] ++ args) []
        tests <- readFile logFile
        putStrLn $ "Generated test cases: " ++ show (length $ lines tests)
        let testCases = zip (lines tests) [1..] :: [(String, Integer)]
        forM_ testCases $ \(testData, testNumber) -> do
            let splitData = (splitOn " " testData)
            let operands = init splitData
            let opArgs = (take 2 args)
            let typeArg = head args

            let op = parseOp operands opArgs

            let expectedResult = readResult (drop (length splitData -2) $ splitData) typeArg
            let sfResult = executeOp op

            --let hwInput = opArgs ++ operands
            --putStrLn $ "HW: " ++ show hwInput
            --res <- readProcess "test/fenv/hw_float" hwInput []
            --putStrLn $ "HW returned: " ++ res
            --putStrLn $ "When split: " ++ show (splitOn " " res)
            --let hwResult = readResult (init $ splitOn " " res) typeArg
            --putStrLn $ "Hw result: " ++ show hwResult
            --if (sfResult /= expectedResult) || (hwResult /= expectedResult)
            if sfResult /= expectedResult
            then do
                putStrLn $ "Test " ++ show testNumber ++ " fails: "    
                putStrLn testData
                putStrLn $ show op
                putStrLn $ "Expected results:     " ++ show expectedResult
                --putStrLn $ "Hw results:           " ++ show hwResult
                putStrLn $ "Softfloat-hs results: " ++ show sfResult
                exitFailure
            else do
                return ()

