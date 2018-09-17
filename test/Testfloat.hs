module Main where

import Ops
import SoftFloat
import Data.Word
import System.Process (readProcess)
import Data.List.Split
import Control.Monad (forM_)
import Numeric (readHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits
import System.Exit

logFile = "out.log"
genPath = "lib/testfloat_gen"
baseArgs = ["-n","100000","-exact","-seed","1"]

-- full test:
-- TODO: add convert
-- TODO: add command line options, such as seed etc
-- TODO: clean FPGen test
-- TODO: merge with upstream
{--
widths = ["f16","f32","f64"]
arithmeticOps = ["add","sub","div","rem","sqrt","le",
                 "eq","lt","eq_signaling","le_quiet","lt_quiet"]
roundings = ["near_even", "minMag","min","max","near_maxMag","odd"]
--}

-- mini test:
widths = ["f32"]
arithmeticOps = ["add","sub","div","rem","sqrt","le",
                 "eq","lt","eq_signaling","le_quiet","lt_quiet"]
roundings = ["near_even", "minMag","min","max","near_maxMag","odd"]

-- create all permutations of the tests
createArgs = [ [width ++ "_" ++ op] ++ ["-r" ++ rounding] ++ baseArgs 
            | width <- widths, rounding <- roundings, op <- arithmeticOps]

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

