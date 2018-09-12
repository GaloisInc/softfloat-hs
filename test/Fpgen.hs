module Main where

import SoftFloat
import System.Directory
--import System.Exit
import Control.Monad
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Data.Bits
import Data.Word
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Control.Applicative
import Data.List (isInfixOf)

data FloatType =   B32 -- b32 Word32
                 | B64 -- b64 Word64
                 -- Below: doesn't matter
                 | B128 -- b128 (Word64, Word64)
                 | D32 -- d32 -- Dont use decimals
                 | D64 -- d64
                 | D128 -- d128
    deriving (Show, Eq)

data FloatOperation = Add -- +
                    | Subtract -- -
                    | Multiply -- *
                    | Divide -- /
                    | MultiplyAdd -- *+
                    | SquareRoot -- V
                    | Remainder -- % NOT USED
                    | RoundFloatToInt -- rfi NOT USED
                    | ConvertFloatToFloat -- cff
                    | ConvertFloatToInt -- cfi NOT USED
                    | ConvertIntToFloat -- cif NOT USED
                    | ConvertToDecimalStr -- cfd NOT USED
                    | ConvertFromDecimalStr -- cdf NOT USED
                    | QuietComparison -- qC NOT USED
                    | SignallingComparsion -- sC NOT USED
                    | Copy -- cp
                    | Negate -- ~
                    | Abs -- A
                    | CopySign -- @ NOT USED
                    | ScalB -- S NOT USED
                    | LogB -- L NOT USED
                    | NextAfter -- Na NOT USED
                    | Class -- ? NOT USED
                    | IsSigned -- ?-
                    | IsNormal -- ?n
                    | IsFinite -- ?f for isfinite
                    | IsZero -- ?0
                    | IsSubNormal -- ?s for issubnormal
                    | IsInf -- ?i for is inf
                    | IsNan -- ?N for isnan
                    | IsSignaling -- ?sN for issignaling
                    | Minimum -- <C for minnum
                    | Maximum -- >C for maxnum
                    | MinNumMag -- <A for minnummag
                    | MaxNumMag -- >A for maxnummag
                    | SameQuantum -- =quant for samequantum NOT USED
                    | Quantize -- quant for quantize NOT USED
                    | NextUp -- Nu for next up NOT USED
                    | NextDown -- Nd for nextdown NOT USED
                    | Equivalent -- eq for equivalent. NOT USED
    deriving (Show, Eq)


-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
parseTypeWidth :: Parsec.Parsec String () FloatType
parseTypeWidth = do
    Parsec.choice [ Parsec.try $ Parsec.string "b32" >> return B32
                  , Parsec.try $ Parsec.string "b64" >> return B64
                  , Parsec.try $ Parsec.string "b128" >> return B128
                  , Parsec.try $ Parsec.string "d32" >> return D32
                  , Parsec.try $ Parsec.string "d64" >> return D64
                  , Parsec.try $ Parsec.string "d128" >> return D128
                  ]

-- TODO: probably should add `Parsec.try $ ` to all multicharacter comparisons
parseOperation :: Parsec.Parsec String () FloatOperation
parseOperation = do
    Parsec.choice [ Parsec.string "+" >> return Add
                  , Parsec.string "-" >> return Subtract
                  , Parsec.try $ Parsec.string "*+" >> return MultiplyAdd
                  , Parsec.string "/" >> return Divide
                  , Parsec.string "*" >> return Multiply
                  , Parsec.string "V" >> return SquareRoot
                  , Parsec.string "%" >> return Remainder
                  , Parsec.string "rfi" >> return RoundFloatToInt
                  , Parsec.try $ Parsec.string "cff" >> return ConvertFloatToFloat
                  , Parsec.try $ Parsec.string "cfi" >> return ConvertFloatToInt
                  , Parsec.try $ Parsec.string "cif" >> return ConvertIntToFloat
                  , Parsec.try $ Parsec.string "cfd" >> return ConvertToDecimalStr
                  , Parsec.try $ Parsec.string "cdf" >> return ConvertFromDecimalStr
                  , Parsec.string "qC" >> return QuietComparison
                  , Parsec.string "sC" >> return SignallingComparsion
                  , Parsec.string "cp" >> return Copy
                  , Parsec.string "~" >> return Negate
                  , Parsec.string "A" >> return Abs
                  , Parsec.string "@" >> return CopySign
                  , Parsec.string "S" >> return ScalB
                  , Parsec.string "L" >> return LogB
                  , Parsec.string "Na" >> return NextAfter
                  , Parsec.try $ Parsec.string "?-" >> return IsSigned
                  , Parsec.try $ Parsec.string "?n" >> return IsNormal
                  , Parsec.try $ Parsec.string "?f" >> return IsFinite
                  , Parsec.try $ Parsec.string "?0" >> return IsZero
                  , Parsec.try $ Parsec.string "?i" >> return IsInf
                  , Parsec.try $ Parsec.string "?N" >> return IsNan
                  , Parsec.try $ Parsec.string "?sN" >> return IsSignaling
                  , Parsec.try $ Parsec.string "?s" >> return IsSubNormal
                  , Parsec.string "?" >> return Class
                  , Parsec.try $ Parsec.string "<C" >> return Minimum
                  , Parsec.try $ Parsec.string ">C" >> return Maximum
                  , Parsec.string "<A" >> return MinNumMag
                  , Parsec.string ">A" >> return MaxNumMag
                  , Parsec.string "=quant" >> return SameQuantum
                  , Parsec.string "quant" >> return Quantize
                  , Parsec.string "Nu" >> return NextUp
                  , Parsec.string "Nd" >> return NextDown
                  , Parsec.string "eq" >> return Equivalent
                  ]

parseRoundingMode :: Parsec.Parsec String () RoundingMode
parseRoundingMode =  do
    Parsec.choice [ Parsec.string ">"  >> return RoundMax
                  , Parsec.string "<"  >> return RoundMin
                  , Parsec.string "0"  >> return RoundMinMag
                  , Parsec.try $ Parsec.string "=0" >> return RoundNearEven
                  , Parsec.string "=^" >> return RoundNearMaxMag
                  ]


parseExceptionFlags :: Parsec.Parsec String () ExceptionFlags
parseExceptionFlags = do
    Parsec.choice [ Parsec.char 'x'
                    >> return (ExceptionFlags True False False False False)
                  , Parsec.char 'u'
                    >> return (ExceptionFlags False True False False False)
                  , Parsec.char 'o'
                    >> return (ExceptionFlags False False True False False)
                  , Parsec.char 'z'
                    >> return (ExceptionFlags False False False True False)
                  , Parsec.char 'i'
                    >> return (ExceptionFlags False False False False True)
                  ]

parseException :: [ExceptionFlags] -> ExceptionFlags
parseException flags = ExceptionFlags {
        inexact = inexactFlag,
        underflow = underflowFlag,
        overflow = overflowFlag,
        infinite = infiniteFlag,
        invalid = invalidFlag
    }
    where
        inexactFlag = foldl (\x y -> x || (inexact y)) False flags
        underflowFlag = foldl (\x y -> x || (underflow y)) False flags
        overflowFlag = foldl (\x y -> x || (overflow y)) False flags
        infiniteFlag = foldl (\x y -> x || (infinite y)) False flags
        invalidFlag = foldl (\x y -> x || (invalid y)) False flags

{-- Structure encompassing all necessary operation for a single
    fpGen test operation
--}
data FpGenOperation = FpGenOperation
    { _types :: [FloatType]
    , _operation :: FloatOperation
    , _roundingMode :: RoundingMode
    , _trappedExceptions :: ExceptionFlags
    , _inputsFloat :: [String] -- TODO
    , _outputFloat :: String -- TODO
    , _outputExceptions :: ExceptionFlags
    }
    deriving (Show)

-- TODO: make this work for 64 bits too
infOrNan32 :: Word32
infOrNan32 = (shiftL 0xFF 23)

qNan32 :: Word32
qNan32 = defaultNan32

defaultNan32 :: Word32
defaultNan32 = infOrNan32 .|. (shiftL 0x1 22)

sNan32 :: Word32
sNan32 = infOrNan32 .|. 0x1

zero32 :: Word32
zero32 = zeroBits

minus32 :: Word32
minus32 = shiftL 0x1 31

inf32 :: Word32
inf32 = zeroBits .|. infOrNan32

plusInf32 :: Word32
plusInf32 = inf32

minusInf32 :: Word32
minusInf32 = minus32 .|. inf32

plusZero32 :: Word32
plusZero32 = zero32

minusZero32 :: Word32
minusZero32 = minus32 .|. zero32

parseSubnormal32 :: Parsec.Parsec String () Word32
parseSubnormal32 = do
    sign <- Parsec.choice [ Parsec.char '+' >> return zero32
                          , Parsec.char '-' >> return minus32
                        ]
    _ <- Parsec.string "0."
    significandRaw <- Parsec.manyTill (Parsec.digit <|> Parsec.oneOf "ABCDEF") (Parsec.string "P-126")
    let significandF = "0x" ++ significandRaw
    return $ sign .|. (read significandF)

parseSign :: Parsec.Parsec String () Int
parseSign = do
    Parsec.choice [ Parsec.char '-' >> return (-1)
                  , return 1
                ]

parseExponent :: Parsec.Parsec String () Word32
parseExponent = do
    sign <- parseSign
    trueExponent <- Parsec.many1 Parsec.digit
    let biasedExponent = sign * (read trueExponent :: Int) + 127
    return ( shiftL ((fromIntegral biasedExponent) .&. 0xFF) 23)

parseNormal32 :: Parsec.Parsec String () Word32
parseNormal32 = do
    sign <- Parsec.choice [ Parsec.char '+' >> return zero32
                          , Parsec.char '-' >> return minus32
                        ]
    _ <- Parsec.string "1."
    significandRaw <- Parsec.manyTill (Parsec.digit <|> Parsec.oneOf "ABCDEF") (Parsec.char 'P')
    biasedExponent <- parseExponent

    let significandF = "0x" ++ significandRaw
    return $ sign .|. biasedExponent .|. (read significandF)

convetFloat32ToBinary :: Parsec.Parsec String () Word32
convetFloat32ToBinary = do
    Parsec.choice [ Parsec.char 'Q' >> return qNan32
                  , Parsec.char 'S' >> return sNan32
                  , Parsec.char '#' >> return defaultNan32 -- TODO: this means `no result`
                  , Parsec.try $ Parsec.string "+Inf" >> return plusInf32
                  , Parsec.try $ Parsec.string "-Inf" >> return minusInf32
                  , Parsec.try $ Parsec.string "+Zero" >> return plusZero32
                  , Parsec.try $ Parsec.string "-Zero" >> return minusZero32
                  , Parsec.try $ Parsec.string "0x0" >> return 0x0
                  , Parsec.try $ Parsec.string "0x1" >> return 0x1
                  , Parsec.try $ parseNormal32
                  , Parsec.try $ parseSubnormal32
                ]


-- TODO: handle 128 bit types?
parseFloat :: Parsec.Parsec String () String
parseFloat = do
        Parsec.spaces <?> "parseFloat: cannot parse spaces before the number"
        number <- Parsec.many1 (Parsec.noneOf [' ']) <?> "parseFloat : cannot parse a number"
        Parsec.spaces <?> "parseFloat: cannot parse spaces after the number"
        return number

parseTypes :: Parsec.Parsec String () FpGenOperation
parseTypes = do
    -- get types (b/d 32/b64/b128)
    types <- Parsec.many1 parseTypeWidth <?> "Cannot parse type"
    operation <- parseOperation <?> "Cannot parse operation type"
    Parsec.spaces <?> "Cannot parse spaces"
    -- get rounding mode
    roundingMode <- parseRoundingMode <?> "Cannot parse rouding mode"
    Parsec.spaces  <?> "Error parsing after Rounding mode"
    -- get exceptions
    exceptions <- Parsec.many parseExceptionFlags
    let trappedException = parseException exceptions
    -- get inputs
    inputsFloat <- Parsec.manyTill parseFloat (Parsec.try $ Parsec.string "->")
                    <?> "cannot parse inputs"
    outputFloat <- parseFloat <?> "problem parsing output"
    -- optional output exception
    outputExceptions <- Parsec.many parseExceptionFlags <?> "output exceptions"
    let outputException = parseException outputExceptions
    return (FpGenOperation types operation roundingMode
            trappedException inputsFloat outputFloat outputException)

testParsec :: String -> IO ()
testParsec str = do
    let result = Parsec.parse convetFloat32ToBinary "" str
    case result of
        Right val -> do
            let s = (showIntAtBase 2 intToDigit val "")
            putStrLn $ (replicate (32 - length s) '0') ++ s
        Left err -> do
            putStrLn (show err)

testSuite :: String
testSuite = "./test/fpgen/test_suite/"

main :: IO ()
main = do
    files <- listDirectory testSuite
    forM_ files $ \ file -> do
        putStrLn $ "Processing " ++ file
        let path = testSuite ++ file
        contents <- readFile path
        let inputs = zip (drop 4 $ lines contents) [5..]
        forM_ inputs $ \(line, lineNumber) -> do
            let result = Parsec.parse parseTypes "" line
            case result of
                Right op -> do
                    let res = executeOperation op
                    case res of
                        Right _s -> do
                            {--
                            if length s > 1
                            then do
                                putStrLn $ "Warning: " ++ s
                            else return ()
                            --}
                            return ()
                        Left err -> do
                            if isInfixOf "Results are not matching" err
                            then do
                                putStrLn $ ">>> " ++ file ++ ": Line " ++ (show (lineNumber :: Integer))
                                putStrLn line
                                putStrLn $ "Error: " ++ err
                                putStrLn "\n"
                            else return ()
                            {--
                            putStrLn $ "Line " ++ (show (lineNumber :: Integer))
                            putStrLn line
                            putStrLn $ "Error: " ++ err
                            --}
                            {--
                            if isInfixOf "Unsupported operation" err
                            then exitFailure
                            else return ()
                            --}
                            {--
                            putStrLn "Continue? [Y/n]"
                            input <- getLine
                            if input == "Y"
                            then return ()
                            else exitFailure
                            --}
                            {--
                            if isInfixOf "Warning" err
                            then return ()
                            else exitFailure
                            --}
                            --return ()

                Left err -> do
                    putStrLn $ "Line " ++ (show (lineNumber :: Integer))
                    putStrLn line
                    putStrLn (show err)
    putStrLn "All good, exiting!"

checkOpArguments :: FpGenOperation -> Bool
checkOpArguments fp =
                case _operation fp of
                    -- 1 argument
                    op | op `elem` [Abs, Copy, Negate, SquareRoot,
                                    IsSigned, IsNormal,IsFinite,
                                    IsZero, IsSubNormal, IsInf, IsNan, IsSignaling,
                                    ConvertFloatToFloat]
                     -> if (length types ==1) && (length inputs == 1)
                        then True
                        else False
                    -- 2 arguments
                    op | op `elem` [Add, Subtract, Multiply, Divide,
                                    Maximum, Minimum, MaxNumMag, MinNumMag]  -> if (length types ==1) && (length inputs == 2)
                           then True
                           else False
                    -- 3 arguments
                    MultiplyAdd -> if (length types ==1) && (length inputs == 3)
                        then True
                        else False
                    _ -> False
    where
        types = _types fp
        inputs = _inputsFloat fp

convertOpArguments32 :: [String] -> Either Parsec.ParseError [Word32]
convertOpArguments32 inputs = mapM (Parsec.parse convetFloat32ToBinary "") inputs

getOpArguments :: FpGenOperation -> [String]
getOpArguments fp = _inputsFloat fp ++ [_outputFloat fp]

executeOperation :: FpGenOperation -> Either String String
executeOperation fp = do
    --evalTest testB32  ("Warning: Unsupported type, currently all operations have to be B32\n" ++ show fp)
    _ <- evalTest testB32  ("Warning: Unsupported type, currently all operations have to be b32")
    _ <- evalTest (checkOpArguments fp) ("Argument check failed, maybe an incorrect number of arguments?\n" ++ show fp)

    -- TODO: fix for different arg types
    let convResult = convertOpArguments32 (getOpArguments fp)
    case convResult of
        Left err -> do
            Left (show err)
        Right args -> do
            -- evaluate operations
            case _operation fp of
                Add -> do
                    -- f32Add :: RoundingMode -> Word32 -> Word32 -> F32Result
                    -- TODO: figure out what to do with trapped exceptions ?
                    let softFloatResult = f32Add (_roundingMode fp) (args !! 0) (args !! 1)
                    let fpgenResult = Result (args !! 2) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                Subtract -> do
                    -- f32Sub :: RoundingMode -> Word32 -> Word32 -> F32Result
                    let softFloatResult = f32Sub (_roundingMode fp) (args !! 0) (args !! 1)
                    let fpgenResult = Result (args !! 2) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                Multiply -> do
                    -- f32Mul :: RoundingMode -> Word32 -> Word32 -> F32Result
                    let softFloatResult = f32Mul (_roundingMode fp) (args !! 0) (args !! 1)
                    let fpgenResult = Result (args !! 2) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                Divide -> do
                    -- f32Div :: RoundingMode -> Word32 -> Word32 -> F32Result
                    let softFloatResult = f32Div (_roundingMode fp) (args !! 0) (args !! 1)
                    let fpgenResult = Result (args !! 2) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                SquareRoot -> do
                    -- f32Sqrt :: RoundingMode -> Word32 -> F32Result
                    let softFloatResult = f32Sqrt (_roundingMode fp) (args !! 0)
                    let fpgenResult = Result (args !! 1) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                MultiplyAdd -> do
                    -- f32MulAdd :: RoundingMode -> Word32 -> Word32 -> Word32 -> F32Result
                    let softFloatResult = f32MulAdd (_roundingMode fp) (args !! 0) (args !! 1) (args !! 2)
                    let fpgenResult = Result (args !! 3) (_outputExceptions fp)
                    evalResults fpgenResult softFloatResult
                IsSigned -> do
                    Right "`IsSigned` Not implemented, skipping"
                IsNormal -> do
                        Right "`IsNormal` Not implemented, skipping"
                IsFinite -> do
                    Right "`IsFinite` Not implemented, skipping"
                IsZero -> do
                    Right "`IsZero` Not implemented, skipping"
                IsSubNormal -> do
                    Right "`IsSubNormal` Not implemented, skipping"
                IsInf -> do
                    Right "`IsInf` Not implemented, skipping"
                IsNan -> do
                    Right "`IsNan` Not implemented, skipping"
                IsSignaling -> do
                    Right "`IsSignaling` Not implemented, skipping"
                Abs -> do
                    Right "`Abs` Not implemented, skipping"
                Copy -> do
                    Right "`Copy` Not implemented, skipping"
                Negate -> do
                    Right "`Negate` Not implemented, skipping"
                Maximum -> do
                    Right "`Maximum` Not implemented, skipping"
                Minimum -> do
                    Right "`Minimum` Not implemented, skipping"
                MaxNumMag -> do
                    Right "`MaxNumMag` Not implemented, skipping"
                MinNumMag -> do
                    Right "`MinNumMag` Not implemented, skipping"
                ConvertFloatToFloat -> do
                    Right "`ConvertFloatToFloat` Not implemented, skipping"
                _ -> do
                    Left "Unsupported operation"
    where
        evalResults fpgenResult softFloatResult = evalTest (fpgenResult == softFloatResult)
                        ("Results are not matching.\n\n" ++
                        "Fpgen:     " ++ show fpgenResult ++ "\n" ++
                        "Softfloat: " ++ show softFloatResult
                        ++ "\n\n" ++ show fp)

        evalTest :: Bool -> String -> Either String String
        evalTest b s | b == True = Right ""
                     | otherwise = Left s

        testB32 :: Bool
        testB32 = and (map (== B32) (_types fp))
