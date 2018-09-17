module Ops where

import SoftFloat
import Data.Word
import Data.Int
import Data.Bits
import Numeric (readHex)
import Data.List (isInfixOf)

type FloatOp16 = FloatOp Word16
type FloatOp32 = FloatOp Word32
type FloatOp64 = FloatOp Word64

data AnyFloatOp = F16 FloatOp16
                | F32 FloatOp32
                | F64 FloatOp64
                | FConvert FloatConvertOp
    deriving (Show)

data AnyResult = RF16 F16Result
               | RF32 F32Result
               | RF64 F64Result
               | RBool BoolResult
               | RI32 I32Result
               | RI64 I64Result
    deriving (Show, Eq)

data FloatOp a = Arith (FloatArithmeticOp a)
               | Compare (FloatCompareOp a)
    deriving (Show)

-- a `elem` [f16,f32,f64]
data FloatArithmeticOp a = MulAdd RoundingMode a a a -- *+
                         | Add RoundingMode a a -- +
                         | Mul RoundingMode a a -- *
                         | Div RoundingMode a a -- /
                         | Sub RoundingMode a a -- -
                         | Rem RoundingMode a a -- %
                         | Sqrt RoundingMode a -- V
                         | RoundToInt RoundingMode a
    deriving (Show, Eq)

-- a `elem` [f16,f32,f64]
data FloatCompareOp a = Eq a a -- ==
                      | Le a a -- <=
                      | Lt a a -- <
                      | EqSignaling a a
                      | LeQuiet a a
                      | LtQuiet a a
    deriving (Show, Eq)

-- Convert operations
data FloatConvertOp = Ui32ToF16 RoundingMode Word32
                    | I32ToF16 RoundingMode Int32
                    | Ui64ToF16 RoundingMode Word64
                    | I64ToF16 RoundingMode Int64
                    | F32ToF16 RoundingMode Word32
                    | F64ToF16 RoundingMode Word64
                    | Ui32ToF32 RoundingMode Word32
                    | I32ToF32 RoundingMode Int32
                    | Ui64ToF32 RoundingMode Word64
                    | I64ToF32 RoundingMode Int64
                    | F16ToF32 RoundingMode Word16
                    | F64ToF32 RoundingMode Word64
                    | Ui32ToF64 RoundingMode Word32
                    | I32ToF64 RoundingMode Int32
                    | Ui64ToF64 RoundingMode Word64
                    | I64ToF64 RoundingMode Int64
                    | F16ToF64 RoundingMode Word16
                    | F32ToF64 RoundingMode Word32
                    | F16ToUi32 RoundingMode Word16
                    | F32ToUi32 RoundingMode Word32
                    | F64ToUi32 RoundingMode Word64
                    | F16ToUi64 RoundingMode Word16
                    | F32ToUi64 RoundingMode Word32
                    | F64ToUi64 RoundingMode Word64
                    | F16ToI32 RoundingMode Word16
                    | F32ToI32 RoundingMode Word32
                    | F64ToI32 RoundingMode Word64
                    | F16ToI64 RoundingMode Word16
                    | F32ToI64 RoundingMode Word32
                    | F64ToI64 RoundingMode Word64
    deriving (Show, Eq)    

executeOp :: AnyFloatOp -> AnyResult
executeOp (F16 op) = executeOp16 op
executeOp (F32 op) = executeOp32 op
executeOp (F64 op) = executeOp64 op
executeOp (FConvert op) = executeConvertOp op

executeOp16 :: FloatOp16 -> AnyResult
executeOp16 (Arith op)   = RF16 $ interp op
executeOp16 (Compare op) = RBool $ interpCompare op

executeOp32 :: FloatOp32 -> AnyResult
executeOp32 (Arith op)   = RF32 $ interp op
executeOp32 (Compare op) = RBool $ interpCompare op

executeOp64 :: FloatOp64 -> AnyResult
executeOp64 (Arith op)   = RF64 $ interp op
executeOp64 (Compare op) = RBool $ interpCompare op

executeConvertOp :: FloatConvertOp -> AnyResult
executeConvertOp (Ui32ToF16 r w) = RF16 (ui32ToF16 r w)
executeConvertOp (I32ToF16 r w) = RF16 (i32ToF16 r w)
executeConvertOp (Ui64ToF16 r w) = RF16 (ui64ToF16 r w)
executeConvertOp (I64ToF16 r w) = RF16 (i64ToF16 r w)
executeConvertOp (F32ToF16 r w) = RF16 (f32ToF16 r w)
executeConvertOp (F64ToF16 r w) = RF16 (f64ToF16 r w)
executeConvertOp (Ui32ToF32 r w) = RF32 (ui32ToF32 r w)
executeConvertOp (I32ToF32 r w) = RF32 (i32ToF32 r w)
executeConvertOp (Ui64ToF32 r w) = RF32 (ui64ToF32 r w)
executeConvertOp (I64ToF32 r w) = RF32 (i64ToF32 r w)
executeConvertOp (F16ToF32 r w) = RF32 (f16ToF32 r w)
executeConvertOp (F64ToF32 r w) = RF32 (f64ToF32 r w)
executeConvertOp (Ui32ToF64 r w) = RF64 (ui32ToF64 r w)
executeConvertOp (I32ToF64 r w) = RF64 (i32ToF64 r w)
executeConvertOp (Ui64ToF64 r w) = RF64 (ui64ToF64 r w)
executeConvertOp (I64ToF64 r w) = RF64 (i64ToF64 r w)
executeConvertOp (F16ToF64 r w) = RF64 (f16ToF64 r w)
executeConvertOp (F32ToF64 r w) = RF64 (f32ToF64 r w)
executeConvertOp (F16ToUi32 r w) = RF32 (f16ToUi32 r w)
executeConvertOp (F32ToUi32 r w) = RF32 (f32ToUi32 r w)
executeConvertOp (F64ToUi32 r w) = RF32 (f64ToUi32 r w)
executeConvertOp (F16ToUi64 r w) = RF64 (f16ToUi64 r w)
executeConvertOp (F32ToUi64 r w) = RF64 (f32ToUi64 r w)
executeConvertOp (F64ToUi64 r w) = RF64 (f64ToUi64 r w)
executeConvertOp (F16ToI32 r w) = RI32 (f16ToI32 r w)
executeConvertOp (F32ToI32 r w) = RI32 (f32ToI32 r w)
executeConvertOp (F64ToI32 r w) = RI32 (f64ToI32 r w)
executeConvertOp (F16ToI64 r w) = RI64 (f16ToI64 r w)
executeConvertOp (F32ToI64 r w) = RI64 (f32ToI64 r w)
executeConvertOp (F64ToI64 r w) = RI64 (f64ToI64 r w)

ops16 :: [String] -> [Word16]
ops16 operands = map (\x -> fst $ head $ readHex x :: Word16) operands

ops32 :: [String] -> [Word32]
ops32 operands = map (\x -> fst $ head $ readHex x :: Word32) operands

ops64 :: [String] -> [Word64]
ops64 operands = map (\x -> fst $ head $ readHex x :: Word64) operands

opsI32 :: [String] -> [Int32]
opsI32 operands = map (\x -> fst $ head $ readHex x :: Int32) operands

opsI64 :: [String] -> [Int64]
opsI64 operands = map (\x -> fst $ head $ readHex x :: Int64) operands

opsBool :: [String] -> [Bool]
opsBool operands = map (\x -> (fst $ head $ readHex x :: Word8) == 1) operands

isConvert :: String -> Bool
isConvert arg = "_to_" `isInfixOf` arg

isCompare :: String -> Bool
isCompare arg = ("_le" `isInfixOf` arg) || ("_lt" `isInfixOf` arg) || ("_eq" `isInfixOf` arg)

opWidth :: String -> String
opWidth arg = if isConvert arg-- one of the `x_to_y` cases
              then reverse $ takeWhile (\x -> x /= '_') $ reverse arg -- the op type is in the suffix
              else takeWhile (\x -> x /= '_') arg -- the op type is in the prefix

-- operands are string operands, such as "BECF"
-- arg is  the type argument, e.g. "f32_add" or "ui32_to_f32"
readResult :: [String] -> String -> AnyResult
readResult operands arg | isCompare arg = RBool (Result (head $ opsBool operands) flags)
                    | opWidth arg == "ui64" =  RF64 (Result (head $ ops64 operands) flags)
                    | opWidth arg == "ui32" =  RF32 (Result (head $ ops32 operands) flags)
                    | opWidth arg == "i32" =  RI32 (Result (head $ opsI32 operands) flags)
                    | opWidth arg == "i64" =  RI64 (Result (head $ opsI64 operands) flags)
                    | opWidth arg == "f16" = RF16 (Result (head $ ops16 operands) flags)
                    | opWidth arg == "f32" = RF32 (Result (head $ ops32 operands) flags)
                    | opWidth arg == "f64" = RF64 (Result (head $ ops64 operands) flags)
                    | otherwise = error $ "Unimplemented: " ++ (opWidth arg)
    where
        flags = readExceptionFlags (fst $ head $ readHex (last operands) :: Word8)
        
-- read roudning mode from the argument, e.g "-rodd" -> RoundOdd
readRoundingMode :: String -> RoundingMode
readRoundingMode arg | mode == "odd" = RoundOdd
                   | mode == "min" = RoundMin
                   | mode == "max" = RoundMax
                   | mode == "minMag" = RoundMinMag
                   | mode == "near_even" = RoundNearEven
                   | mode == "near_maxMag" = RoundNearMaxMag
                   | otherwise = error $ "Unsupported mode: " ++ mode
    where
        mode = drop 2 arg

-- read exception flags from the hexadecimal number. eg. "10" -> isInvalid
readExceptionFlags :: Word8 -> ExceptionFlags
readExceptionFlags flags = ExceptionFlags isInexact isUnderflow isOverflow isInf isInvalid
    where
        isInexact = (flags .&. 0x1) /= 0
        isUnderflow = (flags .&. 0x2) /= 0
        isOverflow = (flags .&. 0x4) /= 0
        isInf = (flags .&. 0x8) /= 0
        isInvalid = (flags .&. 0x10) /= 0

-- args are e.g. "["f16_div","-rodd"]" (optype, rounding mode)
-- operands are for example: "["BFF0B277", "DFFDFE00", "1F72998F"]
parseOp :: [String] -> [String] -> AnyFloatOp
parseOp operands args | isConvert $ head args = FConvert $ parseConvertOp operands args
                      | (opWidth $ head args) == "f16" = F16 $ parseOpInner (ops16 operands) args
                      | (opWidth $ head args) == "f32" = F32 $ parseOpInner (ops32 operands) args
                      | (opWidth $ head args) == "f64" = F64 $ parseOpInner (ops64 operands) args
                      | otherwise = error $ "Unimplemented: " ++ (opWidth $ head args)

parseConvertOp :: [String] -> [String] -> FloatConvertOp
parseConvertOp operands args | opType == "ui32_to_f16" = Ui32ToF16 r (ops32 operands !! 0)
                             | opType == "i32_to_f16" = I32ToF16 r (opsI32 operands !! 0)
                             | opType == "ui64_to_f16" = Ui64ToF16 r (ops64 operands !! 0)
                             | opType == "i64_to_f16" = I64ToF16 r (opsI64 operands !! 0)
                             | opType == "f32_to_f16" = F32ToF16 r (ops32 operands !! 0)
                             | opType == "f64_to_f16" = F64ToF16 r (ops64 operands !! 0)
                             -- Returns f32
                             | opType == "ui32_to_f32" = Ui32ToF32 r (ops32 operands !! 0)
                             | opType == "i32_to_f32" = I32ToF32 r (opsI32 operands !! 0)
                             | opType == "ui64_to_f32" = Ui64ToF32 r (ops64 operands !! 0)
                             | opType == "i64_to_f32" = I64ToF32 r (opsI64 operands !! 0)
                             | opType == "f16_to_f32" = F16ToF32 r (ops16 operands !! 0)
                             | opType == "f64_to_f32" = F64ToF32 r (ops64 operands !! 0)
                             -- Returns f64
                             | opType == "ui32_to_f64" = Ui32ToF64 r (ops32 operands !! 0)
                             | opType == "i32_to_f64" = I32ToF64 r (opsI32 operands !! 0)
                             | opType == "ui64_to_f64" = Ui64ToF64 r (ops64 operands !! 0)
                             | opType == "i64_to_f64" = I64ToF64 r (opsI64 operands !! 0)
                             | opType == "f16_to_f64" = F16ToF64 r (ops16 operands !! 0)
                             | opType == "f32_to_f64" = F32ToF64 r (ops32 operands !! 0)
                             -- Returns ui32
                             | opType == "f16_to_ui32" = F16ToUi32 r (ops16 operands !! 0)
                             | opType == "f32_to_ui32" = F32ToUi32 r (ops32 operands !! 0)
                             | opType == "f64_to_ui32" = F64ToUi32 r (ops64 operands !! 0)
                            -- Returns ui64
                             | opType == "f16_to_ui64" = F16ToUi64 r (ops16 operands !! 0)
                             | opType == "f32_to_ui64" = F32ToUi64 r (ops32 operands !! 0)
                             | opType == "f64_to_ui64" = F64ToUi64 r (ops64 operands !! 0)
                            -- Returns i32
                            | opType == "f16_to_i32" = F16ToI32 r (ops16 operands !! 0)
                            | opType == "f32_to_i32" = F32ToI32 r (ops32 operands !! 0)
                            | opType == "f64_to_i32" = F64ToI32 r (ops64 operands !! 0)
                           -- Returns i64
                            | opType == "f16_to_i64" = F16ToI64 r (ops16 operands !! 0)
                            | opType == "f32_to_i64" = F32ToI64 r (ops32 operands !! 0)
                            | opType == "f64_to_i64" = F64ToI64 r (ops64 operands !! 0)
                            | otherwise = error $ "Unimplemented: " ++ opType
    where
        r = readRoundingMode $ last args
        opType = head args


-- args are e.g. "["f16_div","-rodd"]" (optype, rounding mode)
-- operands are parsed Words {16,32,64}
parseOpInner :: [a] -> [String] -> FloatOp a
parseOpInner ops args | opType == "add" = Arith $ Add r (ops !! 0) (ops !! 1)
                      | opType == "mulAdd" = Arith $ MulAdd r (ops !! 0) (ops !! 1) (ops !! 2)
                      | opType == "mul" = Arith $ Mul r (ops !! 0) (ops !! 1)
                      | opType == "div" = Arith $ Div r (ops !! 0) (ops !! 1)
                      | opType == "sub" = Arith $ Sub r (ops !! 0) (ops !! 1)
                      | opType == "rem" = Arith $ Rem r (ops !! 0) (ops !! 1)
                      | opType == "sqrt" = Arith $ Sqrt r (ops !! 0)
                      | opType == "roundToInt" = Arith $ RoundToInt r (ops !! 0)
                      | opType == "eq" = Compare $ Eq (ops !! 0) (ops !! 1)
                      | opType == "le" = Compare $ Le (ops !! 0) (ops !! 1)
                      | opType == "lt" = Compare $ Lt (ops !! 0) (ops !! 1)
                      | opType == "eq_signaling" = Compare $ EqSignaling (ops !! 0) (ops !! 1)
                      | opType == "le_quiet" = Compare $ LeQuiet (ops !! 0) (ops !! 1)
                      | opType == "lt_quiet" = Compare $ LtQuiet (ops !! 0) (ops !! 1)
                      | otherwise = error $ "Unimplemented: " ++ opType
    where
        opType = tail $ dropWhile (\x -> x /= '_') (head args)
        r = readRoundingMode (last args)

-- TODO: testfloat_gen doesn't generate test cases for 
-- f32IsSignalingNaN :: Word32 -> BoolResult
class FloatArithmeticOpImpl a where
    flMulAdd :: RoundingMode -> a -> a -> a -> Result a
    flAdd :: RoundingMode -> a -> a -> Result a
    flMul :: RoundingMode -> a -> a -> Result a
    flDiv :: RoundingMode -> a -> a -> Result a
    flSub :: RoundingMode -> a -> a -> Result a
    flRem :: RoundingMode -> a -> a -> Result a
    flSqrt :: RoundingMode -> a -> Result a
    flRoundToInt :: RoundingMode -> a -> Result a

class FloatCompareOpImpl a where
    flEq :: a -> a -> BoolResult
    flLe :: a -> a -> BoolResult
    flLt :: a -> a -> BoolResult
    flEqSignaling :: a -> a -> BoolResult
    flLeQuiet :: a -> a -> BoolResult
    flLtQuiet :: a -> a -> BoolResult

interp :: (FloatArithmeticOpImpl a) => FloatArithmeticOp a -> Result a
interp (MulAdd r a b c) = flMulAdd r a b c
interp (Add r a b) = flAdd r a b
interp (Mul r a b) = flMul r a b
interp (Div r a b) = flDiv r a b
interp (Sub r a b) = flSub r a b
interp (Rem r a b) = flRem r a b
interp (Sqrt r a) = flSqrt r a
interp (RoundToInt r a) = flRoundToInt r a

interpCompare :: (FloatCompareOpImpl a) => FloatCompareOp a -> BoolResult
interpCompare (Eq a b) = flEq a b
interpCompare (Le a b) = flLe a b
interpCompare (Lt a b) = flLt a b
interpCompare (EqSignaling a b) = flEqSignaling a b
interpCompare (LeQuiet a b) = flLeQuiet a b
interpCompare (LtQuiet a b) = flLtQuiet a b

instance FloatArithmeticOpImpl Word16 where
    flMulAdd r a b c = f16MulAdd r a b c
    flAdd r a b = f16Add r a b
    flMul r a b = f16Mul r a b
    flDiv r a b = f16Div r a b
    flSub r a b = f16Sub r a b
    flRem r a b = f16Rem r a b
    flSqrt r a = f16Sqrt r a
    flRoundToInt r a = f16RoundToInt r a

instance FloatCompareOpImpl Word16 where
    flEq a b = f16Eq a b
    flLe a b = f16Le a b
    flLt a b = f16Lt a b
    flEqSignaling a b = f16EqSignaling a b
    flLeQuiet a b = f16LeQuiet a b
    flLtQuiet a b = f16LtQuiet a b


instance FloatArithmeticOpImpl Word32 where
    flMulAdd r a b c = f32MulAdd r a b c
    flAdd r a b = f32Add r a b
    flMul r a b = f32Mul r a b
    flDiv r a b = f32Div r a b
    flSub r a b = f32Sub r a b
    flRem r a b = f32Rem r a b
    flSqrt r a = f32Sqrt r a
    flRoundToInt r a = f32RoundToInt r a

instance FloatCompareOpImpl Word32 where
    flEq a b = f32Eq a b
    flLe a b = f32Le a b
    flLt a b = f32Lt a b
    flEqSignaling a b = f32EqSignaling a b
    flLeQuiet a b = f32LeQuiet a b
    flLtQuiet a b = f32LtQuiet a b


instance FloatArithmeticOpImpl Word64 where
    flMulAdd r a b c = f64MulAdd r a b c
    flAdd r a b = f64Add r a b
    flMul r a b = f64Mul r a b
    flDiv r a b = f64Div r a b
    flSub r a b = f64Sub r a b
    flRem r a b = f64Rem r a b
    flSqrt r a = f64Sqrt r a
    flRoundToInt r a = f64RoundToInt r a

instance FloatCompareOpImpl Word64 where
    flEq a b = f64Eq a b
    flLe a b = f64Le a b
    flLt a b = f64Lt a b
    flEqSignaling a b = f64EqSignaling a b
    flLeQuiet a b = f64LeQuiet a b
    flLtQuiet a b = f64LtQuiet a b
