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

data AnyFloatOp = F16 FloatOp16 | F32 FloatOp32 | F64 FloatOp64
    deriving (Show)

data AnyResult = RF16 F16Result | RF32 F32Result | RF64 F64Result | RBool BoolResult
    deriving (Show, Eq)

executeOp :: AnyFloatOp -> AnyResult
executeOp (F16 op) = executeOp16 op
executeOp (F32 op) = executeOp32 op
executeOp (F64 op) = executeOp64 op

executeOp16 :: FloatOp16 -> AnyResult
executeOp16 (Arith op)   = RF16 $ interp op
executeOp16 (Compare op) = RBool $ interpCompare op
--executeOp16 (Convert op) = RF16 $ interpConvert op
executeOp16 op = error $ "Unimplemented: " ++ show op

executeOp32 :: FloatOp32 -> AnyResult
executeOp32 (Arith op)   = RF32 $ interp op
executeOp32 (Compare op) = RBool $ interpCompare op
--executeOp32 (Convert op) = RF32 $ interpConvert op
executeOp32 op = error $ "Unimplemented: " ++ show op

executeOp64 :: FloatOp64 -> AnyResult
executeOp64 (Arith op)   = RF64 $ interp op
executeOp64 (Compare op) = RBool $ interpCompare op
executeOp64 op = error $ "Unimplemented: " ++ show op

readResult :: [String] -> String -> AnyResult
readResult vals arg | isCompare == True = RBool (Result opBool flags)
                    | opWidth == "f16" = RF16 (Result op16 flags)
                    | opWidth == "f32" = RF32 (Result op32 flags)
                    | opWidth == "f64" = RF64 (Result op64 flags)
                    | otherwise = error $ "Unimplemented: " ++ opWidth
    where
        opWidth = takeWhile (\x -> x /= '_') arg
        isCompare = ("_le" `isInfixOf` arg) || ("_lt" `isInfixOf` arg) || ("_eq" `isInfixOf` arg)
        flags = readExceptionFlags (fst $ head $ readHex (last vals) :: Word8)
        op16 = fst $ head $ readHex (head vals) :: Word16
        op32 = fst $ head $ readHex (head vals) :: Word32
        op64 = fst $ head $ readHex (head vals) :: Word64
        opBool | (read $ head vals :: Word8) == 1 = True
               | otherwise = False

readRoundingMode :: String -> RoundingMode
readRoundingMode s | mode == "odd" = RoundOdd
                   | mode == "min" = RoundMin
                   | mode == "max" = RoundMax
                   | mode == "minMag" = RoundMinMag
                   | mode == "near_even" = RoundNearEven
                   | mode == "near_maxMag" = RoundNearMaxMag
                   | otherwise = error $ "Unsupported mode: " ++ mode
    where
        mode = drop 2 s

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
parseOp operands args | opWidth == "f16" = F16 (parseOpInner ops16 args)
                      | opWidth == "f32" = F32 (parseOpInner ops32 args)
                      | opWidth == "f64" = F64 (parseOpInner ops64 args)
                      | otherwise = error $ "Unimplemented: " ++ opWidth
    where
        opWidth = takeWhile (\x -> x /= '_') (head args)
        ops16 = map (\x -> fst $ head $ readHex x :: Word16) operands
        ops32 = map (\x -> fst $ head $ readHex x :: Word32) operands
        ops64 = map (\x -> fst $ head $ readHex x :: Word64) operands

-- args are e.g. "["f16_div","-rodd"]" (optype, rounding mode)
-- operands are parsed Words {16,32,64}
parseOpInner :: [a] -> [String] -> FloatOp a
parseOpInner ops args | opType == "add" = Arith $ Add r (ops !! 0) (ops !! 1)
                  | opType == "mul" = Arith $ Mul r (ops !! 0) (ops !! 1)
                  | opType == "div" = Arith $ Div r (ops !! 0) (ops !! 1)
                  | opType == "sub" = Arith $ Sub r (ops !! 0) (ops !! 1)
                  | opType == "rem" = Arith $ Rem r (ops !! 0) (ops !! 1)
                  | opType == "sqrt" = Arith $ Sqrt r (ops !! 0)
                  | opType == "eq" = Compare $ Eq (ops !! 0) (ops !! 1)
                  | opType == "le" = Compare $ Le (ops !! 0) (ops !! 1)
                  | opType == "lt" = Compare $ Lt (ops !! 0) (ops !! 1)
                  | opType == "eq_signaling" = Compare $ EqSignaling (ops !! 0) (ops !! 1)
                  | opType == "le_quiet" = Compare $ LeQuiet (ops !! 0) (ops !! 1)
                  | opType == "lt_quiet" = Compare $ LtQuiet (ops !! 0) (ops !! 1)
--                  | opType == "to_ui32" = Convert $ ToUi32 (ops !! 0)
--                  | opType == "to_ui64" = Convert $ ToUi64 (ops !! 0)
--                  | opType == "to_i32" = Convert $ ToI32 (ops !! 0)
--                  | opType == "to_i64" = Convert $ ToI64 (ops !! 0)
                  | otherwise = error $ "Unimplemented: " ++ opType
    where
        opType = tail $ dropWhile (\x -> x /= '_') (head args)
        r = readRoundingMode (last args)

data FloatOp a = Arith (FloatArithmeticOp a)
               | Compare (FloatCompareOp a)
               | Convert (FloatConvertOp a)
    deriving (Show)

-- a `elem` [f16,f32,f64]
data FloatArithmeticOp a = MulAdd RoundingMode a a a -- *+
                         | Add RoundingMode a a -- +
                         | Mul RoundingMode a a -- *
                         | Div RoundingMode a a -- /
                         | Sub RoundingMode a a -- -
                         | Rem RoundingMode a a -- %
                         | Sqrt RoundingMode a -- V
    deriving (Show, Eq)

-- a `elem` [f16,f32,f64]
data FloatCompareOp a = Eq a a -- ==
                      | Le a a -- <=
                      | Lt a a -- <
                      | EqSignaling a a
                      | LeQuiet a a
                      | LtQuiet a a
    deriving (Show, Eq)

-- a `elem` [f16,f32,f64]
data FloatConvertOp a = Ui32ToF a
                      -- | ToUi32 a
                      -- | ToUi64 a
                      -- | ToI32 a
                      -- | ToI64 a
                      | I32toF a
                      | Ui64toF a
                      | I64ToF a
                      | F16ToF a
                      | F32toF a
                      | F64toF a
                      -- | ToF16 a
                      -- | ToF32 a
                      -- | ToF64 a
                      | RoundToInt a
    deriving (Show, Eq)

-- TODO: testfloat_gen doesn't generate test cases for 
-- f32IsSignalingNaN :: Word32 -> BoolResult
class FloatArithmeticOpImpl a where
    -- f32MulAdd :: RoundingMode -> Word32 -> Word32 -> Word32 -> F32Result
    flMulAdd :: RoundingMode -> a -> a -> a -> Result a
    -- f32Add :: RoundingMode -> Word32 -> Word32 -> F32Result
    flAdd :: RoundingMode -> a -> a -> Result a
    --f32Mul :: RoundingMode -> Word32 -> Word32 -> F32Result
    flMul :: RoundingMode -> a -> a -> Result a
    -- f32Div :: RoundingMode -> Word32 -> Word32 -> F32Result
    flDiv :: RoundingMode -> a -> a -> Result a
    -- f32Sub :: RoundingMode -> Word32 -> Word32 -> F32Result
    flSub :: RoundingMode -> a -> a -> Result a
    -- f32Rem :: RoundingMode -> Word32 -> Word32 -> F32Result
    flRem :: RoundingMode -> a -> a -> Result a
    -- f32Sqrt :: RoundingMode -> Word32 -> F32Result
    flSqrt :: RoundingMode -> a -> Result a

class FloatCompareOpImpl a where
    -- f32Eq :: Word32 -> Word32 -> BoolResult
    flEq :: a -> a -> BoolResult
    -- f32Le :: Word32 -> Word32 -> BoolResult
    flLe :: a -> a -> BoolResult
    -- f32Lt :: Word32 -> Word32 -> BoolResult
    flLt :: a -> a -> BoolResult
    -- f32EqSignaling :: Word32 -> Word32 -> BoolResult
    flEqSignaling :: a -> a -> BoolResult
    -- f32LeQuiet :: Word32 -> Word32 -> BoolResult
    flLeQuiet :: a -> a -> BoolResult
    -- f32LtQuiet :: Word32 -> Word32 -> BoolResult
    flLtQuiet :: a -> a -> BoolResult

class FloatConvertOpImpl a where
    --flToUi32 :: RoundingMode -> a -> Ui32Result
    --flToUi64 :: RoundingMode -> a -> Ui64Result
    --flToI32  :: RoundingMode -> a -> I32Result
    --flToI64  :: RoundingMode -> a -> I64Result
    flUi32ToF :: RoundingMode -> Word32 -> Result a
    flI32ToF :: RoundingMode -> Int32 -> Result a
    flUi64ToF :: RoundingMode -> Word64 -> Result a
    flI64ToF :: RoundingMode ->  Int64 -> Result a
    flF16ToF :: RoundingMode -> Word16 -> Result a
    flF32ToF :: RoundingMode -> Word32 -> Result a
    flF64ToF :: RoundingMode -> Word64 -> Result a
    --flToF16 :: RoundingMode -> a -> F16Result
    --flToF32 :: RoundingMode -> a -> F32Result
    --flToF64 :: RoundingMode -> a -> F64Result
    flRoundToInt :: RoundingMode -> a -> Result a

interp :: (FloatArithmeticOpImpl a) => FloatArithmeticOp a -> Result a
interp (MulAdd r a b c) = flMulAdd r a b c
interp (Add r a b) = flAdd r a b
interp (Mul r a b) = flMul r a b
interp (Div r a b) = flDiv r a b
interp (Sub r a b) = flSub r a b
interp (Rem r a b) = flRem r a b
interp (Sqrt r a) = flSqrt r a

interpCompare :: (FloatCompareOpImpl a) => FloatCompareOp a -> BoolResult
interpCompare (Eq a b) = flEq a b
interpCompare (Le a b) = flLe a b
interpCompare (Lt a b) = flLt a b
interpCompare (EqSignaling a b) = flEqSignaling a b
interpCompare (LeQuiet a b) = flLeQuiet a b
interpCompare (LtQuiet a b) = flLtQuiet a b

{--
interpConvert :: (FloatConvertOpImpl a) => FloatConvertOp a -> Result a
Ui32ToF a
I32toF a
Ui64toF a
I64ToF a
F16ToF a
F32toF a
F64toF a
RoundToInt a
--}


instance FloatArithmeticOpImpl Word16 where
    flMulAdd r a b c = f16MulAdd r a b c
    flAdd r a b = f16Add r a b
    flMul r a b = f16Mul r a b
    flDiv r a b = f16Div r a b
    flSub r a b = f16Sub r a b
    flRem r a b = f16Rem r a b
    flSqrt r a = f16Sqrt r a

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

instance FloatCompareOpImpl Word64 where
    flEq a b = f64Eq a b
    flLe a b = f64Le a b
    flLt a b = f64Lt a b
    flEqSignaling a b = f64EqSignaling a b
    flLeQuiet a b = f64LeQuiet a b
    flLtQuiet a b = f64LtQuiet a b
