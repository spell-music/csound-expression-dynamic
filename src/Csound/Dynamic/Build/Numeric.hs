{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TypeSynonymInstances, FlexibleInstances #-}
-- | Numeric instances
module Csound.Dynamic.Build.Numeric(
    ceilE, floorE, roundE, fracE        
) where

import Data.Monoid

import Csound.Dynamic.Types
import Csound.Dynamic.Build(toExp, prim, noRate)

---------------------------------------------
-- monoid

instance Monoid E where
    mempty  = 0
    mappend = (+)

--------------------------------------------
-- numeric instances

instance Num E where    
    (+) a b 
        | isZero a = b
        | isZero b = a
        | otherwise = biOpt (+) Add a b
        
    (*) a b 
        | isZero a || isZero b = fromDouble 0
        | otherwise = biOpt (*) Mul a b
        
    (-) a b  
        | isZero a = negate b
        | isZero b = a
        | otherwise = biOpt (-) Sub a b    
    
    negate = unOpt negate Neg
    
    fromInteger = fromDouble . fromInteger
    abs = unOpt abs Abs
    signum = undefined

instance Fractional E where
    (/) a b 
        | isZero a = fromDouble 0
        | isZero b = error "csound (/): division by zero" 
        | otherwise = biOpt (/) Div a b

    fromRational = fromDouble . fromRational    

instance Floating E where
    pi = fromDouble pi
    exp = unOpt exp ExpOp
    sqrt = unOpt sqrt Sqrt
    log = unOpt log Log
    logBase a n = case n of
        2 -> unOpt (flip logBase 2) Logbtwo a
        10 -> unOpt (flip logBase 10) Log10 a
        b -> log a / log b
    (**) = biOpt (**) Pow
    sin = unOpt sin Sin 
    tan = unOpt tan Tan
    cos = unOpt cos Cos
    asin = unOpt asin Sininv
    atan = unOpt atan Taninv
    acos = unOpt acos Cosinv
    sinh = unOpt sinh Sinh
    tanh = unOpt tanh Tanh
    cosh = unOpt cosh Cosh
    asinh a = log $ a + sqrt (a * a + 1)
    acosh a = log $ a + sqrt (a + 1) * sqrt (a - 1)
    atanh a = 0.5 * log ((1 + a) / (1 - a))

enumError :: String -> a
enumError name = error $ name ++ " -- is defined only for literals"
    
instance Enum E where
    succ = (+1)
    pred = \x -> x - 1
    toEnum = fromDouble . fromIntegral
    fromEnum = error "fromEnum is not defined for Csound values" 
    enumFrom a = a : enumFrom (a+1)    
    enumFromThen a b = a : enumFromThen (a + b) b
     
    enumFromTo a b = case (toNumOpt a, toNumOpt b) of
        (Left x, Left y) -> fmap fromDouble $ enumFromTo x y
        _ -> enumError "[a .. b]"
            
    enumFromThenTo a b c = case (toNumOpt a, toNumOpt b, toNumOpt c) of
        (Left x, Left y, Left z) -> fmap fromDouble $ enumFromThenTo x y z
        _ -> enumError "[a, b .. c]"
    
    
instance Real E where toRational = error "instance of the Real is not defined for Csound values. It's here only for other classes."
        
instance Integral E where
    quot a b = intE $ (intE a) / (intE b)
    rem a b = (a `quot` b) * b - a
    mod = mod'
    div a b = intE $ a - mod a b / b
    quotRem a b = (quot a b, rem a b)
    divMod a b = (div a b, mod a b)
    toInteger = error "toInteger is not defined for Csound values"

------------------------------------------------------------
-- Optimizations for constants
--
-- If an arithmetic expression contains constants we can execute
-- it and render as constant. We check wether all arguments 
-- are constants. If it's so we apply some numeric function and
-- propogate a constant value.

toNumOpt :: E -> Either Double E
toNumOpt x = case toExp x of
    ExpPrim (PrimDouble d) -> Left d
    _ -> Right x

fromNumOpt :: Either Double E -> E
fromNumOpt = either (prim . PrimDouble) id 

expNum :: NumExp E -> E
expNum = noRate . ExpNum . fmap toPrimOr

fromDouble :: Double -> E
fromDouble = fromNumOpt . Left

isZero :: E -> Bool
isZero a = either ( == 0) (const False) $ toNumOpt a

-- optimization for unary functions
unOpt :: (Double -> Double) -> NumOp -> E -> E
unOpt doubleOp op a = fromNumOpt $ either (Left . doubleOp) (Right . noOpt1) $ toNumOpt a
    where noOpt1 x = expNum $ PreInline op [x] 

-- optimization for binary functions
biOpt :: (Double -> Double -> Double) -> NumOp -> E -> E -> E
biOpt doubleOp op a b = fromNumOpt $ case (toNumOpt a, toNumOpt b) of
    (Left da, Left db) -> Left $ doubleOp da db
    _ -> Right $ noOpt2 a b
    where noOpt2 x y = expNum $ PreInline op [x, y]

doubleToInt :: (Double -> Int) -> NumOp -> E -> E
doubleToInt fun = unOpt (fromIntegral . fun) 

-- arithmetic

mod' :: E -> E -> E
mod' = biOpt (\a b -> fromIntegral $ mod (floor a :: Int) (floor b)) Mod
 
-- other functions

ceilE, floorE, fracE, intE, roundE :: E -> E

ceilE   = doubleToInt ceiling Ceil 
floorE  = doubleToInt floor Floor
roundE  = doubleToInt round Round
fracE   = unOpt (snd . (properFraction :: (Double -> (Int, Double)))) Frac 
intE    = doubleToInt truncate IntOp 

