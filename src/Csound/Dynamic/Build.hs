module Csound.Dynamic.Build (
    
    -- * Expression tree
    -- | Working with expression tree
    toExp, onExp, 

    -- * Rates
    -- | Updating rates
    noRate, withRate, ratedExp, setRate, 
  
    -- * Queries
    getRates, isMultiOutSignature,

    -- * Constructors
    -- | Basic constructors
    prim, pref, tfm, pn, emptyE, withInits,
    double, int, str,

    -- ** Opcodes constructors
    Spec1, spec1, opcs, 
    Specs, specs, mopcs, mo,

    -- * Dependencies
    se, se_, stripSE, stmtOnly, execSE,

    -- * Variables
    writeVar, readVar, readOnlyVar, initVar, appendVarBy,

    -- * Instruments
    intInstr
) where

import Control.Monad.Trans.State.Strict
import qualified Data.Map as M(fromList)

import Data.Fix(Fix(..))

import Csound.Dynamic.Types

------------------------------------------------
-- basic constructors
  
noRate :: Exp E -> E
noRate = ratedExp Nothing
  
withRate :: Rate -> Exp E -> E
withRate r = ratedExp (Just r)

ratedExp :: Maybe Rate -> Exp E -> E
ratedExp r = Fix . RatedExp r Nothing

prim :: Prim -> E
prim = noRate . ExpPrim 

pref :: Name -> Signature -> Info
pref name signature = Info name signature Prefix Nothing

tfm :: Info -> [E] -> E
tfm info args = noRate $ Tfm info $ fmap toPrimOr args

pn :: Int -> E
pn = prim . P

emptyE :: E 
emptyE = noRate $ EmptyExp 

withInits :: E -> [E] -> E
withInits a es = onExp phi a
    where phi x = case x of
            Tfm t xs -> Tfm t (xs ++ (fmap toPrimOr es))
            _        -> x

-- | Converts Haskell's doubles to Csound's doubles
double :: Double -> E
double = prim . PrimDouble

-- | Converts Haskell's strings to Csound's strings
str :: String -> E
str = prim . PrimString

-- | Converts Haskell's integers to Csound's doubles
int :: Int -> E
int = double . fromIntegral

----------------------------------------------------------------------
-- constructing opcodes

-- single output

-- User friendly type for single output type signatures
type Spec1 = [(Rate, [Rate])]

spec1 :: Spec1 -> Signature
spec1 = SingleRate . M.fromList

opcs :: Name -> Spec1 -> [E] -> E
opcs name signature = tfm (pref name $ spec1 signature)

-- multiple output

-- User friendly type for multiple outputs type signatures
type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate 

mopcs :: Int -> Name -> Specs -> [E] -> [E]
mopcs numOfOuts name signature as = mo numOfOuts $ tfm (pref name $ specs signature) as

mo :: Int -> E -> [E]
mo n e = zipWith (\cellId r -> select cellId r e') [0 ..] outRates
    where outRates = take n $ getRates $ toExp e          
          e' = onExp (setMultiRate outRates) e
          
          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs 
              where ins = case infoSignature info of
                        MultiRate _ a -> a
                        _ -> error "Tuple.hs: multiOutsSection -- should be multiOut expression" 
          setMultiRate _ _ = error "Tuple.hs: multiOutsSection -- argument should be Tfm-expression"  
            
          select cellId rate expr = withRate rate $ Select rate cellId (PrimOr $ Right expr)


--------------------------------------------------
-- variables

-- generic funs

writeVar :: Var -> E -> SE ()
writeVar v x = se_ $ noRate $ WriteVar v $ toPrimOr x 

readVar :: Var -> SE E
readVar v = se $ noRate $ ReadVar v

readOnlyVar :: Var -> E
readOnlyVar v = noRate $ ReadVar v

initVar :: Var -> E -> SE ()
initVar v x = se_ $ noRate $ InitVar v $ toPrimOr x

appendVarBy :: (E -> E -> E) -> Var -> E -> SE ()
appendVarBy op v x = writeVar v . op x =<< readVar v

-- rate coversion

setRate :: Rate -> E -> E
setRate r a = Fix $ (\x -> x { ratedExpRate = Just r }) $ unFix a


getRates :: MainExp a -> [Rate]
getRates (Tfm info _) = case infoSignature info of
    MultiRate outs _ -> outs
    _ -> error "Build.hs:getRates - argument should be multiOut"
getRates _ = error "Build.hs:getRates - argument should be Tfm-expression"

    
isMultiOutSignature :: Signature -> Bool
isMultiOutSignature x = case x of
    MultiRate _ _ -> True
    _ -> False

-- expression tree

toExp :: E -> Exp E
toExp = ratedExpExp . unFix

-- Lifts transformation of main expression
onExp :: (Exp E -> Exp E) -> E -> E
onExp f x = case unFix x of
    a -> Fix $ a{ ratedExpExp = f (ratedExpExp a) }

----------------------------------------------------------------
-- dependency tracking

se :: E -> SE E
se a = SE $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = s }
    in  (x, Just x)

se_ :: E -> SE ()
se_ = fmap (const ()) . se

stripSE :: SE a -> a
stripSE (SE a) = evalState a Nothing

stmtOnly :: Exp E -> SE ()
stmtOnly stmt = se_ $ noRate stmt

execSE :: SE a -> E
execSE a = maybe emptyE id $ snd $ runSE a

----------------------------------------------------------------
-- instruments

intInstr :: Int -> SE () -> Instr
intInstr n expr = Instr (intInstrId n) expr


