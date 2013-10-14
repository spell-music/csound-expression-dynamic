module Csound.Dynamic.Build (
    
    -- * Expression tree
    -- | Working with expression tree
    toExp, onExp, 

    -- * Rates
    -- | Updating rates
    noRate, withRate, ratedExp, setRate, 
  
    -- * Queries
    getRates, isMultiOutSignature, getPrimUnsafe,

    -- * Constructors
    -- | Basic constructors
    prim, opcPrefix, oprPrefix, oprInfix, 
    numExp1, numExp2,
    tfm, pn, emptyE, withInits,
    double, int, str, verbatim,

    -- ** Opcodes constructors
    Spec1, spec1, opcs, opr1, opr1k, infOpr, oprBy,
    Specs, specs, MultiOut, mopcs, mo, 

    -- * Dependencies
    dep, dep_, mdep, stripDep, stmtOnly, execDep,

    -- * Variables
    writeVar, readVar, readOnlyVar, initVar, appendVarBy,

    -- * Instruments
    intInstr,
    
    -- * Score
    alwaysOn,

    -- * Global init statements
    setSr, setKsmps, setNchnls, setKr, setZeroDbfs
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

opcPrefix :: Name -> Signature -> Info
opcPrefix name signature = Info name signature Opcode

oprPrefix :: Name -> Signature -> Info
oprPrefix name signature = Info name signature Prefix

oprInfix :: Name -> Signature -> Info
oprInfix name signature = Info name signature Infix

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
int = prim . PrimInt

verbatim :: String -> Dep ()
verbatim = stmtOnly . Verbatim

----------------------------------------------------------------------
-- constructing opcodes

-- single output

-- User friendly type for single output type signatures
type Spec1 = [(Rate, [Rate])]

spec1 :: Spec1 -> Signature
spec1 = SingleRate . M.fromList

opcs :: Name -> Spec1 -> [E] -> E
opcs name signature = tfm (opcPrefix name $ spec1 signature)

opr1 :: Name -> E -> E
opr1 name a = tfm (oprPrefix name $ spec1 [(Ar, [Ar]), (Kr, [Kr]), (Ir, [Ir])]) [a]

oprBy :: Name -> Spec1 -> [E] -> E
oprBy name signature = tfm (oprPrefix name $ spec1 signature)

opr1k :: Name -> E -> E
opr1k name a = tfm (oprPrefix name $ spec1 [(Kr, [Kr]), (Ir, [Ir])]) [a]

infOpr :: Name -> E -> E -> E
infOpr name a b = tfm (oprInfix name $ spec1 [(Ar, [Ar, Ar]), (Kr, [Kr, Kr]), (Ir, [Ir, Ir])]) [a, b]

numExp1 :: NumOp -> E -> E
numExp1 op x = noRate $ ExpNum $ fmap toPrimOr $ PreInline op [x] 

numExp2 :: NumOp -> E -> E -> E
numExp2 op a b = noRate $ ExpNum $ fmap toPrimOr $ PreInline op [a, b]

-- multiple output

-- User friendly type for multiple outputs type signatures
type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate 

-- | Multiple output. Specify the number of outputs to get the result.
type MultiOut a = Int -> a

mopcs :: Name -> Specs -> [E] -> MultiOut [E]
mopcs name signature as = \numOfOuts -> mo numOfOuts $ tfm (opcPrefix name $ specs signature) as

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

writeVar :: Var -> E -> Dep ()
writeVar v x = dep_ $ noRate $ WriteVar v $ toPrimOr x 

readVar :: Var -> Dep E
readVar v = dep $ noRate $ ReadVar v

readOnlyVar :: Var -> E
readOnlyVar v = noRate $ ReadVar v

initVar :: Var -> E -> Dep ()
initVar v x = dep_ $ noRate $ InitVar v $ toPrimOr x

appendVarBy :: (E -> E -> E) -> Var -> E -> Dep ()
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

getPrimUnsafe :: E -> Prim
getPrimUnsafe x = case toExp x of
    ExpPrim a   -> a
    _           -> error "Csound.Dynamic.Build.getPrimUnsafe:Expression is not a primitive"

-- expression tree

toExp :: E -> Exp E
toExp = ratedExpExp . unFix

-- Lifts transformation of main expression
onExp :: (Exp E -> Exp E) -> E -> E
onExp f x = case unFix x of
    a -> Fix $ a{ ratedExpExp = f (ratedExpExp a) }

----------------------------------------------------------------
-- dependency tracking

dep :: E -> Dep E
dep a = Dep $ state $ \s -> 
    let x = Fix $ (unFix a) { ratedExpDepends = s }
    in  (x, Just x)

dep_ :: E -> Dep ()
dep_ = fmap (const ()) . dep

mdep :: MultiOut [E] -> MultiOut (Dep [E])
mdep a = mapM dep . a

stripDep :: Dep a -> a
stripDep (Dep a) = evalState a Nothing

stmtOnly :: Exp E -> Dep ()
stmtOnly stmt = dep_ $ noRate stmt

execDep :: Dep a -> E
execDep a = maybe emptyE id $ snd $ runDep a

----------------------------------------------------------------
-- instruments

intInstr :: Int -> Dep () -> Instr
intInstr n expr = Instr (intInstrId n) expr

----------------------------------------------------------------
-- score

alwaysOn :: InstrId -> (InstrId, [CsdEvent Note])
alwaysOn instrId = (instrId, [(0, -1, [])])

----------------------------------------------------------------
-- global inits

setSr, setKsmps, setNchnls, setKr :: Int -> Dep ()
    
setZeroDbfs :: Double -> Dep ()

setSr       = gInit "sr"
setKr       = gInit "kr"
setNchnls   = gInit "nchnls"
setKsmps    = gInit "ksmps"
setZeroDbfs = gInitDouble "0dbfs"

gInit :: String -> Int -> Dep ()
gInit name val = writeVar (VarVerbatim Ir name) (int val)

gInitDouble :: String -> Double -> Dep ()
gInitDouble name val = writeVar (VarVerbatim Ir name) (double val)

