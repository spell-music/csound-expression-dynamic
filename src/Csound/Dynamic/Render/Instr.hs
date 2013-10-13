module Csound.Dynamic.Render.Instr(
    renderInstr, renderInstrBody
) where

import Control.Arrow(second)
import Control.Monad.Trans.State.Strict
import Data.List(sort, find)
import qualified Data.Map as M

import Data.Maybe(fromJust)
import Data.Fix(Fix(..), cata)
import Data.Fix.Cse(fromDag, cse)

import qualified Text.PrettyPrint.Leijen as P

import Csound.Dynamic.Tfm.DeduceTypes
import Csound.Dynamic.Tfm.UnfoldMultiOuts

import Csound.Dynamic.Types hiding (Var)
import Csound.Dynamic.Build(execDep, getRates, isMultiOutSignature)
import Csound.Dynamic.Render.Pretty

type Dag f = [(Int, f Int)]

renderInstr :: Instr -> Doc
renderInstr a = ppInstr (instrName a) $ renderInstrBody (instrBody a)

renderInstrBody :: Dep () -> Doc
renderInstrBody a = P.vcat $ flip evalState 0 $ 
    mapM (uncurry ppStmt . clearEmptyResults) $ collectRates $ toDag (execDep a)

-------------------------------------------------------------
-- E -> Dag

toDag :: E -> Dag RatedExp 
toDag expr = fromDag $ cse $ trimByArgLength expr

trimByArgLength :: E -> E
trimByArgLength = cata $ \x -> Fix x{ ratedExpExp = phi $ ratedExpExp x }
    where phi x = case x of
            Tfm info xs -> Tfm (info{infoSignature = trimInfo (infoSignature info) xs}) xs
            _ -> x
          trimInfo signature args = case signature of
            SingleRate tab -> SingleRate $ fmap trim tab
            MultiRate outs ins -> MultiRate outs (trim ins)        
            where trim = take (length args)    
                  
clearEmptyResults :: ([RatedVar], Exp RatedVar) -> ([RatedVar], Exp RatedVar)
clearEmptyResults (res, expr) = (filter ((/= Xr) . ratedVarRate) res, expr)
        
collectRates :: Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates dag = fmap (second ratedExpExp) res
    where res = unfoldMultiOuts unfoldSpec lastFreshId dag1  
          (dag1, lastFreshId) = rateGraph dag

-----------------------------------------------------------
-- Dag -> Dag

-----------------------------------------------------------
-- deduces types

rateGraph :: [Stmt RatedExp Int] -> ([Stmt RatedExp (Var Rate)], Int)
rateGraph dag = (stmts, lastId)
     where (stmts, lastId) = deduceTypes algSpec dag
           algSpec = TypeGraph mkConvert' defineType'

           mkConvert' a = (to, RatedExp Nothing Nothing $ 
                   ConvertRate (ratedVarRate to) (ratedVarRate from) $ PrimOr $ Right from)
               where from = convertFrom a
                     to   = convertTo   a

           defineType' (outVar, expr) desiredRates = (ratesForConversion, (outVar', expr'))
               where possibleRate = deduceRate desiredRates expr 
                     ratesForConversion = filter (not . flip coherentRates possibleRate) desiredRates
                     expr' = RatedExp Nothing Nothing $ rateExp possibleRate $ ratedExpExp expr
                     outVar' = ratedVar possibleRate outVar

----------------------------------------------------------
-- unfolds multiple rates

unfoldSpec :: UnfoldMultiOuts RatedExp Rate 
unfoldSpec = UnfoldMultiOuts getSelector' getParentTypes'
    where getSelector' x = case ratedExpExp x of
                Select _ order (PrimOr (Right parent)) -> Just $ Selector parent order 
                _ -> Nothing
          getParentTypes' x = case ratedExpExp x of
                Tfm i _ -> if (isMultiOutSignature $ infoSignature i) 
                           then Just (getRates $ ratedExpExp x) 
                           else Nothing 
                _ -> Nothing

coherentRates :: Rate -> Rate -> Bool
coherentRates to from = case (to, from) of
    (a, b)  | a == b    -> True
    (Xr, _)             -> True   
    (Kr, Ir)            -> True
    _                   -> False

deduceRate :: [Rate] -> RatedExp Int -> Rate
deduceRate desiredRates expr = case ratedExpExp expr of
    ExpPrim _ -> case desiredRates of
        [Sr] -> Sr
        _ -> Ir
       
    Tfm info _ -> case infoSignature info of
        MultiRate _ _ -> Xr
        SingleRate tab -> 
            let r1 = tfmNoRate (infoName info) desiredRates tab
            in  case ratedExpRate expr of
                    Just r | M.member r tab -> r
                    Just _ -> r1
                    Nothing -> r1
    
    ExpNum _ -> case ratedExpRate expr of
        Just r  -> r
        Nothing -> case maximum (Ar : desiredRates) of
            Xr -> Ar
            r -> r
    
    Select rate _ _ -> rate
    If _ _ _ -> case head $ sort desiredRates of
        Xr -> Ar
        r  -> r
    ReadVar v -> varRate v
    _  -> Xr    
    where tfmNoRate name rates tab = case sort rates of
              [Xr]  -> tfmNoRate name [Ar] tab                
              Xr:as -> tfmNoRate name as tab
              as -> fromJust $ find (flip M.member tab) (as ++ [minBound .. maxBound])         

rateExp :: Rate -> Exp Int -> Exp RatedVar 
rateExp curRate expr = case expr of
    ExpPrim (P n) | curRate == Sr -> ExpPrim (PString n)
    Tfm i xs -> Tfm i $ mergeWithPrimOrBy (flip ratedVar) xs (ratesFromSignature curRate (infoSignature i))
    Select rate pid a -> Select rate pid (fmap (ratedVar Xr) a)    
    If p t e -> If (rec2 condRate p) (rec1 curRate t) (rec1 curRate e) 
    ExpNum _ -> rec2 curRate expr    
    ReadVar v -> ReadVar v
    WriteVar v a -> WriteVar v $ rec1 (varRate v) a
    InitVar v a -> InitVar v $ rec1 (varRate v) a
    ExpPrim p -> ExpPrim p
    IfBegin _ -> rec2 condRate expr
    ElseIfBegin _ -> rec2 condRate expr
    ElseBegin -> ElseBegin
    IfEnd -> IfEnd
    EmptyExp -> EmptyExp    
    Verbatim a -> Verbatim a

    ExpBool _           -> error $ msg "ExpBool expression should be substituted"
    ConvertRate _ _ _   -> error $ msg "ConvertRate couldn't be here. It's introduced on the later stages of processing" 
    where ratesFromSignature rate signature = case signature of
              SingleRate table -> table M.! rate
              MultiRate _ rs   -> rs

          condRate :: Rate
          condRate = max Kr curRate -- Kr
          
          rec2 r = fmap (fmap (ratedVar r))  
          rec1 r = fmap (ratedVar r)

          msg txt = "Csound.Dynamic.Render.Instr.rateExp: " ++ txt 

          

mergeWithPrimOrBy :: (a -> b -> c) -> [PrimOr a] -> [b] -> [PrimOr c]
mergeWithPrimOrBy cons = zipWith (\primOr b -> fmap (flip cons b) primOr)


