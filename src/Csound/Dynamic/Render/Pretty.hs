module Csound.Dynamic.Render.Pretty(
    Doc, vcatSep,
    ppCsdFile, ppGen, ppNotes, ppInstr, ppStmt, ppTotalDur
) where

import Control.Monad.Trans.State.Strict
import Data.Char(toLower)
import qualified Data.IntMap as IM

import Text.PrettyPrint.Leijen

import Csound.Dynamic.Types

vcatSep :: [Doc] -> Doc
vcatSep = vcat . punctuate line

binaries, unaries, funcs :: String -> [Doc] -> Doc

binaries op as = binary op (as !! 0) (as !! 1)
unaries  op as = unary  op (as !! 0)
funcs    op as = func   op (as !! 0)

binary :: String -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> text op <+> b

unary :: String -> Doc -> Doc
unary op a = parens $ text op <> a

func :: String -> Doc -> Doc
func op a = text op <> parens a

ppCsdFile :: Doc -> Doc -> Doc -> Doc
ppCsdFile flags orc sco = 
    tag "CsoundSynthesizer" $ vcatSep [
        tag "CsOptions" flags,
        tag "CsInstruments" orc,
        tag "CsScore" sco]    

tag :: String -> Doc -> Doc
tag name content = vcatSep [
    char '<' <> text name <> char '>', 
    content, 
    text "</" <> text name <> char '>']  

ppNotes :: InstrId -> [CsdEvent Note] -> Doc
ppNotes instrId = vcat . fmap (ppNote instrId)

ppNote :: InstrId -> CsdEvent Note -> Doc
ppNote instrId evt = char 'i' 
    <+> ppInstrId instrId 
    <+> double (csdEventStart evt) <+> double (csdEventDur evt) 
    <+> hsep (fmap ppPrim $ csdEventContent evt)

ppPrim :: Prim -> Doc
ppPrim x = case x of
    P n -> char 'p' <> int n
    PrimInstrId a -> ppInstrId a
    PString a -> int a    
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> dquotes $ text s
    
ppGen :: Int -> Gen -> Doc
ppGen tabId ft = char 'f' 
    <>  int tabId 
    <+> int 0 
    <+> (int $ genSize ft)
    <+> (int $ genId ft) 
    <+> (maybe empty (text . show) $ genFile ft)
    <+> (hsep $ map double $ genArgs ft)

ppInstr :: InstrId -> Doc -> Doc
ppInstr instrId body = vcat [
    text "instr" <+> ppInstrId instrId,
    body,
    text "endin"]

ppInstrId :: InstrId -> Doc
ppInstrId x = case x of
    InstrId den nom -> int nom <> maybe empty ppAfterDot den 
    InstrLabel name -> dquotes $ text name
    where ppAfterDot a = text $ ('.': ) $ reverse $ show a

type TabDepth = Int

ppStmt :: [RatedVar] -> Exp RatedVar -> State TabDepth Doc
ppStmt outs expr = ppExp (ppOuts outs) expr

ppExp :: Doc -> Exp RatedVar -> State TabDepth Doc
ppExp res expr = case fmap ppPrimOrVar expr of
    ExpPrim (PString n)             -> tab $ ppStrget res n
    ExpPrim p                       -> tab $ res $= ppPrim p
    Tfm info [a, b] | isInfix  info -> tab $ res $= binary (infoName info) a b
    Tfm info xs                     -> tab $ ppOpc res (infoName info) xs
    ConvertRate to from x           -> tab $ ppConvertRate res to from x
    If info t e                     -> tab $ ppIf res (ppCond info) t e
    ExpNum (PreInline op as)        -> tab $ res $= ppNumOp op as
    WriteVar v a                    -> tab $ ppVar v $= a
    InitVar v a                     -> tab $ ppOpc (ppVar v) "init" [a]
    ReadVar v                       -> tab $ res $= ppVar v

    IfBegin a                       -> succTab          $ text "if "     <> ppCond a <> text " then"
    ElseIfBegin a                   -> left >> (succTab $ text "elseif " <> ppCond a <> text " then")    
    ElseBegin                       -> left >> (succTab $ text "else")
    IfEnd                           -> left >> (tab     $ text "endif")
    EmptyExp                        -> return empty
    Verbatim str                    -> return $ text str
    x -> error $ "unknown expression: " ++ show x
    where tab doc = fmap (shiftByTab doc) get 
          tabWidth = 4
          shiftByTab doc n
            | n == 0    = doc
            | otherwise = (text $ replicate (tabWidth * n) ' ') <> doc 

          left = modify pred
          succTab doc = do
            a <- tab doc
            modify succ
            return a

ppCond :: Inline CondOp Doc -> Doc
ppCond = ppInline ppCondOp 

($=) :: Doc -> Doc -> Doc
($=) a b = a <+> equals <+> b

ppOuts :: [RatedVar] -> Doc
ppOuts xs = hsep $ punctuate comma $ map ppRatedVar xs

ppPrimOrVar :: PrimOr RatedVar -> Doc
ppPrimOrVar x = either ppPrim ppRatedVar $ unPrimOr x

ppStrget :: Doc -> Int -> Doc
ppStrget out n = ppOpc out "strget" [char 'p' <> int n]
 
ppIf :: Doc -> Doc -> Doc -> Doc -> Doc
ppIf res p t e = vcat 
    [ text "if" <+> p <+> text "then"
    , text "    " <> res <+> char '=' <+> t
    , text "else"
    , text "    " <> res <+> char '=' <+> e
    , text "endif" 
    ]

ppOpc :: Doc -> String -> [Doc] -> Doc
ppOpc out name xs = out <+> ppProc name xs

ppProc :: String -> [Doc] -> Doc
ppProc name xs = text name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name   -> ppVarType ty <> ppRate rate <> text (varPrefix ty : name)
    VarVerbatim _ name -> text name

varPrefix :: VarType -> Char
varPrefix x = case x of
    LocalVar  -> 'l'
    GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar  -> empty
    GlobalVar -> char 'g'

ppConvertRate :: Doc -> Rate -> Rate -> Doc -> Doc
ppConvertRate out to from var = case (to, from) of
    (Ar, Kr) -> upsamp var 
    (Ar, Ir) -> upsamp $ k var
    (Kr, Ar) -> downsamp var
    (Kr, Ir) -> out $= k var
    (Ir, Ar) -> downsamp var
    (Ir, Kr) -> out $= i var
    (a, b)   -> error $ "bug: no rate conversion from " ++ show b ++ " to " ++ show a ++ "."
    where 
        upsamp x = ppOpc out "upsamp" [x]
        downsamp x = ppOpc out "downsamp" [x]
        k = func "k"
        i = func "i"

-- expressions

ppInline :: (a -> [Doc] -> Doc) -> Inline a Doc -> Doc
ppInline ppNode a = iter $ inlineExp a    
    where iter x = case x of
              InlinePrim n        -> inlineEnv a IM.! n
              InlineExp op args   -> ppNode op $ fmap iter args  

-- booleans

ppCondOp :: CondOp -> [Doc] -> Doc  
ppCondOp op = case op of
    TrueOp            -> const $ text "(1 == 1)"                
    FalseOp           -> const $ text "(0 == 1)"
    And               -> bi "&&"
    Or                -> bi "||"
    Equals            -> bi "=="
    NotEquals         -> bi "!="
    Less              -> bi "<"
    Greater           -> bi ">"
    LessEquals        -> bi "<="    
    GreaterEquals     -> bi ">="                         
    where bi  = binaries 
          
-- numeric

ppNumOp :: NumOp -> [Doc] -> Doc
ppNumOp op = case  op of
    Add -> bi "+"
    Sub -> bi "-"
    Mul -> bi "*"
    Div -> bi "/"
    Neg -> uno "-"    
    Pow -> bi "^"    
    Mod -> bi "%"
    
    ExpOp -> fun "exp"
    IntOp -> fun "int" 
    
    x -> fun (firstLetterToLower $ show x)        
    where 
        bi  = binaries
        uno = unaries
        fun = funcs
        firstLetterToLower xs = case xs of
            a:as -> toLower a : as
            [] -> error "ppNumOp firstLetterToLower: empty identifier"

ppRatedVar :: RatedVar -> Doc
ppRatedVar v = ppRate (ratedVarRate v) <> int (ratedVarId v)

ppRate :: Rate -> Doc
ppRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

ppTotalDur :: Double -> Doc
ppTotalDur d = text "f0" <+> double d

