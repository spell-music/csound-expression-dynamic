module Csound.Dynamic.Render(
    renderCsd      
) where

import qualified Text.PrettyPrint.Leijen as P

import Csound.Dynamic.Render.Instr
import Csound.Dynamic.Render.Pretty
import Csound.Dynamic.Types

renderCsd :: (Functor m, Monad m) => Csd m -> m String
renderCsd a = do
    orcDoc <- renderOrc $ csdOrc a
    return $ show $ ppCsdFile 
                (renderFlags $ csdFlags a)
                orcDoc
                (renderSco   $ csdSco a)

renderFlags :: Flags -> Doc
renderFlags = P.pretty

renderOrc :: (Functor m, Monad m) => Orc m -> m Doc
renderOrc a = do
    headExpr    <- renderInstrBody (orcHead a)
    instrExprs  <- mapM renderInstr (orcInstruments a)
    return $ vcatSep $ headExpr : instrExprs

renderSco :: Sco -> Doc
renderSco a = vcatSep 
    [ P.vcat $ fmap (uncurry ppGen)   $ scoGens a
    , maybe P.empty ppTotalDur $ scoTotalDur a    
    , P.vcat $ fmap (uncurry ppNotes) $ scoNotes a ]    

