module Csound.Dynamic.Render(
    renderCsd      
) where

import qualified Text.PrettyPrint.Leijen as P

import Csound.Dynamic.Types
import Csound.Dynamic.Render.Instr
import Csound.Dynamic.Render.Pretty

renderCsd :: Csd -> String
renderCsd a = show $ ppCsdFile 
    (renderFlags $ csdFlags a)
    (renderOrc   $ csdOrc a)
    (renderSco   $ csdSco a)

renderFlags :: Flags -> Doc
renderFlags = P.text

renderOrc :: Orc -> Doc
renderOrc a = vcatSep
    $ renderInstrBody (orcHead a)
    : fmap renderInstr (orcInstruments a)

renderSco :: Sco -> Doc
renderSco a = vcatSep 
    [ P.vcat $ fmap (uncurry ppGen)   $ scoGens a
    , maybe P.empty ppTotalDur $ scoTotalDur a    
    , P.vcat $ fmap (uncurry ppNotes) $ scoNotes a ]    

