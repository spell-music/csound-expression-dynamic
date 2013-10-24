-- | The Csound file
module Csound.Dynamic.Types.CsdFile(            
    Csd(..), Flags, Orc(..), Sco(..), Instr(..),
    intInstr, alwaysOn
) where

import Csound.Dynamic.Types.Exp
import Csound.Dynamic.Types.Dep
import Csound.Dynamic.Types.Flags
import Csound.Dynamic.Types.EventList

data Csd m = Csd
    { csdFlags  :: Flags
    , csdOrc    :: Orc m
    , csdSco    :: Sco
    } 

data Orc m = Orc
    { orcHead           :: DepT m ()
    , orcInstruments    :: [Instr m]
    }

data Instr m = Instr
    { instrName :: InstrId
    , instrBody :: DepT m ()
    }

data Sco = Sco 
    { scoTotalDur   :: Maybe Double
    , scoGens       :: [(Int, Gen)]
    , scoNotes      :: [(InstrId, [CsdEvent Note])]  }

----------------------------------------------------------------
-- instruments

intInstr :: Monad m => Int -> DepT m () -> Instr m
intInstr n expr = Instr (intInstrId n) expr

----------------------------------------------------------------
-- score

alwaysOn :: InstrId -> (InstrId, [CsdEvent Note])
alwaysOn instrId = (instrId, [(0, -1, [])])

