-- | The Csound file
module Csound.Dynamic.Types.CsdFile(            
    Csd(..), Flags, Orc(..), Sco(..), Instr(..), InstrBody,
    intInstr, alwaysOn
) where

import Csound.Dynamic.Types.Exp
import Csound.Dynamic.Types.Flags
import Csound.Dynamic.Types.EventList

data Csd = Csd
    { csdFlags  :: Flags
    , csdOrc    :: Orc
    , csdSco    :: Sco
    } 

data Orc = Orc
    { orcHead           :: InstrBody
    , orcInstruments    :: [Instr]
    }

type InstrBody = E

data Instr = Instr
    { instrName :: InstrId
    , instrBody :: InstrBody
    }

data Sco = Sco 
    { scoTotalDur   :: Maybe Double
    , scoGens       :: [(Int, Gen)]
    , scoNotes      :: [(InstrId, [CsdEvent Note])]  }

----------------------------------------------------------------
-- instruments

intInstr :: Int -> E -> Instr
intInstr n expr = Instr (intInstrId n) expr

----------------------------------------------------------------
-- score

alwaysOn :: InstrId -> (InstrId, [CsdEvent Note])
alwaysOn instrId = (instrId, [(0, -1, [])])

