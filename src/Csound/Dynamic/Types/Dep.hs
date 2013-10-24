-- | Dependency tracking
module Csound.Dynamic.Types.Dep(
    DepT(..), runDepT, 
    depT, depT_, mdepT, stripDepT, stmtOnlyT, execDepT            
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad(ap, liftM)
import Data.Default

import Data.Fix(Fix(..))

import Csound.Dynamic.Types.Exp

-- | Csound's synonym for 'IO'-monad. 'Dep' means Side Effect. 
-- You will bump into 'Dep' trying to read and write to delay lines,
-- making random signals or trying to save your audio to file. 
-- Instrument is expected to return a value of @Dep [Sig]@. 
-- So it's okay to do some side effects when playing a note.
newtype DepT m a = DepT { unDepT :: StateT LocalHistory m a }

data LocalHistory = LocalHistory
    { expDependency :: Maybe E
    , newLocalVarId :: Int }

instance Default LocalHistory where
    def = LocalHistory Nothing 0

instance Monad m => Functor (DepT m) where
    fmap = liftM 

instance Monad m => Applicative (DepT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (DepT m) where
    return = DepT . return
    ma >>= mf = DepT $ unDepT ma >>= unDepT . mf

runDepT :: DepT m a -> m (a, LocalHistory)
runDepT a = runStateT (unDepT a) def

execDepT :: Monad m => DepT m a -> m E
execDepT a = liftM (maybe emptyE id . expDependency . snd) $ runDepT a

-- dependency tracking

depT :: Monad m => m E -> DepT m E
depT ma = DepT $ StateT $ \s -> do
    a <- ma
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    return (x, s { expDependency = Just x })    

depT_ :: (Monad m) => m E -> DepT m ()
depT_ = fmap (const ()) . depT

mdepT :: (Monad m) => m (MultiOut [E]) -> MultiOut (DepT m [E])
mdepT mas = \n -> mapM (depT . return) =<< DepT (lift (liftM ( $ n) mas))

stripDepT :: Monad m => DepT m a -> m a
stripDepT (DepT a) = evalStateT a def 

stmtOnlyT :: Monad m => Exp E -> DepT m ()
stmtOnlyT stmt = depT_ $ return $ noRate stmt

emptyE :: E 
emptyE = noRate $ EmptyExp 

-- local variables


