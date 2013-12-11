-- | Dependency tracking
module Csound.Dynamic.Types.Dep(
    DepT(..), runDepT, LocalHistory(..),    
    -- * Dependencies
    depT, depT_, mdepT, stripDepT, stmtOnlyT, execDepT,

    -- * Variables
    newLocalVar, newLocalVars,
    writeVar, readVar, readOnlyVar, initVar, appendVarBy
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad(ap, liftM, zipWithM_)
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

instance MonadTrans DepT where
    lift ma = DepT $ lift ma
   
runDepT :: DepT m a -> m (a, LocalHistory)
runDepT a = runStateT (unDepT a) def

execDepT :: Monad m => DepT m a -> m E
execDepT a = liftM (maybe emptyE id . expDependency . snd) $ runDepT a

-- dependency tracking

depT :: Monad m => E -> DepT m E
depT a = DepT $ StateT $ \s -> do
    let x = Fix $ (unFix a) { ratedExpDepends = expDependency s }
    return (x, s { expDependency = Just x })    

depT_ :: (Monad m) => E -> DepT m ()
depT_ = fmap (const ()) . depT

mdepT :: (Monad m) => MultiOut [E] -> MultiOut (DepT m [E])
mdepT mas = \n -> mapM depT $ ( $ n) mas

stripDepT :: Monad m => DepT m a -> m a
stripDepT (DepT a) = evalStateT a def 

stmtOnlyT :: Monad m => Exp E -> DepT m ()
stmtOnlyT stmt = depT_ $ noRate stmt

emptyE :: E 
emptyE = noRate $ EmptyExp 

-- local variables

newLocalVars :: Monad m => [Rate] -> m [E] -> DepT m [Var]
newLocalVars rs vs = do
    vars <- mapM newVar rs
    zipWithM_ initVar vars =<< lift vs
    return vars

newLocalVar :: Monad m => Rate -> m E -> DepT m Var
newLocalVar rate val = do
    var <- newVar rate
    initVar var =<< lift val
    return var

newVar :: Monad m => Rate -> DepT m Var
newVar rate = DepT $ do
    s <- get
    let v = Var LocalVar rate (show $ newLocalVarId s)    
    put $ s { newLocalVarId = succ $ newLocalVarId s }
    return v

--------------------------------------------------
-- variables

-- generic funs

writeVar :: Monad m => Var -> E -> DepT m ()
writeVar v x = depT_ $ noRate $ WriteVar v $ toPrimOr x 

readVar :: Monad m => Var -> DepT m E
readVar v = depT $ noRate $ ReadVar v

readOnlyVar :: Var -> E
readOnlyVar v = noRate $ ReadVar v

initVar :: Monad m => Var -> E -> DepT m ()
initVar v x = depT_ $ setRate Ir $ noRate $ InitVar v $ toPrimOr x

appendVarBy :: Monad m => (E -> E -> E) -> Var -> E -> DepT m ()
appendVarBy op v x = writeVar v . op x =<< readVar v


