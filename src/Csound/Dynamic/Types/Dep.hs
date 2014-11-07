-- | Dependency tracking
module Csound.Dynamic.Types.Dep(
    DepT(..), LocalHistory(..), runDepT, execDepT, evalDepT,   
    -- * Dependencies
    depT, depT_, mdepT, stripDepT, stmtOnlyT, 

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
    { expDependency :: E
    , newLineNum    :: Int
    , newLocalVarId :: Int }

instance Default LocalHistory where
    def = LocalHistory start 0 0

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

runDepT :: (Functor m, Monad m) => DepT m a -> m (a, LocalHistory)
runDepT a = runStateT (unDepT $ a) def

evalDepT :: (Functor m, Monad m) => DepT m a -> m a
evalDepT a = evalStateT (unDepT $ a) def
   
execDepT :: (Functor m, Monad m) => DepT m () -> m E
execDepT a = fmap expDependency $ execStateT (unDepT $ a) def

-- dependency tracking

start :: E
start = noRate Starts

depends :: E -> E -> E
depends a1 a2 = noRate $ Seq (toPrimOr a1) (toPrimOr a2)

end :: Monad m => E -> DepT m ()
end a = depT_ $ noRate $ Ends (toPrimOr a)

depT :: Monad m => E -> DepT m E
depT a = DepT $ do
    s <- get
    let a1 = Fix $ (unFix a) { ratedExpDepends = Just (newLineNum s) }
    put $ s { 
        newLineNum = succ $ newLineNum s, 
        expDependency = depends (expDependency s) a1 }
    return a1    

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


