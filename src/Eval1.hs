module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC :3 uwu
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let var e) =  do
                          n <- evalExp e 
                          update var n 
                          return Skip
stepComm (Seq Skip c2) = return c2                      
stepComm (Seq c1 c2) = stepComm c1 >>= \c1' -> return (Seq c1' c2)
stepComm (IfThenElse eb c1 c2) =  do
                                    cond <- evalExp eb
                                    if cond then return c1 else return c2
stepComm w@(While eb c) = do
                          cond <- evalExp eb
                          if cond then return (Seq c w) else return Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const e) = return e
evalExp (Var e) = lookfor e
evalExp (UMinus e) = evalExp e >>= \x -> return (-x)
evalExp (Plus e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 + n2) 

evalExp (Minus e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 - n2)
evalExp (Times e1 e2) =   do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 * n2)
evalExp (Div e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 `div` n2)

evalExp BTrue = return True
evalExp BFalse = return False

evalExp (Lt e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 < n2)
evalExp (Gt e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 > n2)
evalExp (And e1 e2) = do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 && n2)
evalExp (Or e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 || n2)
evalExp (Not e) = evalExp e >>= \x -> return (not x)
evalExp (Eq e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 == n2)
evalExp (NEq e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        return (n1 /= n2)
evalExp (EAssgn var e) =  do
                            n1 <- evalExp e
                            update var n1
                            return n1     

evalExp (ESeq e1 e2) = evalExp e1 >>= \_ -> evalExp e2 >>= \x -> return x





