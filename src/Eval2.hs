module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f  = StateError(\s -> runStateError m s >>= \(v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError(\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  -- lookfor v = State (\s -> (lookfor' v s :!: s))
  --   where lookfor' v s = fromJust $ M.lookup v s
  -- update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert
  lookfor v = StateError(\s ->  case M.lookup v s of
                                  Just n -> Right (n :!: s)
                                  _      -> Left UndefVar)

  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
          Right(_ :!: s) -> Right s
          Left x         -> Left x --Left Error


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
                        if n2 == 0 then throw DivByZero else return (n1 `div` n2)

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



