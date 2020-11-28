module Eval3
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

-- Ejercicio 3.a: Proponer una nueva monada que lleve el costo de las 
-- operaciones efectuadas en la computacion, ademas de manejar errores y 
-- estado, y de su instancia de mónada. Llamela StateErrorCost.
newtype StateErrorCost a = 
  StateErrorCost {runStateErrorCost :: Env -> Either Error (Pair a (Env, Cost))}

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
  pure  = return
  (<*>) = ap
  
instance Monad StateErrorCost where
  return x = StateErrorCost (\s -> Right (x :!: (s,0)))
  m >>= f  = StateErrorCost (\s -> runStateErrorCost m s 
              >>= \(v :!: (s',c)) -> runStateErrorCost (f v) s'
              >>= \(v' :!: (s'', c')) -> Right (v' :!: (s'', c' + c))) 

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost.
instance MonadCost StateErrorCost where
  tick = StateErrorCost(\s -> Right (() :!: (s,1)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost.
instance MonadError StateErrorCost where
  throw e = StateErrorCost(\s -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost.
instance MonadState StateErrorCost where
  lookfor v = StateErrorCost (\s ->  case M.lookup v s of
                                  Just n -> Right (n :!: (s,0))
                                  _      -> Left UndefVar)

  update v i = StateErrorCost (\s -> Right (() :!: (update' v i s, 0))) where update' = M.insert


-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorCost.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Cost)
eval p = case runStateErrorCost (stepCommStar p) initEnv of
          Right(_ :!: (s,c)) -> Right (s,c)
          Left x             -> Left x --Left Error

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadCost m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> m a
evalExp (Const e) = return e
evalExp (Var e) = lookfor e
evalExp (UMinus e) = tick >>= \_ -> evalExp e >>= \x -> return (-x)
evalExp (Plus e1 e2) =  do
                          n1 <- evalExp e1
                          n2 <- evalExp e2
                          tick
                          return (n1 + n2) 

evalExp (Minus e1 e2) =  do
                          n1 <- evalExp e1
                          n2 <- evalExp e2
                          tick
                          return (n1 - n2)

evalExp (Times e1 e2) =   do
                            n1 <- evalExp e1
                            n2 <- evalExp e2
                            tick
                            tick
                            return (n1 * n2)
evalExp (Div e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        tick
                        if n2 == 0 then throw DivByZero else return (n1 `div` n2)

evalExp BTrue = return True
evalExp BFalse = return False

evalExp (Lt e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 < n2)
evalExp (Gt e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 > n2)
evalExp (And e1 e2) = do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 && n2)
evalExp (Or e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 || n2)
evalExp (Not e) = evalExp e >>= \x -> return (not x)
evalExp (Eq e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 == n2)
evalExp (NEq e1 e2) =  do
                        n1 <- evalExp e1
                        n2 <- evalExp e2
                        tick
                        return (n1 /= n2)
evalExp (EAssgn var e) =  do
                            n1 <- evalExp e
                            update var n1
                            return n1     

evalExp (ESeq e1 e2) = evalExp e1 >>= \_ -> evalExp e2 >>= \x -> return x

