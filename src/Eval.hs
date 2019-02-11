{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (EvalErr(..), ErrType(..), eval, evalFile, run, runFile, runEval) where

import Data.Traversable (sequence)
import qualified Control.Applicative as A
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Data.List (intercalate)

import Syntax ( Callframe(..)
              , Callstack
              , Env
              , Expr(..)
              , Info
              , Sexpr(..)
              , SpecialForm(..)
              , callframe
              , emptyCallstack
              , popCallframe
              , pushCallframe
              )

newtype Eval a = Eval { runEval :: E.ExceptT EvalErr (S.StateT EvalState IO) a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState EvalState
           , S.MonadIO
           , E.MonadError EvalErr
           )

data EvalState = EvalState Env Callstack

initEvalState :: Env -> EvalState
initEvalState env = EvalState env emptyCallstack

getEnv :: Eval Env
getEnv = do
  EvalState env _ <- S.get
  return env

getCallstack :: Eval Callstack
getCallstack = do
  EvalState _ stack <- S.get
  return stack

updateCallstack :: Callstack -> Eval ()
updateCallstack stack = do
  (EvalState env _) <- S.get
  S.put $ EvalState env stack

pushCallframe' :: Expr -> Eval ()
pushCallframe' expr = do
  stack <- getCallstack
  updateCallstack $ pushCallframe (callframe expr) stack

popCallframe' :: Eval ()
popCallframe' = do
  stack <- getCallstack
  updateCallstack $ popCallframe stack

updateEnv :: String -> Expr -> Eval ()
updateEnv key val = do
  (EvalState env stack) <- S.get
  S.put $ EvalState (M.insert key val env) stack

run :: Env -> Expr -> IO (Either EvalErr Expr, Env)
run env expr = resultWithEnv <$> (rn expr evalState)
  where evalState = initEvalState env
        rn = S.runStateT . E.runExceptT . runEval . eval

runFile :: Env -> [Expr] -> IO (Either EvalErr [Expr], Env)
runFile env exprs = resultWithEnv <$> (rn evaled evalState)
  where evaled = sequence (eval <$> exprs)
        evalState = initEvalState env
        rn = S.runStateT . E.runExceptT . runEval

resultWithEnv :: (a, EvalState) -> (a, Env)
resultWithEnv (result, EvalState env _) = (result, env)

data EvalErr = EvalErr ErrType Callstack

instance Show EvalErr where
  show (EvalErr errType callstack) = show errType ++ "\n" ++ cs
    where cs = intercalate "\n" $ (show . toExpr) <$> (reverse callstack)
          toExpr (Callframe expr) = expr

data ErrType
  = NumArgs
  | WrongTipe
  | LstLength
  | UnknownVar String
  | NotFn
  | Unknown
  | NotPair
  deriving (Eq)

instance Show ErrType where
  show NumArgs = "wrong number of arguments"
  show WrongTipe = "wrong type"
  show LstLength = "wrong list length"
  show (UnknownVar varName) = "unknown var: " ++ varName
  show NotFn = "not a function"
  show Unknown = "unknown error"
  show NotPair = "not a pair"

throwError :: ErrType -> Eval Expr
throwError errType = do
  callstack <- getCallstack
  E.throwError $ EvalErr errType callstack 

evalFile :: [Expr] -> Eval [Expr]
evalFile xs = sequence $ eval <$> xs

eval :: Expr -> Eval Expr
eval (Expr sexpr info) = do
  case sexpr of
    (Sym name) -> evalSym name
    (Lst ((Expr (SFrm sfrm) _):args)) -> evalSFrm info sfrm args
    (Lst xs) -> evalLst xs
    _ -> throwError Unknown

evalSym :: String -> Eval Expr
evalSym name = do
  env <- getEnv
  case M.lookup name env of
    Just expr   -> return expr
    Nothing     -> throwError $ UnknownVar name

evalSFrm :: Info -> SpecialForm -> [Expr] -> Eval Expr
evalSFrm _ _ [] = throwError NumArgs
evalSFrm info sfrm args = do
  case sfrm of
    Car     -> evalCar args
    Cdr     -> evalCdr args
    Cond    -> evalCond args
    Cons    -> evalCons info args
    Def     -> evalDef args
    IsAtm   -> evalIsAtm info args
    IsEq    -> evalIsEq info args
    Lambda  -> evalLambda args
    Quote   -> evalQuote args

evalIsAtm :: Info -> [Expr] -> Eval Expr
evalIsAtm info [x]   = do
  y <- eval x
  case y of
    Expr (Sym _) _ -> return $ Expr (Sym "t") info
    Expr (Lst []) _ -> return $ Expr (Sym "t") info
    Expr _ _ -> return $ Expr (Lst []) info
evalIsAtm _ _ = throwError NumArgs

evalIsEq :: Info -> [Expr] -> Eval Expr
evalIsEq info [x, y] = do
  (Expr x2 _, Expr y2 _) <- A.liftA2 (,) (eval x) (eval y)
  case (x2, y2) of
    (Sym name1, Sym name2) ->
      if name1 == name2
      then return $ Expr (Sym "t") info
      else return $ Expr (Lst []) info
    (Lst [], Lst []) -> return $ Expr (Sym "t") info
    (_, _)  -> return $ Expr (Lst []) info
evalIsEq _ _ = throwError NumArgs

evalCar :: [Expr] -> Eval Expr
evalCar [x] = do
  Expr e info <- eval x
  case e of
    (Lst []) -> throwError LstLength
    (Lst ((Expr y _):_)) -> return $ Expr y info
    _ -> throwError WrongTipe
evalCar _ = throwError NumArgs

evalCdr :: [Expr] -> Eval Expr
evalCdr [x] = do
  Expr e info <- eval x
  case e of
    (Lst [])      -> return $ Expr (Lst []) info
    (Lst (_:ys))  -> return $ Expr (Lst ys) info
    _             -> throwError WrongTipe
evalCdr _ = throwError NumArgs

evalCons :: Info -> [Expr] -> Eval Expr
evalCons info [x, xs] = do
  Expr e1 i1    <- eval x
  Expr e2 _     <- eval xs
  case (e1, e2) of
    (_, (Lst ys)) -> return $ Expr (Lst ((Expr e1 i1):ys)) info
    (_, _)        -> throwError WrongTipe
evalCons _ _ = throwError NumArgs

evalCond :: [Expr] -> Eval Expr
evalCond [] = throwError NumArgs
evalCond ((Expr c _):cs) =
  case c of
    Lst [] -> throwError NotPair
    Lst [p, e] -> do
      Expr x _ <- eval p
      case x of
        (Sym "t") -> eval e
        (Lst []) -> evalCond cs
        _ -> throwError WrongTipe
    Lst (_:_) -> throwError NotPair
    _         -> throwError WrongTipe

evalDef :: [Expr] -> Eval Expr
evalDef [Expr (Sym key) _, expr] = do
  val <- eval expr
  _ <- updateEnv key val
  return val
evalDef [_,_] = throwError WrongTipe
evalDef _ = throwError NumArgs

evalLambda :: [Expr] -> Eval Expr
evalLambda [Expr (Lst params) info, body] = do
  env <- getEnv
  return $ Expr (Fn env params body) info
evalLambda [_,_] = throwError WrongTipe
evalLambda _ = throwError NumArgs

evalQuote :: [Expr] -> Eval Expr
evalQuote [Expr x info] = return $ Expr x info
evalQuote _ = throwError NumArgs

evalLst :: [Expr] -> Eval Expr
evalLst [] = throwError NumArgs
evalLst (x:xs) = do
  Expr fn _ <- eval x
  case fn of
    Fn localEnv params body -> do
      globalEnv <- getEnv
      let env   = localEnv <> globalEnv
      let expr  = applyLambda params xs body
      evalInLocalEnv env expr
    _  -> throwError NotFn

applyLambda :: [Expr] -> [Expr] -> Expr -> Eval Expr
applyLambda params args body = do
  EvalState env stack <- S.get
  args'   <- traverse eval args
  let env' = M.fromList (zipWith (\(Expr (Sym k) _) v -> (k, v)) params args') <> env
  Eval $ E.mapExceptT (S.withStateT (const (EvalState env' stack))) (runEval $ eval body)

evalInLocalEnv :: Env -> Eval Expr -> Eval Expr
evalInLocalEnv env expr = do
  stack <- getCallstack
  let localState = EvalState env stack
  Eval $ E.mapExceptT (S.withStateT $ const localState) (runEval expr)
