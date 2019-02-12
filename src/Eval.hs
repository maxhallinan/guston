{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
  ( EvalErr(..)
  , ErrType(..)
  , eval
  , evalFile
  , run
  , runFile
  , runEval
  ) where

import qualified Control.Applicative as A
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Data.Traversable (sequence)
import Syntax (Env, Expr(..), Info, SpecialForm(..), XExpr(..))

newtype Eval a = Eval
  { runEval :: E.ExceptT EvalErr (S.StateT EvalState IO) a
  } deriving ( Applicative
             , Functor
             , Monad
             , S.MonadState EvalState
             , S.MonadIO
             , E.MonadError EvalErr
             )

data EvalState =
  EvalState Env

initEvalState :: Env -> EvalState
initEvalState env = EvalState env

getEnv :: Eval Env
getEnv = do
  EvalState env <- S.get
  return env

updateEnv :: String -> XExpr -> Eval ()
updateEnv key val = do
  (EvalState env) <- S.get
  S.put $ EvalState (M.insert key val env)

run :: Env -> XExpr -> IO (Either EvalErr XExpr, Env)
run env expr = resultWithEnv <$> (rn expr evalState)
  where
    evalState = initEvalState env
    rn = S.runStateT . E.runExceptT . runEval . eval

runFile :: Env -> [XExpr] -> IO (Either EvalErr [XExpr], Env)
runFile env exprs = resultWithEnv <$> (rn evaled evalState)
  where
    evaled = sequence (eval <$> exprs)
    evalState = initEvalState env
    rn = S.runStateT . E.runExceptT . runEval

resultWithEnv :: (a, EvalState) -> (a, Env)
resultWithEnv (result, EvalState env) = (result, env)

data EvalErr =
  EvalErr ErrType
          Info

instance Show EvalErr where
  show (EvalErr errType info) = show errType ++ " " ++ (show info)

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

throwError :: ErrType -> Info -> Eval XExpr
throwError errType info = do
  E.throwError $ EvalErr errType info

evalFile :: [XExpr] -> Eval [XExpr]
evalFile xs = sequence $ eval <$> xs

eval :: XExpr -> Eval XExpr
eval (XExpr sexpr info) = do
  case sexpr of
    (Sym name) -> evalSym info name
    (Lst ((XExpr (SFrm sfrm) _):args)) -> evalSFrm info sfrm args
    (Lst xs) -> evalLst info xs
    _ -> throwError Unknown info

evalSym :: Info -> String -> Eval XExpr
evalSym info name = do
  env <- getEnv
  case M.lookup name env of
    Just expr -> return expr
    Nothing -> throwError (UnknownVar name) info

evalSFrm :: Info -> SpecialForm -> [XExpr] -> Eval XExpr
evalSFrm info _ [] = throwError NumArgs info
evalSFrm info sfrm args = do
  case sfrm of
    First -> evalFirst info args
    Rest -> evalRest info args
    If -> evalIf info args
    Cons -> evalCons info args
    Def -> evalDef info args
    IsAtm -> evalIsAtm info args
    IsEq -> evalIsEq info args
    Lambda -> evalLambda info args
    Quote -> evalQuote info args

evalIsAtm :: Info -> [XExpr] -> Eval XExpr
evalIsAtm info [x] = do
  y <- eval x
  case y of
    XExpr (Sym _) _ -> return $ XExpr (Sym "t") info
    XExpr (Lst []) _ -> return $ XExpr (Sym "t") info
    XExpr _ _ -> return $ XExpr (Lst []) info
evalIsAtm info _ = throwError NumArgs info

evalIsEq :: Info -> [XExpr] -> Eval XExpr
evalIsEq info [x, y] = do
  (XExpr x2 _, XExpr y2 _) <- A.liftA2 (,) (eval x) (eval y)
  case (x2, y2) of
    (Sym name1, Sym name2) ->
      if name1 == name2
        then return $ XExpr (Sym "t") info
        else return $ XExpr (Lst []) info
    (Lst [], Lst []) -> return $ XExpr (Sym "t") info
    (_, _) -> return $ XExpr (Lst []) info
evalIsEq info _ = throwError NumArgs info

evalFirst :: Info -> [XExpr] -> Eval XExpr
evalFirst _ [x] = do
  XExpr e info <- eval x
  case e of
    (Lst []) -> throwError LstLength info
    (Lst ((XExpr y _):_)) -> return $ XExpr y info
    _ -> throwError WrongTipe info
evalFirst info _ = throwError NumArgs info

evalRest :: Info -> [XExpr] -> Eval XExpr
evalRest _ [x] = do
  XExpr e info <- eval x
  case e of
    (Lst []) -> return $ XExpr (Lst []) info
    (Lst (_:ys)) -> return $ XExpr (Lst ys) info
    _ -> throwError WrongTipe info
evalRest info _ = throwError NumArgs info

evalCons :: Info -> [XExpr] -> Eval XExpr
evalCons info [x, xs] = do
  XExpr e1 i1 <- eval x
  XExpr e2 _ <- eval xs
  case (e1, e2) of
    (_, (Lst ys)) -> return $ XExpr (Lst ((XExpr e1 i1) : ys)) info
    (_, _) -> throwError WrongTipe info
evalCons info _ = throwError NumArgs info

evalIf :: Info -> [XExpr] -> Eval XExpr
evalIf _ [c, e1, e2] = do
  x <- eval c
  case x of
    XExpr (Lst []) _ -> eval e2
    XExpr _ _ -> eval e1
evalIf info _ = throwError NumArgs info

evalDef :: Info -> [XExpr] -> Eval XExpr
evalDef _ [XExpr (Sym key) _, expr] = do
  val <- eval expr
  _ <- updateEnv key val
  return val
evalDef info [_, _] = throwError WrongTipe info
evalDef info _ = throwError NumArgs info

evalLambda :: Info -> [XExpr] -> Eval XExpr
evalLambda _ [XExpr (Lst params) info, body] = do
  env <- getEnv
  return $ XExpr (Fn env params body) info
evalLambda info [_, _] = throwError WrongTipe info
evalLambda info _ = throwError NumArgs info

evalQuote :: Info -> [XExpr] -> Eval XExpr
evalQuote _ [XExpr x info] = return $ XExpr x info
evalQuote info _ = throwError NumArgs info

evalLst :: Info -> [XExpr] -> Eval XExpr
evalLst info [] = throwError NumArgs info
evalLst _ (x:xs) = do
  XExpr fn info <- eval x
  case fn of
    Fn localEnv params body -> do
      globalEnv <- getEnv
      let env = localEnv <> globalEnv
      let expr = applyLambda params xs body
      evalInLocalEnv env expr
    _ -> throwError NotFn info

applyLambda :: [XExpr] -> [XExpr] -> XExpr -> Eval XExpr
applyLambda params args body = do
  EvalState env <- S.get
  args' <- traverse eval args
  let env' =
        M.fromList (zipWith (\(XExpr (Sym k) _) v -> (k, v)) params args') <>
        env
  Eval $
    E.mapExceptT (S.withStateT (const (EvalState env'))) (runEval $ eval body)

evalInLocalEnv :: Env -> Eval XExpr -> Eval XExpr
evalInLocalEnv env expr = do
  let localState = EvalState env
  Eval $ E.mapExceptT (S.withStateT $ const localState) (runEval expr)
