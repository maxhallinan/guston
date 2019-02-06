module Repl (run) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (runStateT)
import qualified System.Console.Haskeline as H

import qualified Eval as E
import qualified Parse as P
import qualified Syntax as S

run :: IO ()
run = do
  putStrLn "Welcome"
  H.runInputT H.defaultSettings (loop S.defaultEnv)

loop :: S.Env -> H.InputT IO ()
loop env = do
  mInput <- H.getInputLine "> "
  case mInput of
    Nothing     -> H.outputStrLn "Goodbye"
    Just ""     -> loop env
    Just input  -> do
      result <- liftIO $ evalInEnv env input
      either onError onSuccess result
  where onError errMsg = do
          H.outputStrLn errMsg
          loop env
        onSuccess (result, env') = do
          H.outputStrLn $ show result
          loop env'

evalInEnv :: S.Env -> String -> IO (Either String (S.Sexpr, S.Env))
evalInEnv env str = do
  case P.parseStr str of
    Right sexpr -> do
      (result, env') <- run (E.eval sexpr) env
      case result of
        Left err  -> return $ Left $ show err
        Right x   -> return $ Right (x, env')
    Left parseErr -> return $ Left $ show parseErr
  where run = runStateT . E.runEval
