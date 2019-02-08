module Repl where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (between)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (runStateT)
import Data.Void (Void)
import qualified System.Console.Haskeline as H
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec as Mega

import qualified Eval as E
import qualified Parse as P
import qualified Syntax as S

run :: IO ()
run = do
  putStrLn "Welcome to Guston 0.1.0"
  H.runInputT H.defaultSettings (loop S.defaultEnv)

data ReplCmd =
    Eval String
  | Load [String]
  | Quit
  deriving (Show)

loop :: S.Env -> H.InputT IO ()
loop env = do
  mInput <- H.getInputLine "> "
  case mInput of
    Nothing     -> H.outputStrLn "Goodbye"
    Just ""     -> loop env
    Just cmd    -> case parseReplCmd cmd of
      Just (Eval gustonStr)    -> runEvalCmd env gustonStr
      Just (Load filepaths) -> runLoadCmd env filepaths
      Just Quit             -> runQuitCmd
      Nothing               -> runUnknownCmd env cmd

runEvalCmd :: S.Env -> String -> H.InputT IO ()
runEvalCmd env gustonStr = do
  result <- liftIO $ evalInEnv env gustonStr
  either onError onSuccess result
  where onError errMsg = do
          H.outputStrLn errMsg
          loop env
        onSuccess (result, env') = do
          H.outputStrLn $ show result
          loop env'

runLoadCmd :: S.Env -> [String] -> H.InputT IO ()
runLoadCmd env filepaths = do
  files   <- liftIO $ traverse readFile filepaths
  result  <- liftIO $ loadFiles env (zip filepaths files)
  case result of
    Left (errMsg, env') -> do
      H.outputStrLn errMsg
      loop env'
    Right env' -> do
      loop env'

loadFiles :: S.Env -> [(String, String)] -> IO (Either (String, S.Env) S.Env)
loadFiles env [] = return $ Right env
loadFiles env ((filepath, file):files) = do
  case P.parseFile filepath file of
    Left parseErr -> return $ Left (show parseErr, env)
    Right sexpr   -> do
      (result, env') <- E.runFile env sexpr
      case result of
        Left evalErr -> return $ Left (show evalErr, env')
        Right _      -> do
          putStrLn $ "Loaded " ++ filepath
          loadFiles env' files

runQuitCmd :: H.InputT IO ()
runQuitCmd = H.outputStrLn "Goodbye"

runUnknownCmd :: S.Env -> String -> H.InputT IO ()
runUnknownCmd env cmd = do
  H.outputStrLn $ "Unknown command: \"" ++ cmd ++ "\""
  loop env

evalInEnv :: S.Env -> String -> IO (Either String (S.Sexpr, S.Env))
evalInEnv env str = do
  case P.parseStr str of
    Right sexpr -> do
      (result, env') <- E.run env sexpr
      case result of
        Left err  -> return $ Left $ show err
        Right x   -> return $ Right (x, env')
    Left parseErr -> return $ Left $ show parseErr

type Parser = Mega.Parsec Void String

parseReplCmd :: String -> Maybe ReplCmd
parseReplCmd = Mega.parseMaybe replCmd

replCmd :: Parser ReplCmd
replCmd = between Char.space (Char.space <|> Mega.eof) (loadCmd <|> quitCmd <|> evalCmd)

evalCmd :: Parser ReplCmd
evalCmd = Eval <$> Mega.some Mega.anySingle

quitCmd :: Parser ReplCmd
quitCmd = Char.string ":quit" >> return Quit

loadCmd :: Parser ReplCmd
loadCmd = do
  _ <- Char.string ":load"
  _ <- Char.space
  filenames  <- Mega.sepBy filename Char.space
  return $ Load filenames
  where filename = Mega.some $ Mega.anySingleBut ' '
