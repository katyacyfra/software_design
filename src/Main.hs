module Main where
import Command
import Parse (parseLine)
import Environment
import Sentence
import System.IO.Error (catchIOError)
import System.IO (hFlush, stdout)
import Control.Monad.State (runStateT, StateT, State, modify, get, liftIO)
import Control.Monad.Except (runExcept, lift)

runPipeline :: [IOCommand] -> IO String
runPipeline = (`catchIOError` (pure . show)) . foldl (>>=) (pure "")

evalCmd :: Command -> StateT Environment IO IOCommand
evalCmd (Assign k v) = modify ((k, v) :) >> pure (const (pure ""))
evalCmd (Run c a)    = pure (run c a)

eval :: String -> StateT Environment IO String
eval s = do env <- get
            case parseLine s of
              Right p -> case runExcept (mapM (subst env) p) of
                           Right c -> do
                               cmds <- mapM evalCmd c
                               lift (runPipeline cmds)
                           Left  e -> pure $ show e
              Left e -> pure $ show e

mainLoop :: StateT Environment IO ()
mainLoop = do liftIO (putStr ">")
              liftIO (hFlush stdout)
              s <- liftIO getLine
              r <- eval s
              liftIO $ if not (null r) && last r /= '\n'
                  then putStrLn r
                  else putStr r
              mainLoop

main :: IO ()
main = fst <$> runStateT mainLoop []
