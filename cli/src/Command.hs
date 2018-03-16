module Command (IOCommand, run) where
import System.Process (readProcessWithExitCode)
import System.Exit (exitSuccess)
import System.Directory (getCurrentDirectory)

import Command.Grep (grep)
import Command.Wc (wc)

-- | A single entry in the shell pipeline.
type IOCommand = String -> IO String

mkIOCommand :: (String -> IO String) -> IOCommand
mkIOCommand = id

spawn :: FilePath -> [String] -> IOCommand
spawn fp args = mkIOCommand $ fmap (\(_, s, _) -> s)
                            . readProcessWithExitCode fp args

extraArgumentsError :: String -> IO a
extraArgumentsError s = fail ("Function '" ++ s ++ "' doesn't accept arguments")

-- | Transform the string representation of the command and its arguments to
-- | the object that describes the semantics of the command.
run :: String    -- ^ The command
    -> [String]  -- ^ The arguments of the command
    -> IOCommand -- ^ The object representing the command's pipeline role
run "cat"  [] = mkIOCommand pure
run "cat"  fs = (mkIOCommand . const . fmap concat . mapM readFile) fs
run "echo" xs = (mkIOCommand . const . pure . (++ "\n") . unwords) xs
run "wc"   fs = mkIOCommand $ wc fs
run "grep" fs = mkIOCommand $ grep fs
run "pwd"  [] = mkIOCommand (const getCurrentDirectory)
run "pwd"  _  = mkIOCommand (const (extraArgumentsError "pwd"))
run "exit" [] = mkIOCommand (const exitSuccess)
run "exit" _  = mkIOCommand (const (extraArgumentsError "exit"))
run fp     xs = spawn fp xs
