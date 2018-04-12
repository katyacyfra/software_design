module Command (IOCommand, run) where
import System.Process (readProcessWithExitCode)
import System.Exit (exitSuccess)
import System.Directory (getCurrentDirectory, setCurrentDirectory, getHomeDirectory, listDirectory)

-- | A single entry in the shell pipeline.
type IOCommand = String -> IO String

mkIOCommand :: (String -> IO String) -> IOCommand
mkIOCommand = id

spawn :: FilePath -> [String] -> IOCommand
spawn fp args = mkIOCommand $ fmap (\(_, s, _) -> s)
                            . readProcessWithExitCode fp args

wc :: String -> String
wc s = unwords $ map show [length $ lines s, length $ words s, length s]

printWc :: FilePath -> IO String
printWc x = fmap (((x ++ ": ") ++) . wc) $ readFile x

extraArgumentsError :: String -> IO a
extraArgumentsError s = fail ("Function '" ++ s ++ "' doesn't accept arguments")

-- | Changes current directory
cd :: [String] -> IO () 
cd []  = getHomeDirectory >>= setCurrentDirectory -- remain current directiry as is
cd [d] = setCurrentDirectory d                    -- change current directory to d
cd _   = fail $ "cd accepts one or null args"

-- | Directory listing
ls :: [String] -> IO String
ls []  = getCurrentDirectory >>= (fmap unlines . listDirectory) -- listing of current directory
ls [d] = (fmap unlines . listDirectory) d                       -- listing of d directory
ls _   = fail $ "cd accepts one or null args"

-- | Transform the string representation of the command and its arguments to
-- | the object that describes the semantics of the command.
run :: String    -- ^ The command
    -> [String]  -- ^ The arguments of the command
    -> IOCommand -- ^ The object representing the command's pipeline role

run "cat"  [] = mkIOCommand pure
run "cat"  fs = (mkIOCommand . const . fmap concat . mapM readFile) fs
run "echo" xs = (mkIOCommand . const . pure . (++ "\n") . unwords) xs
run "wc"   [] = mkIOCommand (pure . wc)
run "wc"   fs = (mkIOCommand . const . fmap unlines . mapM printWc) fs
run "pwd"  [] = mkIOCommand (const getCurrentDirectory)
run "pwd"  _  = mkIOCommand (const (extraArgumentsError "pwd"))
run "exit" [] = mkIOCommand (const exitSuccess)
run "exit" _  = mkIOCommand (const (extraArgumentsError "exit"))
run "cd"   ds = mkIOCommand (const (cd ds >>= \_ -> pure ""))
run "ls"   ds = mkIOCommand (const (ls ds))
run fp     xs = spawn fp xs
