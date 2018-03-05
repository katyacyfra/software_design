module Command.Wc (wc) where

wc' :: String -> String
wc' s = unwords $ map show [length $ lines s, length $ words s, length s]

printWc :: FilePath -> IO String
printWc x = (((x ++ ": ") ++) . wc') <$> readFile x

-- | Display the amount of lines, words and characters in the specified files
-- or, if none are provided, in the input string.
wc :: [FilePath] -- ^ The files to be wc'd
   -> String     -- ^ The input stream used if no files are provided
   -> IO String  -- ^ The effect of outputting a result string
wc [] = pure . wc'
wc fs = const $ fmap unlines $ mapM printWc fs
