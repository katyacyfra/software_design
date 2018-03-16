module Command.Grep (grep) where
import Text.Regex.TDFA
import System.Console.GetOpt
import Control.Exception (catch, ErrorCall(ErrorCall))

data GrepParams = GrepParams
                { ignoreCase :: Bool
                , wholeWords :: Bool
                , linesAfter :: Int
                , regexp     :: String
                }

defaultGrepParams = GrepParams False False 0

grep' :: GrepParams -> (String -> Bool) -> String -> String
grep' gp f s = unlines $ grep'' (lines s) 0
  where grep'' :: [String] -> Int -> [String]
        grep'' [] i = []
        grep'' (x:xs) i | f x = x : grep'' xs (linesAfter gp)
        grep'' (x:xs) 0 = grep'' xs 0
        grep'' (x:xs) i = x : grep'' xs (pred i)

compileRegex :: GrepParams -> IO (String -> Bool)
compileRegex gp = match <$> (makeRegexOptsM
                  defaultCompOpt {caseSensitive = not (ignoreCase gp)}
                  defaultExecOpt
                  (if wholeWords gp
                   then "\\b" ++ regexp gp ++ "\\b"
                   else regexp gp) :: IO Regex)

parseGrepOpts :: [String] -> IO (GrepParams, [FilePath])
parseGrepOpts ar = case errs of
      [] -> case sequenceA params of
          Right par -> case rest of
              (re:fls) -> let nopt = foldl (.) id par
                             in pure (nopt $ defaultGrepParams re, fls)
              [] -> fail $ "No regular expression provided to grep"
          Left err -> fail err
      es -> fail $ concat es
  where optDescs :: [OptDescr (Either String (GrepParams -> GrepParams))]
        optDescs = [ Option "-i" []
                         (NoArg $ Right (\x -> x {ignoreCase = True}))
                         "ignore case"
                   , Option "-w" []
                         (NoArg $ Right (\x -> x {wholeWords = True}))
                         "match only whole words"
                   , Option "-A" []
                         (ReqArg (\s -> case reads s of
                                 [(i, "")] -> Right (\x -> x {linesAfter = i})
                                 _ -> Left "-A requires an integral argument")
                             "LINES")
                         "print LINES lines after a match"
                   ]
        (params, rest, errs) = getOpt Permute optDescs ar

-- | Display the lines that have on them matches specified by a regular
-- expression. The lines are taken from files or, if none are specified,
-- from the standard input.
grep :: [String]  -- ^ The arguments to the function
     -> String    -- ^ Input that is used if no files are specified
     -> IO String -- ^ The effect of outputting the result
grep ar = \s -> do (opts, files) <- parseGrepOpts ar
                   rex <- compileRegex opts
                   if null files
                     then pure $ grep' opts rex s
                     else unlines <$>
                          mapM (\f -> do s <- readFile f
                                         pure $ grep' opts rex s) files
