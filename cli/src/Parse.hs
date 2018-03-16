module Parse (CommandAtom(Run, Assign), PipelineAtom, parseLine) where
import Text.Parsec
import String (UString, StringPart (Plain, Var))

-- | Shell command before it and its arguments undergo variable substitution
data CommandAtom = Run -- ^ A plain command
                 { command   :: UString   -- ^ The command to be run
                 , arguments :: [UString] -- ^ The arguments of the command
                 }
                 | Assign -- ^ An assignment
                 { key   :: String  -- ^ The name of the variable
                 , value :: UString -- ^ The value of the variable
                 }
                 deriving (Show)

-- | A group of pre-interpolated commands chained into a pipeline
type PipelineAtom = [CommandAtom]

variableName :: Parsec String u String
variableName = many1 $ alphaNum <|> char '_'

plainString :: Parsec String u String
plainString = many1 $ noneOf $ ifs ++ "$'\"|"

doubleQuoted :: Parsec String u UString
doubleQuoted = many $ Plain <$> many1 (noneOf "$\"") <|>
                      Var <$> (char '$' *> variableName)

singleQuoted :: Parsec String u UString
singleQuoted = (:[]) . Plain <$> many (noneOf "'")

uString :: Parsec String u UString
uString = concat <$> many1 (between (char '"') (char '"') doubleQuoted <|>
                            between (char '\'') (char '\'') singleQuoted <|>
                            (:[]) . Plain <$> plainString <|>
                            (:[]) . Var <$> (char '$' *> variableName))

ifs = " \t\n"

ifsParser = many1 (oneOf ifs)

commandAtom :: Parsec String u CommandAtom
commandAtom = try (Assign <$> (variableName <* char '=') <*> uString) <|>
              Run <$> uString <*>
                ((ifsParser *> many (uString <* optional ifsParser)) <|>
                pure [])

pipeline :: Parsec String u PipelineAtom
pipeline = optional ifsParser *>
           (sepBy1 commandAtom
             (char '|' *> optional ifsParser))
           <* optional ifsParser

-- | Parse a single line of user input
parseLine :: String                         -- ^ The user input
          -> Either ParseError PipelineAtom -- ^ Either an error or the result
parseLine = parse pipeline ""
