module String (StringPart(Plain, Var),
               UString,
               interpolate,
               InterpolationError) where
import Environment (Environment)
import Control.Monad.Except (Except, throwError)

-- | A component of which a single string token consists
data StringPart = Plain String -- ^ Plain string
                | Var String   -- ^ A variable
                  deriving (Show)

-- | A single string token that hasn't yet been interpolated
type UString = [StringPart]

-- | An error indicating that a variable couldn't be found in the environment
newtype InterpolationError = UnknownToken String

instance Show InterpolationError where
    show (UnknownToken s) = "Unset variable '$" ++ s ++ "\""

-- | Interpolate a string token into a string with all the variables substituted
interpolate :: Environment                      -- ^ Lookup table for variables
            -> UString                          -- ^ The string token
            -> Except InterpolationError String -- ^ Either an interpolation
                                                -- error or the result
interpolate env pts = fmap concat (traverse (parse env) pts)
  where parse :: Environment -> StringPart -> Except InterpolationError String
        parse env (Plain str) = pure str
        parse env (Var ident) | Just str <- ident `lookup` env = pure str
        parse env (Var ident) = throwError (UnknownToken ident)
