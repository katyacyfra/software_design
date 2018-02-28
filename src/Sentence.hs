module Sentence where
import Environment (Environment)
import qualified Parse as P (PipelineAtom, CommandAtom(Run, Assign))
import String (interpolate, InterpolationError)
import Control.Monad.Except (Except, throwError)

-- | The final command that is to be run, with all substitutions already
-- performed
data Command = Run -- ^ A plain command
             { command   :: String   -- ^ The command to be run
             , arguments :: [String] -- ^ The arguments of the command
             }
             | Assign -- ^ An assignment
             { key   :: String -- ^ The name of the variable
             , value :: String -- ^ The value of the variable
             }
             deriving (Show)

-- | A group of commands chained into a pipeline
type Pipeline = [Command]

-- | Transform the pre-substitution commands into proper ones
subst :: Environment                       -- ^ The environment with variable
                                           -- values
      -> P.CommandAtom                     -- ^ The pre-substitution command
      -> Except InterpolationError Command -- ^ Either lookup error or the
                                           -- result
subst e (P.Run c a)    = Run <$> interpolate e c <*> mapM (interpolate e) a
subst e (P.Assign k v) = Assign k <$> interpolate e v
