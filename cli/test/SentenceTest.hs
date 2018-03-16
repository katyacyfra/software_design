module SentenceTest (sentenceTestList) where
import Sentence
import qualified Parse as P
import String
import Control.Monad.Except
import Test.HUnit
import Data.Either

testSubstWorks = let env = [("1", "a"), ("2", "c"), ("3", "b")]
                 in TestCase $ assertEqual "variables should be substituted"
                     (Right (Run "a" ["a", "b", "c"]))
                     (runExcept (subst env (P.Run [Var "1"]
                          [[Var "1"], [Var "3"], [Var "2"]])))

testWrongSubstDoesnt = TestCase $ assertBool "if a variable isn't there, fail"
                                    (isLeft (runExcept (subst []
                                      (P.Run [Var "1"] []))))

sentenceTestList = TestList [testSubstWorks]
