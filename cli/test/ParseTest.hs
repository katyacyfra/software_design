module ParseTest (parseTestList) where
import Test.HUnit
import Parse
import Data.Either (isLeft, isRight)
import Text.Parsec

testCommand = TestCase $ assertBool "a single command is correct"
                           (isRight (parseLine "echo"))

testCommandWithArgs = TestCase $ assertBool "a command with args is correct"
                           (isRight (parseLine "echo a b c"))

testPipes = TestCase $ assertBool "a pipeline is correct"
                           (isRight (parseLine "echo a b c | wc"))

parseTestList = TestList [testCommand, testCommandWithArgs, testPipes]
