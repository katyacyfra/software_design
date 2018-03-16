import ParseTest
import SentenceTest
import CommandTest
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [parseTestList, sentenceTestList, commandTestList]
