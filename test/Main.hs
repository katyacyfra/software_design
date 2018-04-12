import ParseTest
import SentenceTest
import CommandTest
import CdLsTest
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [parseTestList, sentenceTestList, commandTestList, cdLsTestList]
