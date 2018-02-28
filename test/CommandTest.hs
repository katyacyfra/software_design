module CommandTest (commandTestList) where
import Test.HUnit
import Command

testCat s = TestCase $ do ss <- run "cat" [] s
                          assertEqual "cat returns what it receives" s ss
testEcho args = TestCase $ do s <- run "echo" args "gibberish"
                              assertEqual "echo prints its args and a newline"
                                (init s) (unwords args)

commandTestList = TestList [testCat ['1'..'9'], testEcho ["a", "b", "c"]]
