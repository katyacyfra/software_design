module CdLsTest (cdLsTestList) where
import Test.HUnit
import Command
import System.Directory (getCurrentDirectory, setCurrentDirectory, getHomeDirectory, listDirectory)
import Data.List

testCdHome = TestCase $ do
    oldDir <- run "pwd" [] ""
    run "cd" [] ""
    currentDir <- run "pwd" [] ""
    homeDir <- getHomeDirectory
    assertEqual "Is it home directory?" currentDir homeDir
    run "cd" [oldDir] ""
    currentDir <- run "pwd" [] ""
    assertEqual "Changed back" currentDir oldDir

testCdAndLs = TestCase $ do
    old <- run "pwd" [] ""
    run "cd" ["/"] ""
    currentDir <- run "pwd" [] ""
    assertEqual "Changed directory to root" currentDir "/"
    files <- run "ls" [] ""
    assertBool "/bin is in /" ("bin" `elem` lines files)
    run "cd" [old] ""
    currentDir <- run "pwd" [] ""
    assertEqual "Changed back" currentDir old
    nfiles <- run "ls" ["/"] ""
    assertEqual "List / from other dir" files nfiles

cdLsTestList = TestList [testCdHome, testCdAndLs]