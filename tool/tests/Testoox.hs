module Testoox where

import Main (execute)
import Test.HUnit
import System.Exit (exitFailure)
import Data.Configuration
import Execution.Result

{-
   (1) Load this test-file : cabal v2-repl, followed by appropriate :l
   (2) After being loaded in (1) we can run the test-suites in this test-file.

       To run a particular test-suite T: runTestTT T
       To run all test-suites: runallTestSuites
-}

config0 =  Configuration
   { fileName     = ""
   , maximumDepth = 30
   , entryPoint   = ""
   , verifyEnsures = True
   , verifyExceptional = True
   , verifyRequires = True
   , symbolicNulls  = False
   , symbolicAliases  = False
   , symbolicArraySize = 2
   , cacheFormulas = True
   , applyPOR = False
   , applyLocalSolver = True
   , applyRandomInterleaving = False
   , logLevel = 0
   , runBenchmark = False
}

-- for testing non-concurrent program:
simpletestOOX ooxFile targetMethod expectedResult = testOOX ooxFile targetMethod expectedResult 30
testOOX ooxFile targetMethod expectedResult depth =
    TestLabel (ooxFile ++ " -- " ++ targetMethod)
    $ TestCase
    $ do
      (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

testOOX_withLargerArray ooxFile targetMethod expectedResult depth =
    TestLabel (ooxFile ++ " -- " ++ targetMethod)
    $ TestCase
    $ do
      (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth, symbolicArraySize=4}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

expectValid   ooxResult = (isValid ooxResult, "the target oox-program is VALID")
expectInvalid ooxResult = (isInvalid ooxResult, "the target oox-program is INVALID")
expectDeadlock ooxResult = (isDeadlock ooxResult, "the target oox-program contains DEADLOCK")
-- for testing concurrent program:
testOOX_concur ooxFile targetMethod expectedResult depth =
  TestLabel (ooxFile ++ " -- " ++ targetMethod)
  $ TestCase
  $ do
    (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth, applyPOR = True}
    let (pass, oracleName) = expectedResult vresult
    assertBool oracleName pass

--
-- Bunch of tests vs simple1.oox
--
simple1_oox = "./examples/simple/simple1.oox"
tsuite_simple1 = ("tsuite_simple1",
   TestList [
     simpletestOOX simple1_oox "SomeClass.mOne" expectValid,
     simpletestOOX simple1_oox "SomeClass.mOneInvalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.mTwo" expectValid,
     simpletestOOX simple1_oox "SomeClass.mTwoInvalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.mThree" expectValid,
     -- m3-invalid requires a more elaborate setup:
     TestLabel (simple1_oox ++ " -- " ++ "SomeClass.mThreeInvalid")
       $ TestCase
       $ do
          (vresult,_) <- execute $ config0 {
                           fileName = simple1_oox ,
                           entryPoint = "SomeClass.mThreeInvalid",
                           symbolicArraySize = 4,
                           maximumDepth = 100}
          assertBool "the target is invalid" (isInvalid vresult),
     simpletestOOX simple1_oox "SomeClass.mFour" expectValid,
     simpletestOOX simple1_oox "SomeClass.mFourInvalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.mFive" expectValid,
     simpletestOOX simple1_oox "SomeClass.mFiveInvalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.mSix" expectValid,
     simpletestOOX simple1_oox "SomeClass.mSeven" expectValid,
     simpletestOOX simple1_oox "SomeClass.mEight" expectValid
     ])

--
-- Bunch of tests vs concursimple1.oox. This contains some simple tests of
-- concurrent programs.
--
concursimpel1_oox = "./examples/simple/concursimple1.oox"
tsuite_concursimple1 = ("tsuite_concursimple1",
   TestList [
     --simpletestOOX concursimpel1_oox "Main.incr" expectValid,
     --simpletestOOX concursimpel1_oox "Main.incrInvalid" expectInvalid,
     testOOX_concur concursimpel1_oox "Main.mOne" expectValid 100,
     testOOX_concur concursimpel1_oox "Main.mOneInvalidOne" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.mOneInvalidTwo" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.mTwo" expectValid 200,
     testOOX_concur concursimpel1_oox "Main.mTwoInvalidOne" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.mTwoInvalidTwo" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.mThree" expectValid 200,
     testOOX_concur concursimpel1_oox "Main.mThreeInvalidOne" expectInvalid 300,
     testOOX_concur concursimpel1_oox "Main.mThreeInvalidTwo" expectInvalid 200,
     testOOX_concur concursimpel1_oox "Main.mThreeInvalidThree" expectInvalid 200
   ])

--
-- Some tests on lock construct
--
lock1_oox = "./examples/simple/locks1.oox"
tsuite_locks1 = ("tsuite_locks1",
   TestList [
      testOOX_concur lock1_oox "Main.main" expectValid 50,
      testOOX_concur lock1_oox "Main.mainInvalidOne" expectInvalid 50
   ])

tsuite_deadlock = ("tsuite_deadlock",
   TestList [
      testOOX_concur "./examples/simple/deadlock.oox" "Main.main" expectDeadlock 50,
      testOOX_concur "./examples/philosophers.oox" "Main.main" expectDeadlock 200
   ])

tsuitex = ("bla", TestList [
       --testOOX_concur concursimpel1_oox "Main.mOneInvalidTwo" expectInvalid 100,
       --testOOX_concur concursimpel1_oox "Main.mTwoInvalidOne" expectInvalid 100,
       --testOOX_concur concursimpel1_oox "Main.mTwoInvalidTwo" expectInvalid 100,
       --testOOX_concur concursimpel1_oox "Main.mThree" expectValid 150,
       --testOOX_concur concursimpel1_oox "Main.mThreeInvalidOne" expectInvalid 300,
       --testOOX_concur concursimpel1_oox "Main.mThreeInvalidOne" expectInvalid 300,
       --testOOX_concur concursimpel1_oox "Main.mThreeInvalidTwo" expectInvalid 300,
       --testOOX_concur concursimpel1_oox "Main.mThreeInvalidThree" expectInvalid 200,
       --simpletestOOX simple1_oox "SomeClass.mEight" expectValid
       -- testOOX_concur concursimpel1_oox "Main.mFive" expectValid 100
       testOOX_withLargerArray "./examples/array.oox" "Main.foo_1" expectValid 100,
       testOOX_withLargerArray "./examples/array.oox" "Main.sort" expectValid 100,
       testOOX_withLargerArray "./examples/array.oox" "Main.max" expectValid 100
      ])

-- for running a testsiute:
runTestSuite suite = do
   let (suiteName,suite_) = suite
   putStrLn "\n**"
   putStrLn ("** Running test-suite: " ++ suiteName)
   putStrLn "**"
   Counts cases tried errors fails <- runTestTT suite_
   if errors > 0 || fails > 0
     then do
          putStrLn ("** " ++ suiteName ++ ": EXIT due to some failure")
          exitFailure
     else do
          putStrLn ("** " ++ suiteName ++ ": all tests pass.")
          return ()



--for running all test suites as a Main; will exit with a non-zero exit code if
-- some tests fail
main::IO()
main = do
  runTestSuite tsuite_simple1
  runTestSuite tsuite_concursimple1
