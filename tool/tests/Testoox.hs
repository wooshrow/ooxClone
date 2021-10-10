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

simpletestOOX ooxFile targetMethod expectedResult = testOOX ooxFile targetMethod expectedResult 30
testOOX ooxFile targetMethod expectedResult depth =
    TestLabel (ooxFile ++ " -- " ++ targetMethod)
    $ TestCase
    $ do
      (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

expectValid   ooxResult = (isValid ooxResult, "the target oox-program is VALID")
expectInvalid ooxResult = (isInvalid ooxResult, "the target oox-program is INVALID")

--
-- Bunch of tests vs simple1.oox
--
simple1_oox = "./examples/simple/simple1.oox"
tsuite_simple1 = TestList [
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
   simpletestOOX simple1_oox "SomeClass.mSeven" expectValid
   ]

--
-- Bunch of tests vs simple2.oox. This contains some simple tests of concurrent programs.
--
concursimpel1_oox = "./examples/simple/concursimple1.oox"
testOOX_concur ooxFile targetMethod expectedResult depth =
  TestLabel (ooxFile ++ " -- " ++ targetMethod)
  $ TestCase
  $ do
    (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth, applyPOR = True}
    let (pass, oracleName) = expectedResult vresult
    assertBool oracleName pass

tsuite_concursimple1 = TestList [
   --simpletestOOX concursimpel1_oox "Main.incr" expectValid,
   --simpletestOOX concursimpel1_oox "Main.incrInvalid" expectInvalid,
   testOOX_concur concursimpel1_oox "Main.mOne" expectValid 100,
   testOOX_concur concursimpel1_oox "Main.mOneInvalidOne" expectInvalid 100,
   testOOX_concur concursimpel1_oox "Main.mOneInvalidTwo" expectInvalid 100,
   testOOX_concur concursimpel1_oox "Main.mTwo" expectValid 200,
   testOOX_concur concursimpel1_oox "Main.mTwoInvalidOne" expectInvalid 100,
   testOOX_concur concursimpel1_oox "Main.mTwoInvalidTwo" expectInvalid 100,
   testOOX_concur concursimpel1_oox "Main.mThree" expectValid 200,
   testOOX_concur concursimpel1_oox "Main.mThreeInvalidOne" expectInvalid 200,
   testOOX_concur concursimpel1_oox "Main.mThreeInvalidTwo" expectInvalid 200
   ]

tsuitex = TestList [
       testOOX_concur concursimpel1_oox "Main.mThree" expectValid 150
       -- testOOX_concur concursimpel1_oox "Main.mOneInvalidTwo" expectInvalid 100
       --testOOX_concur concursimpel1_oox "Main.mThreeInvalidOne" expectInvalid 300
       -- testOOX_concur concursimpel1_oox "Main.mThreeInvalidTwo" expectInvalid 300
      ]

-- invoke this to run all test suites:
runallTestSuites :: IO Counts
runallTestSuites = runTestTT . addAll $ [
      tsuite_simple1
      , tsuite_concursimple1
   ]
   where
   addAll [] = TestList[]
   addAll (TestList t1 : therest) = TestList (t1 ++ t2)
      where
      TestList t2 = addAll therest

--for running all test suites as a Main; will exit with a non-zero exit code if
-- some tests fail
main::IO()
main = do
  Counts cases tried errors fails <- runallTestSuites
  if errors > 0 || fails > 0 then exitFailure else return ()
