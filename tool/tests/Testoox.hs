module Testoox where

import Main
import Test.HUnit
import Data.Configuration
import Execution.Result

config0 =  Configuration
   { fileName     = ""
   , maximumDepth = 30
   , entryPoint   = ""
   , verifyEnsures = False
   , verifyExceptional = False
   , verifyRequires = False
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
      (vresult,stat) <- execute $ config0 { fileName = simple1OOX , entryPoint = targetMethod, maximumDepth=depth}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

expectValid ooxResult = (isValid ooxResult, "valid")
expectInvalid ooxResult = (isInvalid ooxResult, "invalid")

-- Bunch of tests vs simple1.oox
simple1OOX = "./examples/simple/simple1.oox"
test_suite1 = TestList [
               simpletestOOX simple1OOX "SomeClass.mOne" expectValid,
               simpletestOOX simple1OOX "SomeClass.mOneInvalid" expectInvalid,
               simpletestOOX simple1OOX "SomeClass.mTwo" expectValid,
               simpletestOOX simple1OOX "SomeClass.mTwoInvalid" expectInvalid,
               simpletestOOX simple1OOX "SomeClass.mThree" expectValid,
               -- m3-invalid requires a more elaborate setup:
               TestLabel (simple1OOX ++ " -- " ++ "SomeClass.mThreeInvalid")
                 $ TestCase
                 $ do
                   (vresult,_) <- execute $ config0 {
                                       fileName = simple1OOX ,
                                       entryPoint = "SomeClass.mThreeInvalid",
                                       symbolicArraySize = 4,
                                       maximumDepth = 100}
                   assertBool "invalid" (isInvalid vresult),
               simpletestOOX simple1OOX "SomeClass.mFour" expectValid,
               simpletestOOX simple1OOX "SomeClass.mFourInvalid" expectInvalid,
               simpletestOOX simple1OOX "SomeClass.mFive" expectValid,
               simpletestOOX simple1OOX "SomeClass.mFiveInvalid" expectInvalid,
               simpletestOOX simple1OOX "SomeClass.mSix" expectValid,
               simpletestOOX simple1OOX "SomeClass.mSeven" expectValid
               ]
