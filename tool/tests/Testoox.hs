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
   , symbolicNulls  = True
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
    let label = ooxFile ++ " -- " ++ targetMethod
    in
    TestLabel label
    $ TestCase
    $ do
      putStrLn ("\n>>> " ++ label)
      (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

testOOX_withLargerArray ooxFile targetMethod expectedResult depth =
    let label = ooxFile ++ " -- " ++ targetMethod
    in
    TestLabel label
    $ TestCase
    $ do
      putStrLn ("\n>>> " ++ label)
      (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth, symbolicArraySize=4}
      let (pass, oracleName) = expectedResult vresult
      assertBool oracleName pass

expectValid   ooxResult = (isValid ooxResult, "the target oox-program is VALID")
expectInvalid ooxResult = (isInvalid ooxResult, "the target oox-program is INVALID")
expectDeadlock ooxResult = (isDeadlock ooxResult, "the target oox-program contains DEADLOCK")
-- for testing concurrent program:
testOOX_concur ooxFile targetMethod expectedResult depth =
  let label = ooxFile ++ " -- " ++ targetMethod
  in
  TestLabel label
  $ TestCase
  $ do
    putStrLn ("\n>>> " ++ label)
    (vresult,stat) <- execute $ config0 { fileName = ooxFile , entryPoint = targetMethod, maximumDepth=depth, applyPOR = True}
    let (pass, oracleName) = expectedResult vresult
    assertBool oracleName pass

--
-- Bunch of tests vs simple1.oox
--
simple1_oox = "./examples/simple/simple1.oox"
tsuite_simple1 = ("tsuite_simple1",
   TestList [
     simpletestOOX simple1_oox "SomeClass.m1" expectValid,
     simpletestOOX simple1_oox "SomeClass.m1Invalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.m2" expectValid,
     simpletestOOX simple1_oox "SomeClass.m2Invalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.m3" expectValid,
     -- m3-invalid requires a more elaborate setup:
     TestLabel (simple1_oox ++ " -- " ++ "SomeClass.m3Invalid")
       $ TestCase
       $ do
          (vresult,_) <- execute $ config0 {
                           fileName = simple1_oox ,
                           entryPoint = "SomeClass.m3Invalid",
                           symbolicArraySize = 4,
                           maximumDepth = 100}
          assertBool "the target is invalid" (isInvalid vresult),
     simpletestOOX simple1_oox "SomeClass.m4" expectValid,
     simpletestOOX simple1_oox "SomeClass.m4Invalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.m5" expectValid,
     simpletestOOX simple1_oox "SomeClass.m5Invalid" expectInvalid,
     simpletestOOX simple1_oox "SomeClass.m6" expectValid,
     simpletestOOX simple1_oox "SomeClass.m7" expectValid,
     simpletestOOX simple1_oox "SomeClass.m8" expectValid
     ])

--
-- Bunch of tests vs concursimple1.oox. This contains some simple tests of
-- concurrent programs.
--
concursimpel1_oox = "./examples/simple/concursimple1.oox"
tsuite_concursimple1 = ("tsuite_concursimple1",
   TestList [
     simpletestOOX concursimpel1_oox "Main.incr" expectValid,
     simpletestOOX concursimpel1_oox "Main.incrInvalid" expectInvalid,
     testOOX_concur concursimpel1_oox "Main.m1" expectValid 100,
     testOOX_concur concursimpel1_oox "Main.m1_invalid1" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.m1_invalid2" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.m2" expectValid 200,
     testOOX_concur concursimpel1_oox "Main.m2_invalid1" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.m2_invalid2" expectInvalid 100,
     testOOX_concur concursimpel1_oox "Main.m3" expectValid 200,
     testOOX_concur concursimpel1_oox "Main.m3_invalid1" expectInvalid 300,
     testOOX_concur concursimpel1_oox "Main.m3_invalid2" expectInvalid 200,
     testOOX_concur concursimpel1_oox "Main.m3_invalid3" expectInvalid 200,
     testOOX_concur concursimpel1_oox "Main.m4_invalid" expectInvalid 200,
     testOOX_concur concursimpel1_oox "Main.m5" expectValid 300
   ])

--
-- Some tests on lock construct and deadlock checking
--
tsuite_locks1 = ("tsuite_locks1",
   TestList [
      testOOX_concur "./examples/simple/locks1.oox" "Main.main" expectValid 50,
      testOOX_concur "./examples/simple/locks1.oox" "Main.main_invalid1" expectInvalid 50,
      testOOX_concur "./examples/simple/deadlock.oox" "Main.main" expectDeadlock 50,
      testOOX_concur "./examples/philosophers.oox" "Main.main" expectDeadlock 200
   ])

--
-- Some tests involving arrays
--
tsuite_arrays = ("tsuite_arrays",
   TestList [
      simpletestOOX "./examples/array/array1.oox" "Main.foo" expectValid,
      simpletestOOX "./examples/array/array1.oox" "Main.foo_invalid" expectInvalid,
      simpletestOOX "./examples/array/array1.oox" "Main.max" expectValid,
      simpletestOOX "./examples/array/array1.oox" "Main.max_invalid1" expectInvalid,
      simpletestOOX "./examples/array/array1.oox" "Main.max_invalid2" expectInvalid,
      testOOX_concur "./examples/array/array1.oox" "Main.sort" expectValid 100,
      testOOX_concur "./examples/array/array1.oox" "Main.sort_invalid1" expectInvalid 100,
      simpletestOOX "./examples/array/array2.oox" "Main.foo1" expectValid,
      simpletestOOX "./examples/array/array2.oox" "Main.foo1_invalid" expectInvalid,
      testOOX_concur "./examples/array/array2.oox" "Main.foo2" expectInvalid 100,
      testOOX_concur "./examples/array/array2.oox" "Main.sort" expectInvalid 100
   ])

--
-- Some tests involving exceptions
--
tsuite_exceptions = ("tsuite_exceptions",
  TestList [
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m1" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m1_invalid" expectInvalid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m2" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m3" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m3_invalid1" expectInvalid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.m3_invalid2" expectInvalid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.nullExc1" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.nullExc2" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.arrayExc1" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.arrayExc2" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.arrayExc3" expectValid,
     simpletestOOX "./examples/simple/exceptions.oox" "Main.arrayExc4" expectValid
  ] )

--
-- Some tests involving object strucures
--
tsuite_objstructures = ("tsuite_objstructures",
  TestList [
     -- simpletestOOX "./examples/simple/simplelist.oox" "Main.m1" expectValid
  ])

--
-- Test vs some simple data structures
--
tsuite_datastructures = ("tsuite_datastructures",
  TestList [
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test1" expectInvalid 50,
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test2" expectValid 100,
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test2_invalid" expectInvalid 50,
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test3" expectValid 100,
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test3_invalid1" expectInvalid 80,
       testOOX_concur "./examples/intLinkedlist.oox" "Node.test3_invalid2" expectInvalid 80
  ])
--
-- Test vs some example algorithms
--
tsuite_algorithms = ("tsuite_algorithms",
  TestList [
       testOOX_concur "./examples/bubblesort.oox" "Main.sort" expectValid 1000,
       testOOX_concur "./examples/fib.oox" "Main.main" expectValid 40,
       -- concurrent mergesort requires a more elaborate setup:
       TestLabel ("./examples/mergesort.oox" ++ " -- " ++ "Main.sort")
         $ TestCase
         $ do
            (vresult,_) <- execute $ config0 {
                             fileName = "./examples/mergesort.oox" ,
                             entryPoint = "Main.sort",
                             symbolicArraySize = 5,
                             applyPOR = True,
                             maximumDepth = 200}
            assertBool "the target is valid" (isValid vresult),
       TestLabel ("./examples/mergesortMUTCMP2.oox" ++ " -- " ++ "Main.sort")
         $ TestCase
         $ do
            (vresult,_) <- execute $ config0 {
                             fileName = "./examples/mergesortMUTCMP2.oox" ,
                             entryPoint = "Main.sort",
                             symbolicArraySize = 5,
                             applyPOR = True,
                             maximumDepth = 200}
            assertBool "the target is invalid" (isInvalid vresult)
  ])

tsuitex = ("bla", TestList [
       -- testOOX_concur concursimpel1_oox "Main.mFive" expectValid 100
       --testOOX_withLargerArray "./examples/array.oox" "Main.foo_1" expectValid 100,
       --testOOX_withLargerArray "./examples/array.oox" "Main.sort" expectValid 100,
       --testOOX_withLargerArray "./examples/array.oox" "Main.max" expectValid 100
       testOOX_withLargerArray "./examples/array2.oox" "Main.sort" expectValid 100
       --testOOX_withLargerArray "./examples/array.oox" "Main.max" expectValid 100
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
  runTestSuite tsuite_locks1
  runTestSuite tsuite_arrays
  runTestSuite tsuite_exceptions
  runTestSuite tsuite_datastructures
  runTestSuite tsuite_algorithms
