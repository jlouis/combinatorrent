{- The Test module provides an interface to test-framework. It is called when --tests
 - is supplied on the command line. This file only gathers together various test suites
 - from all over the rest of the code and then executes them via test-framework.
 -}
module Test (runTests) where

import System.Environment ( getArgs )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified Data.PieceSet  (testSuite)
import qualified Data.Queue     (testSuite)
import qualified Protocol.BCode (testSuite)
import qualified Protocol.Wire  (testSuite)


runTests :: IO ()
runTests =
 do args <- filter (/= "--tests") `fmap` getArgs
    flip defaultMainWithArgs args
     [ testSuite
     , Data.Queue.testSuite
     , Data.PieceSet.testSuite
     , Protocol.BCode.testSuite
     , Protocol.Wire.testSuite
     ]

testSuite :: Test
testSuite = testGroup "Test test-framework"
 [ testProperty "reverse-reverse/id" prop_reversereverse ]

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: [Int] -> Bool
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

