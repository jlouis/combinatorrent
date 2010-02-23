module Test (runTests) where

import System.Environment ( getArgs )
import Test.Framework
import Test.Framework.Providers.QuickCheck


runTests :: IO ()
runTests =
 do args <- filter (/= "--tests") `fmap` getArgs
    flip defaultMainWithArgs args
     [ testSuite ]

testSuite = testGroup "Test test-framework"
 [ testProperty "reverse-reverse/id" prop_reversereverse ]

-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

