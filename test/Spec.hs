

import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))

main :: IO ()
main = defaultMain tests
tests :: TestTree
tests = testGroup "Tests" [tokenizerTest]

tokenizerTest :: TestTree
tokenizerTest = testCase "Tokenizer Test" $
  assertEqual "Should tokenize input into associated tokens" expectedOutput (tokenizer "a1 = 5")
   where
     expectedOutput = [IdentifierToken "a1", EqualToken, IntegerToken 5]