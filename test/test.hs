

import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))

main :: IO ()
main = defaultMain tests
tests :: TestTree --Define the tree here
tests = testGroup "Tests"  --Put all of your testCases here and follow the below syntax to design test cases

  [   testCase "Testing tokenization of assignment expresssion " $
        either assertFailure (@?= [IdentifierToken "a1", EqualToken, IntegerToken 5]) (tokenize "a1 = 5")
  ,
      testCase "Tokenize binary addition operations" $
       either assertFailure (@?= [IntegerToken 5, AddToken, IntegerToken 5]) (tokenize "5 + 5")
  ,
      testCase "Tokenize trinary addition operations" $
       either assertFailure (@?= [IntegerToken 5, AddToken, IntegerToken 5, AddToken, IntegerToken 10]) (tokenize "5 + 5 + 10")
  ]