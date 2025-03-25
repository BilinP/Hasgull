

import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))

main :: IO ()
main = defaultMain tests
tests :: TestTree --Define the tree here
tests = testGroup "Tests"  --Put all of your testCases here and follow the below syntax to design test cases

  [   testCase "Testing tokenization of assignment expresssion " $
       (tokenizer "a1 = 5") @?= [IdentifierToken "a1", EqualToken, IntegerToken 5]
  ,
      testCase "Tokenize binary addition operations" $
       (tokenizer "5 + 5")  @?= [IntegerToken 5, AddToken, IntegerToken 5]
  ,
      testCase "Tokenize trinary addition operations" $
       (tokenizer "5 + 5 + 10") @?=  [IntegerToken 5, AddToken, IntegerToken 5, AddToken, IntegerToken 10]
  ]