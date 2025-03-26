

import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))

main :: IO ()
main = defaultMain tests
tests :: TestTree --Define the tree here
tests = testGroup "Tests"  --Put all of your testCases here and follow the below syntax to design test cases

  [   testCase "Testing tokenization of assignment expresssion " $
       case tokenize "a1 = 5" of
        Right tokens -> tokens @?= [IdentifierToken "a1", EqualToken, IntegerToken 5]
        Left err -> assertFailure err
     , testCase "Tokenize binary addition operations" $
       case tokenize "5 + 5" of
        Right tokens -> tokens @?= [IntegerToken 5, AddToken, IntegerToken 5]
        Left err -> assertFailure err
     ,  testCase "Tokenize trinary addition operations" $
        case tokenize "5 + 5 + 5" of 
         Right tokens -> tokens @?= [IntegerToken 5, AddToken, IntegerToken 5, AddToken, IntegerToken 5]
         Left err -> assertFailure err
  ]