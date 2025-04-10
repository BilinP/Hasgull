import Test.Tasty
import Test.Tasty.HUnit
import Parser.Parser                  
import Tokenizer.Tokenizer  
import Tokenizer.Token       

 :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testCase "Parse integer literal" $
      parseFromString "5" @?= Right (IntLiteral 5)

  ,
  ]
