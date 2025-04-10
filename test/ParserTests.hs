import Test.Tasty
import Test.Tasty.HUnit
import Parser.Parser (parseFromString)                  
import Tokenizer.Tokenizer  
import Tokenizer.Token       

 :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testCase "Parse single integer token as expression" $
      parseFromString "45" @?= Right (ProgramStmt (ExpStmt (PrimaryExp (IntegerToken 42) SemiColonToken)))

  ]
