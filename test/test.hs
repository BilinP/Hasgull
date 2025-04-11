import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))
import Parser.Parser (Expr(..) , parseExpression)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
      [ tokenizerTests
      , parserTests
      ]
tokenizerTests :: TestTree
tokenizerTests = testGroup "Tokenizer Tests"

  [   
     testCase "Testing tokenization of assignment expression" $
      either assertFailure (@=? [IdentifierToken "a1", EqualToken, IntegerToken 5]) (tokenize "a1 = 5")
  ,
     testCase "Tokenize binary addition operations" $
      either assertFailure (@=? [IntegerToken 5, AddToken, IntegerToken 5]) (tokenize "5 + 5")
  ,
     testCase "Tokenize trinary addition operations" $
      either assertFailure (@=? [IntegerToken 5, AddToken, IntegerToken 5, AddToken, IntegerToken 10]) (tokenize "5 + 5 + 10")
  , 
     testCase "Tokenize parentheses and braces" $
      either assertFailure (@=? [LParenToken, IdentifierToken "x", RParenToken, LBraceToken, RBraceToken]) (tokenize "(x) {}")
  , 
     testCase "Tokenize comparison operators" $
      either assertFailure (@=? [IdentifierToken "x", GreaterThanToken, IdentifierToken "y", LessThanToken, IdentifierToken "z"]) (tokenize "x > y < z")
  , 
     testCase "Tokenize keywords" $
      either assertFailure (@=? [IfToken, IdentifierToken "x", ReturnToken, IntegerToken 42]) (tokenize "if x return 42")
  , 
     testCase "Tokenize boolean literals" $
      either assertFailure (@=? [TrueToken, FalseToken]) (tokenize "true false")
  , 
     testCase "Tokenize struct and trait keywords" $
      either assertFailure (@=? [StructToken, IdentifierToken "MyStruct", TraitToken, IdentifierToken "MyTrait"]) (tokenize "struct MyStruct trait MyTrait")
  , 
     testCase "Tokenize multi-character symbols" $
      either assertFailure (@=? [ArrowToken, EqualsToken, NotEqualToken]) (tokenize "=> == !=")
  , 
     testCase "Tokenize miscellaneous symbols" $
      either assertFailure (@=? [CommaToken, ColonToken, SemiColonToken]) (tokenize ", : ;")
  , 
     testCase "Tokenize identifiers and integers" $
      either assertFailure (@=? [IdentifierToken "var1", IntegerToken 123, IdentifierToken "var2", IntegerToken 456]) (tokenize "var1 123 var2 456")
  , 
     testCase "Tokenize with comments" $
      either assertFailure (@=? [LetToken, IdentifierToken "x", EqualToken, IntegerToken 10, SemiColonToken]) (tokenize "let x = 10; // This is a comment")
  ,
     testCase "Tokenize Int, Void, Boolean, Else, While, PrintLn, Self, Method, Break, Impl, New, For" $
      either assertFailure (@=? [IntToken, VoidToken, BooleanToken, ElseToken, WhileToken, PrintLnToken, SelfToken, MethodToken, BreakToken, ImplToken, NewToken, ForToken]) 
      (tokenize "Int Void Boolean else while println Self method break impl new for")
  ,
     testCase "Tokenize Subtract, Multiply, Divide tokens" $
      either assertFailure (@=? [IntegerToken 10, SubtractToken, IntegerToken 5, MultiplyToken, IntegerToken 2, DivideToken, IntegerToken 4]) 
      (tokenize "10 - 5 * 2 / 4")
  ,
    testCase "Tokenize WhileIf" $
     either assertFailure (@=? [IdentifierToken "WhileIf"]) (tokenize "WhileIf")
  ,
     testCase "Unrecognized symbol error message" $
      tokenize "$" @?= Left "Unrecognized symbol near here `: $`"
  ,
     testCase "unrecognized symbol error within a more comprehensive input string" $
       tokenize "let a1 $ 5" @?= Left "Unrecognized symbol near here `: $`"
    
  ,
     testCase "Invalid integer error message" $
      tokenize "123abc" @?= Left "Invalid integer: 123abc"

  , 
   testCase "Testing Token derived Show" $
    case tokenize "5" of
     Right tokens -> show tokens @?= "[IntegerToken 5]"
     Left err -> assertFailure err
  ,
   testCase "Test that Tokens can equal each other" $
    AddToken @?= AddToken
  , 
    testCase "ArrowTokens equal each other" $
    ArrowToken == ArrowToken @?= True
  ,
    testCase "test that different tokens do not equal" $
    False @=? AddToken == SubtractToken 
  , 
    testCase "another test that different tokens do not equal" $
    AddToken /= DivideToken @?= True

  , 
    testCase "List of Tokens can equal each other" $
      [IdentifierToken "hi", AddToken, IntegerToken 5] == [IdentifierToken "hi", AddToken, IntegerToken 5] @?= True

  , 
    testCase "List of Tokens are not equal if mismatched values" $
      [IdentifierToken "hi", AddToken, IntegerToken 5] == [IdentifierToken "hi", SubtractToken, IntegerToken 5] @?= False
   
  ]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ testCase " Parse Single integer " $
      case tokenize "42" of
        Right tokens -> parseExpression tokens @?= Right (Int 42)
        Left err -> assertFailure err
  ]