import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Tokenizer
import Tokenizer.Token (Token(..))
import Parser.AST
import Parser.Parser (parseExpression, parseType, parseParam, parseStmt, pTraitDef, pAbsMethodDef, pStructDef, pImplDef, pConcMethodDef, pFuncDef)

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
     testCase "Tokenize stmt within braces" $
     either assertFailure(@=? [LBraceToken, LetToken, IdentifierToken "a1", ColonToken, IntToken, EqualToken, IntegerToken 5, RBraceToken]) (tokenize " {let a1:Int = 5}") 
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
parserTests = testGroup "Parser Tests"
  [ testCase "Parse single integer" $
      case tokenize "42" of
        Right tokens -> parseExpression tokens @?= Right (Int 42)
        Left err -> assertFailure err
  , testCase "Parse single variable" $
      case tokenize "x" of
        Right tokens -> parseExpression tokens @?= Right (Identifier "x")
        Left err -> assertFailure err
  , testCase "Parse negation" $
      case tokenize "-5" of
        Right tokens -> parseExpression tokens @?= Right (Negative (Int 5))
        Left err -> assertFailure err
  , testCase "Parse addition" $
      case tokenize "-3 + 4" of
        Right tokens -> parseExpression tokens @?= Right (Add (Negative (Int 3)) (Int 4))
        Left err -> assertFailure err
  , testCase "Parse mixed addition and multiplication" $
      case tokenize "2 + 3 * 4" of
        Right tokens -> parseExpression tokens @?= Right (Add (Int 2) (Multiply (Int 3) (Int 4)))
        Left err -> assertFailure err
  , testCase "Parse parentheses with multiplication" $
      case tokenize "(2 + 3) * 4" of
        Right tokens -> parseExpression tokens @?= Right (Multiply (Add (Int 2) (Int 3)) (Int 4))
        Left err -> assertFailure err
  , testCase "Parse division" $
      case tokenize "10 / 2" of
        Right tokens -> parseExpression tokens @?= Right (Division (Int 10) (Int 2))
        Left err -> assertFailure err
  , testCase "Parse subtraction" $
      case tokenize "7 - 3" of
        Right tokens -> parseExpression tokens @?= Right (Sub (Int 7) (Int 3))
        Left err -> assertFailure err
  , testCase "Parse complex expression with variables" $
      case tokenize "(x + 1) * 2" of
        Right tokens -> parseExpression tokens @?= Right (Multiply (Add (Identifier "x") (Int 1)) (Int 2))
        Left err -> assertFailure err
  , testCase "Parse type Int" $
      case tokenize "Int" of
        Right tokens -> parseType tokens @?= Right IntType
        Left err -> assertFailure err
  , testCase "Parse type Void" $
      case tokenize "Void" of
        Right tokens -> parseType tokens @?= Right VoidType
        Left err -> assertFailure err
  , testCase "Parse type Boolean" $
      case tokenize "Boolean" of
        Right tokens -> parseType tokens @?= Right BooleanType
        Left err -> assertFailure err
  , testCase "Parse type Self" $
      case tokenize "Self" of
        Right tokens -> parseType tokens @?= Right SelfType
        Left err -> assertFailure err
  , testCase "Parse type StructName" $
      case tokenize "car" of
        Right tokens -> parseType tokens @?= Right (StructName "car")
        Left err -> assertFailure err
  , testCase "Parse types inside Parenthesis" $
      case tokenize "(Int)" of
        Right tokens -> parseType tokens @?= Right IntType
        Left err -> assertFailure err
  , testCase "Parse higher order function with only one type inside Parenthesis" $
      case tokenize "(Int , Boolean) => Int" of
        Right tokens -> parseType tokens @?= Right (HigherOrderType (CommaType IntType [BooleanType]) IntType)
        Left err -> assertFailure err
  , testCase "Parse params" $
      case tokenize ("a1: Int") of
        Right tokens -> parseParam tokens @?= Right (Param "a1" (IntType) )
        Left err -> assertFailure err
  , testCase "Parse multiple params" $
      case tokenize ("a1: Int , a2: Boolean") of
        Right tokens -> parseParam tokens @?= Right (CommaParam (Param "a1" IntType) [Param "a2" BooleanType ])
        Left err -> assertFailure err
  , testCase "Parse LetStmt" $
      case tokenize ("let a1: Int = 5;") of
        Right tokens -> parseStmt tokens @?= Right (LetStmt (Param "a1" (IntType)) (Int 5))
  , testCase "Parse AssignStmt" $
      case tokenize ("a1 = 5;") of
        Right tokens -> parseStmt tokens @?= Right (AssgStmt (Identifier "a1") (Int 5))
        Left err -> assertFailure err
  , testCase "Parse BreakStmt" $
      case tokenize ("break;") of
        Right tokens -> parseStmt tokens @?= Right (BreakStmt)
        Left err -> assertFailure err
  , testCase "Parse Block stmts" $
      case tokenize ("{let a1: Int = 5; a1 = 6;}") of
        Right tokens -> parseStmt tokens @?= Right (BlockStmt [LetStmt (Param "a1" IntType) (Int 5), AssgStmt (Identifier "a1") (Int 6) ] ) 
        Left err -> assertFailure err
  , testCase "Parse TraitDef with a single abstract method" $
      case tokenize "trait MyTrait { method doIt(x: Int): Void; }" of
        Right tokens ->
          pTraitDef tokens @?= Right
            (TraitDef
               { traitName = "MyTrait"
               , traitAbsMethodDef =
                   [ AbsMethodDef
                       { abMethName = "doIt"
                       , abMethParameters = Param "x" IntType
                       , abMethReturnType = VoidType
                       }
                   ]
               })
        Left err -> assertFailure err

  , testCase "Parse AbsMethodDef" $
      case tokenize "method doIt(x: Int): Void;" of
        Right tokens ->
          pAbsMethodDef tokens @?= Right
            (AbsMethodDef
               { abMethName = "doIt"
               , abMethParameters = Param "x" IntType
               , abMethReturnType = VoidType
               })
        Left err -> assertFailure err

  , testCase "Parse StructDef with a single field" $
      case tokenize "struct Car { brand: Int }" of
        Right tokens ->
          pStructDef tokens @?= Right
            (StructDef
               { strucName = "Car"
               , strucFields = Param "brand" IntType
               })
        Left err -> assertFailure err

  , testCase "Parse ImplDef with one concrete method" $
      case tokenize "impl MyTrait for Car { method doIt(x: Int): Void { break; } }" of
        Right tokens ->
          pImplDef tokens @?= Right
            (ImplDef
               { implTraitName = "MyTrait"
               , iForType = StructName "Car"
               , iMethods =
                   [ ConcMethodDef
                       { cmName = "doIt"
                       , cmParameters = Param "x" IntType
                       , cmReturnType = VoidType
                       , cmBody = [BreakStmt]
                       }
                   ]
               })
        Left err -> assertFailure err

  , testCase "Parse ConcMethodDef alone" $
      case tokenize "method setVal(x: Int): Void { x = 5; }" of
        Right tokens ->
          pConcMethodDef tokens @?= Right
            (ConcMethodDef
               { cmName = "setVal"
               , cmParameters = Param "x" IntType
               , cmReturnType = VoidType
               , cmBody = [AssgStmt (Identifier "x") (Int 5)]
               })
        Left err -> assertFailure err

  , testCase "Parse FuncDef with a single statement" $
      case tokenize "func main(a: Int): Void { a = 5; }" of
        Right tokens ->
          pFuncDef tokens @?= Right
            (FuncDef
               { funcName = "main"
               , funcParameters = Param "a" IntType
               , funcReturnType = VoidType
               , funcBody = [AssgStmt (Identifier "a") (Int 5)]
               })
        Left err -> assertFailure err

  ]     