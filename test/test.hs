import Data.List (isSuffixOf)
import Generation.Generation (createOutputFile, generateJS, translateExpr, translateParam, translateStmt, translateType)
import Parser.AST
import Parser.AST (Program (progItems), Stmt (ExprStmt), StructActualParam (StructActualParam), Type (StructName))
import Parser.Parser (pAbsMethodDef, pConcMethodDef, pFuncDef, pImplDef, pProgram, pProgramItem, pStructDef, pTraitDef, parseExpression, parseParam, parseStmt, parseType)
import System.IO (readFile)
import Test.Tasty
import Test.Tasty.HUnit
import Tokenizer.Token (Token (..))
import Tokenizer.Tokenizer

main :: IO ()
main = defaultMain generatorTests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ tokenizerTests
    , parserTests
    ]

tokenizerTests :: TestTree
tokenizerTests =
  testGroup
    "Tokenizer Tests"
    [ testCase "Testing tokenization of assignment expression" $
        either assertFailure (@=? [IdentifierToken "a1", EqualToken, IntegerToken 5]) (tokenize "a1 = 5")
    , testCase "Tokenize binary addition operations" $
        either assertFailure (@=? [IntegerToken 5, AddToken, IntegerToken 5]) (tokenize "5 + 5")
    , testCase "Tokenize trinary addition operations" $
        either assertFailure (@=? [IntegerToken 5, AddToken, IntegerToken 5, AddToken, IntegerToken 10]) (tokenize "5 + 5 + 10")
    , testCase "Tokenize parentheses and braces" $
        either assertFailure (@=? [LParenToken, IdentifierToken "x", RParenToken, LBraceToken, RBraceToken]) (tokenize "(x) {}")
    , testCase "Tokenize stmt within braces" $
        either assertFailure (@=? [LBraceToken, LetToken, IdentifierToken "a1", ColonToken, IntToken, EqualToken, IntegerToken 5, RBraceToken]) (tokenize " {let a1:Int = 5}")
    , testCase "Tokenize comparison operators" $
        either assertFailure (@=? [IdentifierToken "x", GreaterThanToken, IdentifierToken "y", LessThanToken, IdentifierToken "z"]) (tokenize "x > y < z")
    , testCase "Tokenize keywords" $
        either assertFailure (@=? [IfToken, IdentifierToken "x", ReturnToken, IntegerToken 42]) (tokenize "if x return 42")
    , testCase "Tokenize boolean literals" $
        either assertFailure (@=? [TrueToken, FalseToken]) (tokenize "true false")
    , testCase "tokenize self.value" $
        either assertFailure (@?= [LowerCaseSelfToken, DotToken, IdentifierToken "value"]) (tokenize "self.value")
    , testCase "Tokenize struct and trait keywords" $
        either assertFailure (@=? [StructToken, IdentifierToken "MyStruct", TraitToken, IdentifierToken "MyTrait"]) (tokenize "struct MyStruct trait MyTrait")
    , testCase "Tokenize multi-character symbols" $
        either assertFailure (@=? [ArrowToken, EqualsToken, NotEqualToken]) (tokenize "=> == !=")
    , testCase "Tokenize miscellaneous symbols" $
        either assertFailure (@=? [CommaToken, ColonToken, SemiColonToken]) (tokenize ", : ;")
    , testCase "Tokenize identifiers and integers" $
        either assertFailure (@=? [IdentifierToken "var1", IntegerToken 123, IdentifierToken "var2", IntegerToken 456]) (tokenize "var1 123 var2 456")
    , testCase "Tokenize with comments" $
        either assertFailure (@=? [LetToken, IdentifierToken "x", EqualToken, IntegerToken 10, SemiColonToken]) (tokenize "let x = 10; // This is a comment")
    , testCase "Tokenize Int, Void, Boolean, Else, While, PrintLn, Self, Method, Break, Impl, New, For" $
        either
          assertFailure
          (@=? [IntToken, VoidToken, BooleanToken, ElseToken, WhileToken, PrintLnToken, SelfToken, MethodToken, BreakToken, ImplToken, NewToken, ForToken])
          (tokenize "Int Void Boolean else while println Self method break impl new for")
    , testCase "Tokenize Subtract, Multiply, Divide tokens" $
        either
          assertFailure
          (@=? [IntegerToken 10, SubtractToken, IntegerToken 5, MultiplyToken, IntegerToken 2, DivideToken, IntegerToken 4])
          (tokenize "10 - 5 * 2 / 4")
    , testCase "Tokenize WhileIf" $
        either assertFailure (@=? [IdentifierToken "WhileIf"]) (tokenize "WhileIf")
    , testCase "Unrecognized symbol error message" $
        tokenize "$" @?= Left "Unrecognized symbol near here `: $`"
    , testCase "unrecognized symbol error within a more comprehensive input string" $
        tokenize "let a1 $ 5" @?= Left "Unrecognized symbol near here `: $`"
    , testCase "Invalid integer error message" $
        tokenize "123abc" @?= Left "Invalid integer: 123abc"
    , testCase "Testing Token derived Show" $
        case tokenize "5" of
          Right tokens -> show tokens @?= "[IntegerToken 5]"
          Left err -> assertFailure err
    , testCase "Test that Tokens can equal each other" $
        AddToken @?= AddToken
    , testCase "ArrowTokens equal each other" $
        ArrowToken == ArrowToken @?= True
    , testCase "test that different tokens do not equal" $
        False @=? AddToken == SubtractToken
    , testCase "another test that different tokens do not equal" $
        AddToken /= DivideToken @?= True
    , testCase "List of Tokens can equal each other" $
        [IdentifierToken "hi", AddToken, IntegerToken 5] == [IdentifierToken "hi", AddToken, IntegerToken 5] @?= True
    , testCase "List of Tokens are not equal if mismatched values" $
        [IdentifierToken "hi", AddToken, IntegerToken 5] == [IdentifierToken "hi", SubtractToken, IntegerToken 5] @?= False
    ]

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
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
    , testCase "parse new struct delcaration" $
        case tokenize "new car {x: 7}" of
          Right tokens -> parseExpression tokens @?= Right (NewStruct (StructName "car") [StructActualParam "x" (Int 7)])
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
    , testCase "parse self expression" $
        case tokenize "self" of
          Right tokens -> parseExpression tokens @?= Right (LowerSelf)
          Left err -> assertFailure err
    , testCase "parse dot expressions" $
        case tokenize "self.value" of
          Right tokens -> parseExpression tokens @?= Right (DotExpr LowerSelf (Identifier "value"))
          Left err -> assertFailure err
    , testCase "parse adding dot exprsessions" $
        case tokenize "self.value + self.other" of
          Right tokens -> parseExpression tokens @?= Right (Add (DotExpr LowerSelf (Identifier "value")) (DotExpr LowerSelf (Identifier "other")))
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
          Right tokens -> parseType tokens @?= Right (HigherOrderType ([IntType, BooleanType]) IntType)
          Left err -> assertFailure err
    , testCase "Parse higher order function with only one argument" $
        case tokenize ("(Int) => Int") of
          Right tokens -> parseType tokens @?= Right (HigherOrderType ([IntType]) IntType)
          Left err -> assertFailure err
    , testCase "Parse params" $
        case tokenize ("a1: Int") of
          Right tokens -> parseParam tokens @?= Right (Param "a1" (IntType))
          Left err -> assertFailure err
    , testCase "dot Stmt" $
        case tokenize ("self.value;") of
          Right tokens -> parseStmt tokens @?= Right (ExprStmt (DotExpr LowerSelf (Identifier "value")))
          Left err -> assertFailure err
    , testCase "dot sttm paren" $
       case tokenize ("println(x.value);") of
          Right tokens -> parseStmt tokens @?= Right  (PrintLnStmt (DotExpr (Identifier "x") (Identifier "value") ))
          Left err -> assertFailure err
    , testCase "Parse LetStmt" $
        case tokenize ("let a1: Int = 5;") of
          Right tokens -> parseStmt tokens @?= Right (LetStmt (Param "a1" (IntType)) (Int 5))
    , testCase "Parse AssignStmt" $
        case tokenize ("a1 = 5;") of
          Right tokens -> parseStmt tokens @?= Right (AssgStmt (Identifier "a1") (Int 5))
          Left err -> assertFailure err
    , testCase "Parse WhileStmt" $
        case tokenize ("while(x < 5) {  x = x+1; }") of
          Right tokens -> parseStmt tokens @?= Right (WhileStmt (LessThan (Identifier "x") (Int 5)) (BlockStmt [AssgStmt (Identifier "x") (Add (Identifier "x") (Int 1))]))
          Left err -> assertFailure err
    , testCase "Parse ForStmt" $
        case tokenize ("for(i = 0; i < 10; i = i + 1) { println(i); }") of
          Right tokens ->
            parseStmt tokens
              @?= Right
                ( ForStmt
                    (AssgStmt (Identifier "i") (Int 0))
                    (LessThan (Identifier "i") (Int 10))
                    (AssgStmt (Identifier "i") (Add (Identifier "i") (Int 1)))
                    (BlockStmt [PrintLnStmt (Identifier "i")])
                )
          Left err -> assertFailure err
    , testCase "Parse IfStmt" $
        case tokenize ("if (x <5) x = x * 2 ;") of
          Right tokens -> parseStmt tokens @?= Right (IfStmt (LessThan (Identifier "x") (Int 5)) (AssgStmt (Identifier "x") (Multiply (Identifier "x") (Int 2))) Nothing)
          Left err -> assertFailure err
    , testCase "Parse IfStmt with a Else" $
        case tokenize ("if (x < 5) x = x * 2 ; else x = x + 2;") of
          Right tokens -> parseStmt tokens @?= Right (IfStmt (LessThan (Identifier "x") (Int 5)) (AssgStmt (Identifier "x") (Multiply (Identifier "x") (Int 2))) (Just (AssgStmt (Identifier "x") (Add (Identifier "x") (Int 2)))))
          Left err -> assertFailure err
    , testCase "Parse BreakStmt" $
        case tokenize ("break;") of
          Right tokens -> parseStmt tokens @?= Right (BreakStmt)
          Left err -> assertFailure err
    , testCase "Parse PrintLnStmt" $
        case tokenize ("println(x);") of
          Right tokens -> parseStmt tokens @?= Right (PrintLnStmt (Identifier "x"))
          Left err -> assertFailure err
    , testCase "Parse ReturnStmt" $
        case tokenize ("return x;") of
          Right tokens -> parseStmt tokens @?= Right (ReturnStmt (Just (Identifier "x")))
          Left err -> assertFailure err
    , testCase "Parse ReturnStmt with zero expressions" $
        case tokenize ("return ;") of
          Right tokens -> parseStmt tokens @?= Right (ReturnStmt Nothing)
          Left err -> assertFailure err
    , testCase "Parse Block stmts" $
        case tokenize ("{let a1: Int = 5; a1 = 6;}") of
          Right tokens -> parseStmt tokens @?= Right (BlockStmt [LetStmt (Param "a1" IntType) (Int 5), AssgStmt (Identifier "a1") (Int 6)])
          Left err -> assertFailure err
    , testCase "Parse TraitDef with a single abstract method" $
        case tokenize "trait MyTrait { method doIt(x: Int): Void; }" of
          Right tokens ->
            pTraitDef tokens
              @?= Right
                ( TraitDef
                    { traitName = "MyTrait"
                    , traitAbsMethodDef =
                        [ AbsMethodDef
                            { abMethName = "doIt"
                            , abMethParameters = [Param "x" IntType]
                            , abMethReturnType = VoidType
                            }
                        ]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse AbsMethodDef" $
        case tokenize "method doIt(x: Int): Void;" of
          Right tokens ->
            pAbsMethodDef tokens
              @?= Right
                ( AbsMethodDef
                    { abMethName = "doIt"
                    , abMethParameters = [Param "x" IntType]
                    , abMethReturnType = VoidType
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse StructDef with a single field" $
        case tokenize "struct Car { brand: Int }" of
          Right tokens ->
            pStructDef tokens
              @?= Right
                ( StructDef
                    { strucName = "Car"
                    , strucFields = [Param "brand" IntType]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse ImplDef with one concrete method" $
        case tokenize "impl MyTrait for Car { method doIt(x: Int): Void { break; } }" of
          Right tokens ->
            pImplDef tokens
              @?= Right
                ( ImplDef
                    { implTraitName = "MyTrait"
                    , iForType = StructName "Car"
                    , iMethods =
                        [ ConcMethodDef
                            { cmName = "doIt"
                            , cmParameters = [Param "x" IntType]
                            , cmReturnType = VoidType
                            , cmBody = [BreakStmt]
                            }
                        ]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse ConcMethodDef alone" $
        case tokenize "method setVal(x: Int): Void { x = 5; }" of
          Right tokens ->
            pConcMethodDef tokens
              @?= Right
                ( ConcMethodDef
                    { cmName = "setVal"
                    , cmParameters = [Param "x" IntType]
                    , cmReturnType = VoidType
                    , cmBody = [AssgStmt (Identifier "x") (Int 5)]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse FuncDef with a single statement" $
        case tokenize "func main(a: Int): Void { a = 5; }" of
          Right tokens ->
            pFuncDef tokens
              @?= Right
                ( FuncDef
                    { funcName = "main"
                    , funcParameters = [Param "a" IntType]
                    , funcReturnType = VoidType
                    , funcBody = [AssgStmt (Identifier "a") (Int 5)]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse ProgramItem with StructDef" $
        case tokenize "struct Person { name: Int }" of
          Right tokens ->
            pProgramItem tokens
              @?= Right
                ( PI_Struct
                    ( StructDef
                        { strucName = "Person"
                        , strucFields = [Param "name" IntType]
                        }
                    )
                )
          Left err ->
            assertFailure err
    , testCase "Parse Program with one struct and one statement" $
        case tokenize "struct Person { age: Int } let age: Int = 21;" of
          Right tokens ->
            pProgram tokens
              @?= Right
                ( Program
                    { progItems =
                        [ PI_Struct
                            ( StructDef
                                { strucName = "Person"
                                , strucFields = [Param "age" IntType]
                                }
                            )
                        ]
                    , progStmts =
                        [LetStmt (Param "age" IntType) (Int 21)]
                    }
                )
          Left err ->
            assertFailure err
    , testCase "Parse Program with multiple items and multiple statements" $
        case tokenize
          "trait MyTrait { method doIt(x: Int): Void; } \
          \struct Car { brand: Int } \
          \let a: Int = 42; \
          \a = a + 1;" of
          Right tokens ->
            pProgram tokens
              @?= Right
                ( Program
                    { progItems =
                        [ PI_Trait
                            ( TraitDef
                                { traitName = "MyTrait"
                                , traitAbsMethodDef =
                                    [ AbsMethodDef
                                        { abMethName = "doIt"
                                        , abMethParameters = [Param "x" IntType]
                                        , abMethReturnType = VoidType
                                        }
                                    ]
                                }
                            )
                        , PI_Struct
                            ( StructDef
                                { strucName = "Car"
                                , strucFields = [Param "brand" IntType]
                                }
                            )
                        ]
                    , progStmts =
                        [ LetStmt (Param "a" IntType) (Int 42)
                        , AssgStmt (Identifier "a") (Add (Identifier "a") (Int 1))
                        ]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse Program with all 4 different Program Items and Two Statements" $
        case tokenize
          "trait MyTrait { method doIt(x: Int): Void; } \
          \struct Car { brand: Int } \
          \impl MyTrait for Car { method doIt(x: Int): Void { break; } } \
          \func main(a: Int): Void { return; } \
          \let a: Int = 42; \
          \a = a + 1;" of
          Right tokens ->
            pProgram tokens
              @?= Right
                ( Program
                    { progItems =
                        [ PI_Trait
                            ( TraitDef
                                { traitName = "MyTrait"
                                , traitAbsMethodDef =
                                    [ AbsMethodDef
                                        { abMethName = "doIt"
                                        , abMethParameters = [Param "x" IntType]
                                        , abMethReturnType = VoidType
                                        }
                                    ]
                                }
                            )
                        , PI_Struct
                            ( StructDef
                                { strucName = "Car"
                                , strucFields = [Param "brand" IntType]
                                }
                            )
                        , PI_Impl
                            ( ImplDef
                                { implTraitName = "MyTrait"
                                , iForType = StructName "Car"
                                , iMethods =
                                    [ ConcMethodDef
                                        { cmName = "doIt"
                                        , cmParameters = [Param "x" IntType]
                                        , cmReturnType = VoidType
                                        , cmBody = [BreakStmt]
                                        }
                                    ]
                                }
                            )
                        , PI_Func
                            ( FuncDef
                                { funcName = "main"
                                , funcParameters = [Param "a" IntType]
                                , funcReturnType = VoidType
                                , funcBody = [ReturnStmt Nothing]
                                }
                            )
                        ]
                    , progStmts =
                        [ LetStmt (Param "a" IntType) (Int 42)
                        , AssgStmt (Identifier "a") (Add (Identifier "a") (Int 1))
                        ]
                    }
                )
          Left err -> assertFailure err
    , testCase "Parse dot expression plus call" $
        case tokenize ("obj.method2(a,b)") of
          Right tokens -> parseExpression tokens @?= Right (DotExpr (Identifier "obj") (Call  (Identifier "method2") [Identifier "a", Identifier "b"]))
          Left err -> assertFailure err
    ]

-- Helper to run the full pipeline: tokenize, parse, then generate JS.
runGenTest :: String -> String -> Assertion
runGenTest code expected =
  case tokenize code of
    Right tokens ->
      case pProgram tokens of
        Right prog -> generateJS prog @?= expected
        Left err -> assertFailure (show err)
    Left err -> assertFailure (show err)

runFileOutput :: String -> String -> Assertion
runFileOutput input expect =
  case tokenize input of
    Right tokens ->
      case pProgram tokens of
        Right prog -> do
          content <- createOutputFile prog "test"
          content @?= expect
        Left err -> assertFailure (show err)
    Left err -> assertFailure (show err)

-- Test function to read from file (say .gull because hasgull) and then compile to javascript
-- successfull pass should create a file(test.js) of javascript code
testreadFile :: String -> String -> String -> Assertion
testreadFile inputFile outputName expecting = do
  -- Get the file we want to read from
  if ".gull" `isSuffixOf` inputFile
    then do
      validFile <- readFile inputFile
      let sample = lines validFile
      let extracted = concatMap (++ "") sample
      case tokenize extracted of
        Right tokens ->
          case pProgram tokens of
            Right prog -> do
              content <- createOutputFile prog outputName
              content @?= expecting
            Left err -> assertFailure (show err)
        Left err -> assertFailure (show err)
    else "ILLEGAL FILE" @?= expecting

generatorTests :: TestTree
generatorTests = testGroup "Generator Tests"
  [ testCase "Translate BreakStmt" $
      runGenTest "break;" "break;"
  , testCase "Translate Int Expr" $
      runGenTest "let x: Int = 5;" "let x = 5;"
  , testCase "translate higher order type" $
      runGenTest "Int" ""
  , testCase "Translate Add with two Ints" $
      runGenTest "let x: Int = 7+7;" "let x = 7+7;"
  , testCase "Translate Add that is one add + one int ie 3+3+bill" $
      runGenTest "let x: Int = 3+3+bill;" "let x = 3+3+bill;"
  , testCase "Translate a Dot Expression into a string" $
      runGenTest "let x: Int = self.value;" "let x = self.value;"
  , testCase "Translate Multiply (x+2)*2" $
      runGenTest "let x: Int = (x+2)*2;" "let x = (x+2)*2;"
  , testCase "Translate a CallExp" $
      runGenTest "let x: Int = obj.add(a,b);" "let x = obj.add(a,b);"
  , testCase "calldot stmt" $
      runGenTest "print();" "print();"
  , testCase "Translate let a1: Int = 5;" $
      runGenTest "let a1: Int = 5;" "let a1 = 5;"
  , testCase "Translate blockstmt {let a: Int = 10; a=x+5;}" $
      runGenTest "{let a: Int = 10; a = a+5;}" "{ let a = 10; \n a=a+5; } "
  , testCase "while stmt translation" $
      runGenTest "while(x<5){x=x+1;}" "while(x<5) { x=x+1; } "
  , testCase "if stmt translation" $
      runGenTest "if(x<5) x=x*2;" "if( x<5) x=x*2; "
  , testCase "if else stmt translation" $
      runGenTest "if(x<5) x=x*2; else x=x+2;" "if( x<5) x=x*2;  else x=x+2; "
  , testCase "println" $
      runGenTest "println(x);" "console.log(x);"
  , testCase "new struct instance" $
      runGenTest "let x: car = new car {x: 2, y: 3};" "let x = car(2,3);"
  , testCase "Translate function definition" $
      runGenTest "func bob(a: Int, x:Int): Void {a=5;}" "function bob(a,x){a=5; }"
  , testCase "define a Struct" $
        runGenTest
          "struct IntWrapper { value: Int}"
          "class IntWrapper {\n  constructor(value) {\n    this.value = value;\n }\n }\n\n"
    , testCase "Translate a trait impl" $
        -- runGenTest = tokenize ⟶ parse ⟶ generateJS
        runGenTest
          -- Gull input: a trait, a struct, and an impl
          "trait Inc { method inc(): Int; }; \
          \struct Counter { n: Int }; \
          \impl Inc for Counter { \
          \  method inc(): Int { return self.n + 1; } \
          \};"
          -- expected JS: nothing for the trait, a class + a prototype method
          ( "class Counter { constructor(n) { this.n = n; } }\n"
              ++ "Counter.prototype.inc = function() {\n"
              ++ "  return this.n + 1;\n"
              ++ "};\n\n"
          )
  , testCase "actually read from a file" $
      testreadFile "sample.gull" "increment" "Successfully Compilied!"
  , testCase "test not allowing a non .gull file to compile" $
      testreadFile "fail.py" "willwork" "ILLEGAL FILE"
  ]
