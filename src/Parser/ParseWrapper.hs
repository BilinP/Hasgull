module Parser.ParseWrapper (parseFromString) where

import Parser.Parser (parseTokens)
import Tokenizer.Tokenizer (tokenize)
import Parser.Expr (Program)

--defined function that will tokenize and then parse
parseFromString :: String -> Either String Program
parseFromString input = do
  tokens <- tokenize input
  return (parseTokens tokens)