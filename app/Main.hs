module Main (main) where

import Lib
import Tokenizer.Tokenizer

main :: IO ()
main = do
  let testInput = "while(x+3)"
  case tokenize testInput of
    Left err  -> putStrLn $ "Tokenization failed: " ++ err
    Right toks -> putStrLn $ "Tokenization succeeded: " ++ show toks

