module Main (main) where

import Tokenizer.Tokenizer

main :: IO ()
main = do
  putStrLn "Enter input to tokenize:"
  input <- readMultipleLines
  case tokenize input of
    Left err -> putStrLn $ "Tokenization failed: " ++ err
    Right toks -> do
      putStrLn "Tokenization succeeded. Tokens:"
      print toks

-- Helper function to read multiple lines until an empty line is entered
readMultipleLines :: IO String
readMultipleLines = do
  line <- getLine
  if null line
    then return "" -- Stop reading when an empty line is entered
    else do
      rest <- readMultipleLines
      return (line ++ "\n" ++ rest)