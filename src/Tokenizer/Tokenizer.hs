module Tokenizer.Tokenizer
    ( someFunc
    ) where

-- Goal: Take a string and return a list of tokens through our Tokenizer.
-- 1. Take a string and split it into words.  We can do this by calling the default words function on our input string.
-- 2. Go through our list of words and determine if it matches any of our tokens. If it does, append it to oru list of tokens

data Token = IdentifierToken String | IntegerToken Int deriving(Show,Eq)




-- Uses words to split a string into a list of strings. Pattern matching to deal with emtpy strings. 
-- Unsure of how to use this to put into a list so we can actually go through and tokenize it.
stripWhiteSpace :: String -> [String]
stripWhiteSpace [] = []
stripWhiteSpace x = words x

testToUseDo :: String -> [String]
testToUseDo x = do
  let y = stripWhiteSpace x
  --Do later
      

someFunc :: IO ()
someFunc = putStrLn "someFunc"
