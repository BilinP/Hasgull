module Main (main) where

import Tokenizer.Tokenizer
import System.Environment (getArgs)
import Parser.Parser(pProgram)
import System.IO (readFile)
import Generation.Generation (createOutputFile)
import Control.Exception ()
import Data.List (isSuffixOf)
import Parser.AST (Program(progItems))


--main execution point
-- Take cl arguments, run them through compile
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "no file provided"
    theArgs -> do
      if length theArgs > 2 then
        putStrLn "too many CL arguments"
        else do
          case theArgs of 
            [firstArg,secondArg] -> putStrLn =<< hasgullExecution firstArg secondArg
            _ -> putStrLn "How did you even get here?"

-- The "compile" logic I guess
-- Uses all of our compiler points tokenizer -> parser -> generator 
-- returns a IO String (cause it's in a do that then above uses =<< to bind it to a IO)
hasgullExecution :: String -> String -> IO String
hasgullExecution inputFile outputName = do
  if ".gull" `isSuffixOf`inputFile 
    then do
      currFile <- readFile inputFile
      let sample = lines currFile
      let extracted = concatMap (++ "") sample
      case tokenize extracted of 
        Right tokens ->
          case pProgram tokens of
            Right prog -> do
              createOutputFile prog outputName
            Left err -> 
              return "Failed to Compile:" 
        Left err ->
           return "Failed to Compile:" 
    else
      return "Failed to compile: illegal file"

