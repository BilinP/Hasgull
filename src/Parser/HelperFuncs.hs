module Parser.Helpers (checkMatchingToken) where

import Text.Megaparsec
import Data.Void (Void)
import Tokenizer.Token (Token(..))
import qualified Tokenizer.Token as T

type Parser = Parsec Void [Token]

-- | Match exactly the given token, consuming one token from the stream.
checkMatchingToken :: Token -> Parser Token
checkMatchingToken t = label (show t) $ satisfy (== t)