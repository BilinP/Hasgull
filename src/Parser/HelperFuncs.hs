module Parser.HelperFuncs ()where


-- import Text.Megaparsec hiding (Token)
-- import Data.Void (Void)
-- import Tokenizer.Token (Token(..))

-- type Parser = Parsec Void [Token]

-- -- | Match exactly the given token, consuming one token from the stream.
-- checkMatchingToken :: Token -> Parser Token
-- checkMatchingToken t = label (show t) $ satisfy (== t)

-- isIdentifierToken :: Token -> Bool
-- isIdentifierToken (IdentifierToken _) = True
-- isIdentifierToken _ = False

