module Data.Xdr.Parser where

import qualified Data.Text                  as T
import           Data.Xdr.Spec
import qualified Prelude
import           Protolude                  hiding (check, many, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Space consumer
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

-- | Consume whitespace after every lexeme automatically, but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- (3) An identifier is a letter followed by an optional sequence of letters,
-- digits, or underbar (’_’). The case of identifiers is not ignored.
identifier :: Parser Identifier
identifier = Identifier <$> (lexeme . try) (p >>= check)
  where
    p = T.cons
      <$> letterChar
      <*> fmap T.pack (many alphaNumChar)
    check x =
      if x `elem` reservedWords
      then Prelude.fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else pure x

reservedWords :: [Text]
reservedWords =
  [ "bool"
  , "case"
  , "const"
  , "default"
  , "double"
  , "quadruple"
  , "enum"
  , "float"
  , "hyper"
  , "int"
  , "opaque"
  , "string"
  , "struct"
  , "switch"
  , "typedef"
  , "union"
  , "unsigned"
  , "void"
  , "program"
  , "version"
  ]

-- A decimal constant expresses a number in base 10 and is a
-- sequence of one or more decimal digits, where the first digit is not
-- a zero, and is optionally preceded by a minus-sign (’-’).
decimalConstant :: Parser Constant
decimalConstant = DecConstant <$> L.signed spaceConsumer (lexeme L.decimal)

-- An octal constant expresses a number in base 8, always leads with digit 0,
-- and is a sequence of one or more octal digits (’0’, ’1’, ’2’, ’3’, ’4’, ’5’, ’6’, ’7’).
octalConstant :: Parser Constant
octalConstant = OctConstant <$> (char '0' >> L.octal)

-- A hexadecimal constant expresses a number in base 16, and must be
-- preceded by ’0x’, followed by one or hexadecimal digits (’A’, ’B’,
-- ’C’, ’D’, E’, ’F’, ’a’, ’b’, ’c’, ’d’, ’e’, ’f’, ’0’, ’1’, ’2’, ’3’,
-- ’4’, ’5’, ’6’, ’7’, ’8’, ’9’).
hexadecimalConstant :: Parser Constant
hexadecimalConstant = HexConstant <$> (char '0' >> char' 'x' >> L.hexadecimal)
