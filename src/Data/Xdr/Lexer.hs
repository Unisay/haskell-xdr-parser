module Data.Xdr.Lexer
  ( module Text.Megaparsec.Char.Lexer
  , module Text.Megaparsec.Char
  , angles
  , braces
  , brackets
  , colon
  , comma
  , lexeme
  , nonEmptyLines
  , nonEmptyList
  , parens
  , rword
  , reservedWords
  , symbol
  , space
  , semicolon
  ) where

import qualified Prelude
import           Protolude                  hiding (many, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (space)
import           Text.Megaparsec.Char.Lexer hiding (lexeme, space, symbol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser m = ParsecT Void Text m

-- | Space consumer
space :: Parser m ()
space = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

-- | Consume whitespace after every lexeme automatically, but not before it.
lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme space

symbol :: Text -> Parser m Text
symbol = L.symbol space

colon :: Parser m Text
colon = symbol ":"

semicolon :: Parser m Text
semicolon = symbol ";"

comma :: Parser m Text
comma = symbol ","

parens :: Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")

angles :: Parser m a -> Parser m a
angles = between (symbol "<") (symbol ">")

braces :: Parser m a -> Parser m a
braces = between (symbol "{") (symbol "}")

brackets :: Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")

-- | @'nonEmptyList' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
nonEmptyList :: Parser m sep -> Parser m a -> Parser m (NonEmpty a)
nonEmptyList sep p = do
  x <- p
  (x :|) <$> many (sep >> p)
{-# INLINE nonEmptyList #-}

nonEmptyLines :: Parser m a -> Parser m (NonEmpty a)
nonEmptyLines = nonEmptyList space
{-# INLINE nonEmptyLines #-}

reservedWords :: [Prelude.String]
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
  ]

rword :: Text -> Parser m ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)
