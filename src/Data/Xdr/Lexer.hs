module Data.Xdr.Lexer
  ( module Text.Megaparsec.Char.Lexer
  , module Text.Megaparsec.Char
  , Parser
  , angles
  , braces
  , brackets
  , colon
  , comma
  , lexeme
  , nonEmptyLines
  , nonEmptyList
  , parens
  , reserved
  , reservedWords
  , symbol
  , space
  , semicolon
  ) where

import qualified Prelude
import           Protolude                  hiding (many)
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (space)
import           Text.Megaparsec.Char.Lexer hiding (lexeme, space, symbol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Space consumer
space :: Parser ()
space = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

-- | Consume whitespace after every lexeme automatically, but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

colon :: Parser Text
colon = symbol ":"

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | @'nonEmptyList' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
nonEmptyList :: Parser sep -> Parser a -> Parser (NonEmpty a)
nonEmptyList sep p = do
  x <- p
  (x :|) <$> many (sep >> p)
{-# INLINE nonEmptyList #-}

nonEmptyLines :: Parser a -> Parser (NonEmpty a)
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

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> space
