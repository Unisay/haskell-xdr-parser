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

import           Data.Xdr.Parsing
import           Protolude                  hiding (many, try)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import           Text.Megaparsec.Char.Lexer hiding (lexeme, space, symbol)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Space consumer
space :: Parsing p => p ()
space = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

-- | Consume whitespace after every lexeme automatically, but not before it.
lexeme :: Parsing p => p a -> p a
lexeme = L.lexeme space

symbol :: Parsing p => Text -> p Text
symbol = L.symbol space

colon :: Parsing p => p Text
colon = symbol ":"

semicolon :: Parsing p => p Text
semicolon = symbol ";"

comma :: Parsing p => p Text
comma = symbol ","

parens :: Parsing p => p a -> p a
parens = between (symbol "(") (symbol ")")

angles :: Parsing p => p a -> p a
angles = between (symbol "<") (symbol ">")

braces :: Parsing p => p a -> p a
braces = between (symbol "{") (symbol "}")

brackets :: Parsing p => p a -> p a
brackets = between (symbol "[") (symbol "]")

-- | @'nonEmptyList' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
nonEmptyList :: Parsing p => p sep -> p a -> p (NonEmpty a)
nonEmptyList sep p = do
  x <- p
  (x :|) <$> many (sep >> p)
{-# INLINE nonEmptyList #-}

nonEmptyLines :: Parsing p => p a -> p (NonEmpty a)
nonEmptyLines = nonEmptyList space
{-# INLINE nonEmptyLines #-}

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
  ]

rword :: Parsing p => Text -> p ()
rword w = lexeme . try $ string w *> notFollowedBy alphaNumChar
