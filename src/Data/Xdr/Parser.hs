module Data.Xdr.Parser
  ( specification
  , parseFile
  ) where

import           Control.Arrow   (left)
import qualified Data.Text       as T
import           Data.Xdr.Lexer  (Parser)
import qualified Data.Xdr.Lexer  as L
import           Data.Xdr.Spec
import qualified Prelude
import           Protolude       hiding (check, many, try)
import           Text.Megaparsec


parseFile :: FilePath -> IO (Either Text Specification)
parseFile path = left show . runParser specification path <$> readFile path

specification :: Parser Specification
specification = L.space *> sepEndBy definition L.space

definition :: Parser Definition
definition = eitherP typeDef constantDef

typeDef :: Parser TypeDef
typeDef = choice
  [ typeDef'
  , typeDefEnum
  , typeDefStruct
  , typeDefUnion
  ] where

  typeDef' :: Parser TypeDef
  typeDef' = TypeDef
    <$> (L.reserved "typedef" *> declaration')

  typeDefEnum :: Parser TypeDef
  typeDefEnum = TypeDefEnum
    <$> (L.reserved "enum" *> identifier)
    <*> (enumBody <* L.semicolon)

  typeDefStruct :: Parser TypeDef
  typeDefStruct =
    TypeDefStruct
      <$> (L.reserved "struct" *> identifier)
      <*> (structBody <* L.semicolon)

  typeDefUnion :: Parser TypeDef
  typeDefUnion =
    TypeDefUnion
      <$> (L.reserved "union" *> identifier)
      <*> (unionBody <* L.semicolon)

enumBody :: Parser EnumBody
enumBody = L.braces $ L.nonEmptyList L.comma idValue
  where
  idValue :: Parser (Identifier, Value)
  idValue = (,)
    <$> identifier <* L.symbol "="
    <*> value

structBody :: Parser StructBody
structBody = L.braces $ L.nonEmptyLines declaration'

unionBody :: Parser UnionBody
unionBody = body
  <$> unionDiscriminant
  <*> L.braces ((,) <$> unionArms <*> unionDefault)
  where
  body discr (arms, def) = UnionBody discr arms def

  unionDefault :: Parser (Maybe Declaration)
  unionDefault = optional $ L.reserved "default" >> L.colon *> declaration'

  unionDiscriminant :: Parser Declaration
  unionDiscriminant = L.reserved "switch" *> L.parens declaration

  unionArms :: Parser (NonEmpty CaseSpec)
  unionArms = L.nonEmptyLines $
    CaseSpec <$> caseSpecValues <*> declaration'

  caseSpecValues :: Parser (NonEmpty Value)
  caseSpecValues = L.nonEmptyLines $
    L.reserved "case" *> value <* L.colon

declaration :: Parser Declaration
declaration = choice
  [ declarationSingle
  , declarationArrayFixLen
  , declarationArrayVarLen
  , declarationOpaqueFixLen
  , declarationOpaqueVarLen
  , declarationString
  , declarationOptional
  , declarationVoid
  ] where

  declarationSingle :: Parser Declaration
  declarationSingle = DeclarationSingle
    <$> typeSpecifier
    <*> identifier

  declarationArrayFixLen :: Parser Declaration
  declarationArrayFixLen = DeclarationArrayFixLen
    <$> typeSpecifier
    <*> identifier
    <*> L.brackets value

  declarationArrayVarLen :: Parser Declaration
  declarationArrayVarLen = DeclarationArrayVarLen
    <$> typeSpecifier
    <*> identifier
    <*> L.angles (optional value)

  declarationOpaqueFixLen :: Parser Declaration
  declarationOpaqueFixLen = DeclarationOpaqueFixLen
    <$> lookAhead (L.reserved "opaque" *> identifier)
    <*> L.brackets value

  declarationOpaqueVarLen :: Parser Declaration
  declarationOpaqueVarLen = DeclarationOpaqueVarLen
    <$> (L.reserved "opaque" *> identifier)
    <*> L.angles (optional value)

  declarationString :: Parser Declaration
  declarationString = DeclarationString
    <$> (L.reserved "string" *> identifier)
    <*> L.angles (optional value)

  declarationOptional :: Parser Declaration
  declarationOptional = DeclarationOptional
    <$> (typeSpecifier <* L.symbol "*")
    <*> identifier

  declarationVoid :: Parser Declaration
  declarationVoid = DeclarationVoid <$ L.reserved "void"

declaration' :: Parser Declaration
declaration' = declaration <* L.semicolon

typeSpecifier :: Parser TypeSpecifier
typeSpecifier = choice
  [ typeUnsignedInt
  , typeInt
  , typeUnsignedHyper
  , typeHyper
  , typeFloat
  , typeDouble
  , typeQuadruple
  , typeBool
  , typeEnum
  , typeStruct
  , typeUnion
  , typeIdentifier
  ] where

  unsigned = L.reserved "unsigned"

  typeUnsignedInt :: Parser TypeSpecifier
  typeUnsignedInt = TypeUnsignedInt
    <$ (unsigned >> L.reserved "int")

  typeInt :: Parser TypeSpecifier
  typeInt = TypeInt <$ L.reserved "int"

  typeUnsignedHyper :: Parser TypeSpecifier
  typeUnsignedHyper = TypeUnsignedHyper
    <$ (unsigned >> L.reserved "hyper")

  typeHyper :: Parser TypeSpecifier
  typeHyper = TypeHyper <$ L.reserved "hyper"

  typeFloat :: Parser TypeSpecifier
  typeFloat = TypeFloat <$ L.reserved "float"

  typeDouble :: Parser TypeSpecifier
  typeDouble = TypeDouble <$ L.reserved "double"

  typeQuadruple :: Parser TypeSpecifier
  typeQuadruple = TypeQuadruple <$ L.reserved "quadruple"

  typeBool :: Parser TypeSpecifier
  typeBool = TypeBool <$ L.reserved "bool"

  typeEnum :: Parser TypeSpecifier
  typeEnum = TypeEnum <$> (L.reserved "enum" *> enumBody)

  typeStruct :: Parser TypeSpecifier
  typeStruct = TypeStruct <$> (L.reserved "struct" *> structBody)

  typeUnion :: Parser TypeSpecifier
  typeUnion = TypeUnion <$> (L.reserved "union" *> unionBody)

  typeIdentifier :: Parser TypeSpecifier
  typeIdentifier = TypeIdentifier <$> identifier

value :: Parser Value
value = eitherP constant identifier

constantDef :: Parser ConstantDef
constantDef = ConstantDef
  <$> (L.reserved "const" *> identifier)
  <*> (L.symbol "=" *> constant <* L.semicolon)

constant :: Parser Constant
constant = choice
  [ decimalConstant
  , hexadecimalConstant
  , octalConstant
  ]

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> (L.lexeme . try) (p >>= check)
 where
  p = (:) <$> L.letterChar <*> many L.alphaNumChar
  check x = if x `elem` L.reservedWords
    then Prelude.fail $ "keyword " <> show x <> " cannot be an identifier"
    else pure x

decimalConstant :: Parser Constant
decimalConstant = DecConstant <$> L.signed L.space (L.lexeme L.decimal)

octalConstant :: Parser Constant
octalConstant = OctConstant <$> (L.char '0' >> L.octal)

hexadecimalConstant :: Parser Constant
hexadecimalConstant = HexConstant <$> (L.char '0' >> L.char' 'x' >> L.hexadecimal)
