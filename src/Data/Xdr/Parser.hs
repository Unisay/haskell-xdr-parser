module Data.Xdr.Parser
  ( specification
  , parseFile
  ) where

import           Control.Arrow   (left)
import qualified Data.Text       as T
import qualified Data.Xdr.Lexer  as L
import           Data.Xdr.Spec
import qualified Prelude
import           Protolude       hiding (check, many, try)
import           Text.Megaparsec hiding (State)

type Scope = Text
type S = StateT (Map Scope [Identifier]) IO
type Parser = ParsecT Void Text S

parseFile :: FilePath -> IO (Either Text Specification)
parseFile =  b . c
  where
  b :: S (Either (ParseError (Token Text) Void) Specification) -> IO (Either Text Specification)
  b s = left show <$> evalStateT s mempty
  c :: FilePath -> S (Either (ParseError (Token Text) Void) Specification)
  c path = liftIO (readFile path) >>= runParserT specification path

specification :: Parser Specification
specification = between L.space eof $ sepEndBy definition L.space

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
    <$> (L.rword "typedef" *> declaration')

  typeDefEnum :: Parser TypeDef
  typeDefEnum = TypeDefEnum
    <$> (L.rword "enum" *> identifier)
    <*> (enumBody <* L.semicolon)

  typeDefStruct :: Parser TypeDef
  typeDefStruct =
    TypeDefStruct
      <$> (L.rword "struct" *> identifier)
      <*> (structBody <* L.semicolon)

  typeDefUnion :: Parser TypeDef
  typeDefUnion =
    TypeDefUnion
      <$> (L.rword "union" *> identifier)
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
  unionDefault = optional $ L.rword "default" >> L.colon *> declaration'

  unionDiscriminant :: Parser Declaration
  unionDiscriminant = L.rword "switch" *> L.parens declaration

  unionArms :: Parser (NonEmpty CaseSpec)
  unionArms = L.nonEmptyLines $
    CaseSpec <$> caseSpecValues <*> declaration'

  caseSpecValues :: Parser (NonEmpty Value)
  caseSpecValues = L.nonEmptyLines $
    L.rword "case" *> value <* L.colon

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
    <$> lookAhead (L.rword "opaque" *> identifier)
    <*> L.brackets value

  declarationOpaqueVarLen :: Parser Declaration
  declarationOpaqueVarLen = DeclarationOpaqueVarLen
    <$> (L.rword "opaque" *> identifier)
    <*> L.angles (optional value)

  declarationString :: Parser Declaration
  declarationString = DeclarationString
    <$> (L.rword "string" *> identifier)
    <*> L.angles (optional value)

  declarationOptional :: Parser Declaration
  declarationOptional = DeclarationOptional
    <$> (typeSpecifier <* L.symbol "*")
    <*> identifier

  declarationVoid :: Parser Declaration
  declarationVoid = DeclarationVoid <$ L.rword "void"

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

  unsigned = L.rword "unsigned"

  typeUnsignedInt :: Parser TypeSpecifier
  typeUnsignedInt = TypeUnsignedInt
    <$ (unsigned >> L.rword "int")

  typeInt :: Parser TypeSpecifier
  typeInt = TypeInt <$ L.rword "int"

  typeUnsignedHyper :: Parser TypeSpecifier
  typeUnsignedHyper = TypeUnsignedHyper
    <$ (unsigned >> L.rword "hyper")

  typeHyper :: Parser TypeSpecifier
  typeHyper = TypeHyper <$ L.rword "hyper"

  typeFloat :: Parser TypeSpecifier
  typeFloat = TypeFloat <$ L.rword "float"

  typeDouble :: Parser TypeSpecifier
  typeDouble = TypeDouble <$ L.rword "double"

  typeQuadruple :: Parser TypeSpecifier
  typeQuadruple = TypeQuadruple <$ L.rword "quadruple"

  typeBool :: Parser TypeSpecifier
  typeBool = TypeBool <$ L.rword "bool"

  typeEnum :: Parser TypeSpecifier
  typeEnum = TypeEnum <$> (L.rword "enum" *> enumBody)

  typeStruct :: Parser TypeSpecifier
  typeStruct = TypeStruct <$> (L.rword "struct" *> structBody)

  typeUnion :: Parser TypeSpecifier
  typeUnion = TypeUnion <$> (L.rword "union" *> unionBody)

  typeIdentifier :: Parser TypeSpecifier
  typeIdentifier = TypeIdentifier <$> identifier

value :: Parser Value
value = eitherP constant identifier

constantDef :: Parser ConstantDef
constantDef = ConstantDef
  <$> (L.rword "const" *> identifier)
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
  p = (:) <$> L.letterChar <*> many L.alphaNumChar <?> "identifier"
  check x = if x `elem` L.reservedWords
    then Prelude.fail $ "keyword " <> show x <> " cannot be an identifier"
    else pure x

decimalConstant :: Parser Constant
decimalConstant = DecConstant <$> L.signed L.space (L.lexeme L.decimal)

octalConstant :: Parser Constant
octalConstant = OctConstant <$> (L.char '0' >> L.octal)

hexadecimalConstant :: Parser Constant
hexadecimalConstant = HexConstant <$> (L.char '0' >> L.char' 'x' >> L.hexadecimal)
