module Data.Xdr.Parser
  ( specification
  , parseFile
  , runParser
  ) where

import           Control.Arrow    (left)
import qualified Data.Text        as T
import qualified Data.Xdr.Lexer   as L
import           Data.Xdr.Parsing
import           Data.Xdr.Spec
import qualified Prelude
import           Protolude        hiding (check, many, try)
import           Text.Megaparsec

parseFile :: FilePath -> IO (Either Text Specification)
parseFile path = readFile path
             <&> runParser spec path
             <&> left show
  where
    spec :: Parser Specification
    spec = evalStateT specification initialState

    initialState :: ParserState
    initialState = mempty

specification :: Parsing p => p Specification
specification = between L.space eof $ sepEndBy definition L.space

definition :: Parsing p => p Definition
definition = eitherP typeDef constantDef

typeDef :: Parsing p => p TypeDef
typeDef = choice
  [ typeDef'
  , typeDefEnum
  , typeDefStruct
  , typeDefUnion
  ] where

  typeDef' :: Parsing p => p TypeDef
  typeDef' = TypeDef
    <$> (L.rword "typedef" *> declaration')

  typeDefEnum :: Parsing p => p TypeDef
  typeDefEnum = TypeDefEnum
    <$> (L.rword "enum" *> identifier)
    <*> (enumBody <* L.semicolon)

  typeDefStruct :: Parsing p => p TypeDef
  typeDefStruct =
    TypeDefStruct
      <$> (L.rword "struct" *> identifier)
      <*> (structBody <* L.semicolon)

  typeDefUnion :: Parsing p => p TypeDef
  typeDefUnion =
    TypeDefUnion
      <$> (L.rword "union" *> identifier)
      <*> (unionBody <* L.semicolon)

enumBody :: Parsing p => p EnumBody
enumBody = L.braces $ L.nonEmptyList L.comma idValue
  where
  idValue :: Parsing p => p (Identifier, Value)
  idValue = (,)
    <$> identifier <* L.symbol "="
    <*> value

structBody :: Parsing p => p StructBody
structBody = L.braces $ L.nonEmptyLines declaration'

unionBody :: Parsing p => p UnionBody
unionBody = body
  <$> unionDiscriminant
  <*> L.braces ((,) <$> unionArms <*> unionDefault)
  where
  body discr (arms, def) = UnionBody discr arms def

  unionDefault :: Parsing p => p (Maybe Declaration)
  unionDefault = optional $ L.rword "default" >> L.colon *> declaration'

  unionDiscriminant :: Parsing p => p Declaration
  unionDiscriminant = L.rword "switch" *> L.parens declaration

  unionArms :: Parsing p => p (NonEmpty CaseSpec)
  unionArms = L.nonEmptyLines $
    CaseSpec <$> caseSpecValues <*> declaration'

  caseSpecValues :: Parsing p => p (NonEmpty Value)
  caseSpecValues = L.nonEmptyLines $
    L.rword "case" *> value <* L.colon

declaration :: Parsing p => p Declaration
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

  declarationSingle :: Parsing p => p Declaration
  declarationSingle = DeclarationSingle
    <$> typeSpecifier
    <*> identifier

  declarationArrayFixLen :: Parsing p => p Declaration
  declarationArrayFixLen = DeclarationArrayFixLen
    <$> typeSpecifier
    <*> identifier
    <*> L.brackets value

  declarationArrayVarLen :: Parsing p => p Declaration
  declarationArrayVarLen = DeclarationArrayVarLen
    <$> typeSpecifier
    <*> identifier
    <*> L.angles (optional value)

  declarationOpaqueFixLen :: Parsing p => p Declaration
  declarationOpaqueFixLen = DeclarationOpaqueFixLen
    <$> lookAhead (L.rword "opaque" *> identifier)
    <*> L.brackets value

  declarationOpaqueVarLen :: Parsing p => p Declaration
  declarationOpaqueVarLen = DeclarationOpaqueVarLen
    <$> (L.rword "opaque" *> identifier)
    <*> L.angles (optional value)

  declarationString :: Parsing p => p Declaration
  declarationString = DeclarationString
    <$> (L.rword "string" *> identifier)
    <*> L.angles (optional value)

  declarationOptional :: Parsing p => p Declaration
  declarationOptional = DeclarationOptional
    <$> (typeSpecifier <* L.symbol "*")
    <*> identifier

  declarationVoid :: Parsing p => p Declaration
  declarationVoid = DeclarationVoid <$ L.rword "void"

declaration' :: Parsing p => p Declaration
declaration' = declaration <* L.semicolon

typeSpecifier :: Parsing p => p TypeSpecifier
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

  unsigned :: Parsing p => p ()
  unsigned = L.rword "unsigned"

  typeUnsignedInt :: Parsing p => p TypeSpecifier
  typeUnsignedInt = TypeUnsignedInt
    <$ (unsigned >> L.rword "int")

  typeInt :: Parsing p => p TypeSpecifier
  typeInt = TypeInt <$ L.rword "int"

  typeUnsignedHyper :: Parsing p => p TypeSpecifier
  typeUnsignedHyper = TypeUnsignedHyper
    <$ (unsigned >> L.rword "hyper")

  typeHyper :: Parsing p => p TypeSpecifier
  typeHyper = TypeHyper <$ L.rword "hyper"

  typeFloat :: Parsing p => p TypeSpecifier
  typeFloat = TypeFloat <$ L.rword "float"

  typeDouble :: Parsing p => p TypeSpecifier
  typeDouble = TypeDouble <$ L.rword "double"

  typeQuadruple :: Parsing p => p TypeSpecifier
  typeQuadruple = TypeQuadruple <$ L.rword "quadruple"

  typeBool :: Parsing p => p TypeSpecifier
  typeBool = TypeBool <$ L.rword "bool"

  typeEnum :: Parsing p => p TypeSpecifier
  typeEnum = TypeEnum <$> (L.rword "enum" *> enumBody)

  typeStruct :: Parsing p => p TypeSpecifier
  typeStruct = TypeStruct <$> (L.rword "struct" *> structBody)

  typeUnion :: Parsing p => p TypeSpecifier
  typeUnion = TypeUnion <$> (L.rword "union" *> unionBody)

  typeIdentifier :: Parsing p => p TypeSpecifier
  typeIdentifier = TypeIdentifier <$> identifier

value :: Parsing p => p Value
value = eitherP constant identifier

constantDef :: Parsing p => p ConstantDef
constantDef = ConstantDef
  <$> (L.rword "const" *> identifier)
  <*> (L.symbol "=" *> constant <* L.semicolon)

constant :: Parsing p => p Constant
constant = choice
  [ decimalConstant
  , hexadecimalConstant
  , octalConstant
  ]

identifier :: Parsing p => p Identifier
identifier = Identifier . T.pack <$> (L.lexeme . try) (p >>= check)
 where
   p = (:) <$> L.letterChar <*> many L.alphaNumChar <?> "identifier"
   check x = if x `elem` L.reservedWords
     then Prelude.fail $ "keyword " <> show x <> " cannot be an identifier"
     else pure x

decimalConstant :: Parsing p => p Constant
decimalConstant = DecConstant <$> L.signed L.space (L.lexeme L.decimal)

octalConstant :: Parsing p => p Constant
octalConstant = OctConstant <$> (L.char '0' >> L.octal)

hexadecimalConstant :: Parsing p => p Constant
hexadecimalConstant = HexConstant <$> (L.char '0' >> L.char' 'x' >> L.hexadecimal)
