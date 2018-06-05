module Data.Xdr.Parser
  ( specification
  , parseFile
  , runParser
  , runStatefulParser
  ) where

import qualified Data.Text              as T
import qualified Data.Xdr.Lexer         as L
import           Data.Xdr.ParserState   (ParserState, Positioned)
import qualified Data.Xdr.ParserState   as PS
import           Data.Xdr.Parsing
import           Data.Xdr.Parsing.Error
import           Data.Xdr.Types
import           Protolude              hiding (many, try)
import           Text.Megaparsec

parseFile :: FilePath -> IO (Either ParserError Specification)
parseFile path = do
  raw <- readFile path
  let parser = runStatefulParser specification
  forM (runParser parser path raw) $ \(sp, finalState) ->
    print finalState $> sp -- TODO: remove print

runStatefulParser :: StatefulParser a -> Parser (a, ParserState)
runStatefulParser = flip runStateT PS.initialState

specification :: Parsing p => p Specification
specification = between L.space eof $ sepEndBy definition L.space

definition :: Parsing p => p Definition
definition = eitherP typeDef constantDef

addIdentifier :: Parsing p => Identifier -> Positioned TypeSpecifier -> p ()
addIdentifier id pts@(_, ts) = do
  modify (PS.addIdentifier id pts)
  when (isScopeCreating ts) createScope

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
  typeDefEnum = do
    (pos, id) <- positioned $ L.rword "enum" *> identifier
    body <- enumBody <* L.semicolon
    addIdentifier id (pos, TypeEnum body)
    pure $ TypeDefEnum id body

  typeDefStruct :: Parsing p => p TypeDef
  typeDefStruct =
    TypeDefStruct
      <$> (L.rword "struct" *> identifier)
      <*> (structBody <* L.semicolon)

  typeDefUnion :: Parsing p => p TypeDef
  typeDefUnion = do
    (pos, id) <- positioned $ L.rword "union" *> identifier
    body <- unionBody <* L.semicolon
    addIdentifier id (pos, TypeUnion body)
    pure $ TypeDefUnion id body

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
  <$> (L.rword "switch" *> L.parens unionDiscriminant)
  <*> L.braces ((,) <$> unionArms <*> unionDefault)
  where
  body discr (arms, def) = UnionBody discr arms def

  unionDefault :: Parsing p => p (Maybe Declaration)
  unionDefault = optional $ L.rword "default" >> L.colon *> declaration'

  unionDiscriminant :: Parsing p => p Discriminant
  unionDiscriminant = choice
    [ DiscriminantInt  <$> (typeInt  *> identifier)
    , DiscriminantUInt <$> (typeUInt *> identifier)
    , DiscriminantBool <$> (typeBool *> identifier)
    , DiscriminantEnum <$> (typeEnum *> identifier)
    , do idRef <- positioned identifierRef
         id <- positioned identifier
         typeSpec <- evaluateRef idRef
         typedDiscriminant id typeSpec
    ] where

    evaluateRef :: Parsing p => Positioned IdentifierRef -> p TypeSpecifier
    evaluateRef (pos, idRef) = get <&> PS.lookupIdentifierRef idRef >>=
      maybe (invalidDiscriminant pos) (pure . unPositioned)

    typedDiscriminant
      :: Parsing p
      => Positioned Identifier
      -> TypeSpecifier
      -> p Discriminant
    typedDiscriminant (_, id) TypeInt      = pure $ DiscriminantInt  id
    typedDiscriminant (_, id) TypeUInt     = pure $ DiscriminantUInt id
    typedDiscriminant (_, id) TypeBool     = pure $ DiscriminantBool id
    typedDiscriminant (_, id) (TypeEnum _) = pure $ DiscriminantEnum id
    typedDiscriminant (pos, _) ts          = invalidDiscriminantType ts pos

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
  declarationSingle = do
    pts@(_, ts) <- positioned typeSpecifier
    id <- identifier
    addIdentifier id pts
    pure $ DeclarationSingle ts id

  declarationArrayFixLen :: Parsing p => p Declaration
  declarationArrayFixLen = do
    pts@(_, ts) <- positioned typeSpecifier
    id <- identifier
    len <- L.brackets nonNegativeValue
    addIdentifier id pts
    pure $ DeclarationArrayFixLen ts id len

  declarationArrayVarLen :: Parsing p => p Declaration
  declarationArrayVarLen = do
    pts@(_, ts) <- positioned typeSpecifier
    id <- identifier
    len <- L.angles (optional nonNegativeValue)
    addIdentifier id pts
    pure $ DeclarationArrayVarLen ts id len

  declarationOpaqueFixLen :: Parsing p => p Declaration
  declarationOpaqueFixLen = DeclarationOpaqueFixLen
    <$> lookAhead (L.rword "opaque" *> identifier)
    <*> L.brackets nonNegativeValue

  declarationOpaqueVarLen :: Parsing p => p Declaration
  declarationOpaqueVarLen = DeclarationOpaqueVarLen
    <$> (L.rword "opaque" *> identifier)
    <*> L.angles (optional nonNegativeValue)

  declarationString :: Parsing p => p Declaration
  declarationString = DeclarationString
    <$> (L.rword "string" *> identifier)
    <*> L.angles (optional nonNegativeValue)

  declarationOptional :: Parsing p => p Declaration
  declarationOptional = do
    pts@(_, ts) <- positioned typeSpecifier <* L.symbol "*"
    id <- identifier
    addIdentifier id pts
    pure $ DeclarationOptional ts id

  declarationVoid :: Parsing p => p Declaration
  declarationVoid = DeclarationVoid <$ L.rword "void"

declaration' :: Parsing p => p Declaration
declaration' = declaration <* L.semicolon

typeSpecifier :: Parsing p => p TypeSpecifier
typeSpecifier = choice
  [ typeUInt
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
  ]

unsigned :: Parsing p => p ()
unsigned = L.rword "unsigned"

typeUInt :: Parsing p => p TypeSpecifier
typeUInt = TypeUInt
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
typeIdentifier = TypeIdentifier <$> identifierRef

value :: Parsing p => p Value
value = eitherP constant identifierRef

nonNegativeValue :: Parsing p => p Value
nonNegativeValue = do
  pos <- getPosition
  v <- value
  checkNonNegValue pos v
  pure v

  where
    checkNonNegValue pos (Left cd)  =
      when (isNegativeConst cd) (negativeArrayLengthConst pos)

    checkNonNegValue pos (Right idr@(IdentifierRef id)) =
      whenM (isNegativeIdRef idr pos) (negativeArrayLengthId id pos)

    isNegativeConst (DecConstant i) = i < 0
    isNegativeConst (HexConstant i) = i < 0
    isNegativeConst (OctConstant i) = i < 0

    isNegativeIdRef idr@(IdentifierRef id) pos = do
      parserState <- get
      PS.lookupConstantById parserState idr &
        maybe (negativeArrayLengthId id pos) (pure . isNegativeConst)

constantDef :: Parsing p => p ConstantDef
constantDef = do
  cd <- ConstantDef
    <$> (L.rword "const" *> identifier)
    <*> (L.symbol "=" *> constant <* L.semicolon)
  modify (PS.addConstantDef cd)
  pure cd

constant :: Parsing p => p Constant
constant = choice
  [ decimalConstant
  , hexadecimalConstant
  , octalConstant
  ]

identifierWord :: Parsing p => p Text
identifierWord = L.lexeme . try $ do
  word <- T.pack <$> ((:) <$> L.letterChar <*> many L.alphaNumChar)
  -- Check keyword
  if word `elem` L.reservedWords
    then keywordIdentifier word
    else pure word

identifierRef :: Parsing p => p IdentifierRef
identifierRef = IdentifierRef <$> identifierWord

identifier :: Parsing p => p Identifier
identifier = try $ do
  word <- identifierWord
  let id = Identifier word
  -- Check uniqueness
  existing <- get <&> PS.lookupIdentifier id
  maybe (pure id) (conflictingIdentifier word . fst) existing


isScopeCreating :: TypeSpecifier -> Bool
isScopeCreating (TypeStruct _) = True
isScopeCreating (TypeUnion  _) = True
isScopeCreating _              = False

createScope :: Parsing p => p ()
createScope = modify PS.createScope

decimalConstant :: Parsing p => p Constant
decimalConstant = DecConstant <$> L.signed L.space (L.lexeme L.decimal)

octalConstant :: Parsing p => p Constant
octalConstant = OctConstant <$> (L.char '0' >> L.octal)

hexadecimalConstant :: Parsing p => p Constant
hexadecimalConstant = HexConstant
  <$> (L.char '0' >> L.char' 'x' >> L.hexadecimal)

positioned :: Parsing p => p a -> p (Positioned a)
positioned p = (,) <$> getPosition <*> p

unPositioned :: Positioned a -> a
unPositioned = snd
