module Data.Xdr.ParserState where

import qualified Data.List          as List
import           Data.List.NonEmpty
import qualified Data.Map           as Map
import           Data.Xdr.Types
import           Protolude
import           Text.Megaparsec    (SourcePos)

type Positioned a = (SourcePos, a)

type Scope = Map Identifier (Positioned TypeSpecifier)

data ParserState
  = ParserState
  [Positioned ConstantDef]
  [Positioned (Identifier, TypeSpecifier)]
  (NonEmpty Scope)
  deriving Show

initialState :: ParserState
initialState = ParserState mempty mempty (pure emptyScope)

emptyScope :: Scope
emptyScope = Map.empty

createScope :: ParserState -> ParserState
createScope (ParserState cds tds scopes) =
  ParserState cds tds (emptyScope <| scopes)

addTypeIdentifier
  :: Positioned (Identifier, TypeSpecifier)
  -> ParserState
  -> ParserState
addTypeIdentifier idTypeSpec (ParserState constDefs idTypeSpecs scopes) =
  ParserState constDefs (idTypeSpec : idTypeSpecs) scopes

typeSpecById :: Identifier -> ParserState -> Maybe (Positioned TypeSpecifier)
typeSpecById id (ParserState _ typeSpecs _) =
  List.find f typeSpecs <&> \(pos, (_, ts)) -> (pos, ts)
  where f (_, (i, _)) = id == i

addConstantDef
  :: Positioned ConstantDef
  -> ParserState
  -> ParserState
addConstantDef cd (ParserState cds tds scopes) =
  ParserState (cd : cds) tds scopes

constantById
  :: ParserState
  -> IdentifierRef
  -> Maybe (Positioned ConstantDef)
constantById (ParserState cds _ _) ir = List.find (f ir) cds
  where f (IdentifierRef r) (_, ConstantDef (Identifier i) _) = r == i

addDeclaration
  :: Identifier
  -> Positioned TypeSpecifier
  -> ParserState
  -> ParserState
addDeclaration id posTs (ParserState cds tds (h :| t)) =
  ParserState cds tds (Map.insert id posTs h :| t)

declarationByIdRef :: IdentifierRef -> ParserState -> Maybe (Positioned TypeSpecifier)
declarationByIdRef (IdentifierRef r) = declarationById (Identifier r)

declarationById :: Identifier -> ParserState -> Maybe (Positioned TypeSpecifier)
declarationById ir (ParserState _ _ scopes) =
  getFirst $ foldMap (First . Map.lookup ir) scopes


declarationIdentifierPos :: Identifier -> ParserState -> Maybe SourcePos
declarationIdentifierPos id ps = fst <$> declarationById id ps

typeIdentifierPos :: Identifier -> ParserState -> Maybe SourcePos
typeIdentifierPos id ps = fst <$> typeSpecById id ps

constantIdentifierPos :: Identifier -> ParserState -> Maybe SourcePos
constantIdentifierPos (Identifier i) ps =
  fst <$> constantById ps (IdentifierRef i)

typeOrConstandIdPos :: Identifier -> ParserState -> Maybe SourcePos
typeOrConstandIdPos id ps =
  typeIdentifierPos id ps <|> constantIdentifierPos id ps

identifierPos :: Identifier -> ParserState -> Maybe SourcePos
identifierPos id ps =
  typeOrConstandIdPos id ps <|> declarationIdentifierPos id ps
