module Data.Xdr.ParserState
  ( ParserState
  , Positioned
  , createScope
  , initialState
  , addIdentifier
  , addConstantDef
  , lookupConstantById
  , lookupIdentifier
  , lookupIdentifierRef
  ) where


import qualified Data.List          as List
import           Data.List.NonEmpty
import qualified Data.Map           as Map
import           Data.Xdr.Types
import           Protolude
import           Text.Megaparsec    (SourcePos)

type Positioned a = (SourcePos, a)

type Scope = Map Identifier (Positioned TypeSpecifier)

data ParserState
  = ParserState [ConstantDef] (NonEmpty Scope)
  deriving Show

initialState :: ParserState
initialState = ParserState mempty (pure emptyScope)

emptyScope :: Scope
emptyScope = Map.empty

createScope :: ParserState -> ParserState
createScope (ParserState cds scopes) = ParserState cds (emptyScope <| scopes)

addConstantDef :: ConstantDef -> ParserState -> ParserState
addConstantDef cd (ParserState cds scopes) = ParserState (cd : cds) scopes

lookupConstantById :: ParserState -> IdentifierRef -> Maybe Constant
lookupConstantById (ParserState cds _) ir =
  List.find (f ir) cds <&> \(ConstantDef _ c) -> c
  where f (IdentifierRef r) (ConstantDef (Identifier i) _) = r == i

addIdentifier :: Identifier -> Positioned TypeSpecifier -> ParserState -> ParserState
addIdentifier id posTs (ParserState cds (h :| t)) =
  ParserState cds (Map.insert id posTs h :| t)

lookupIdentifierRef :: IdentifierRef -> ParserState -> Maybe (Positioned TypeSpecifier)
lookupIdentifierRef (IdentifierRef r) = lookupIdentifier (Identifier r)

lookupIdentifier :: Identifier -> ParserState -> Maybe (Positioned TypeSpecifier)
lookupIdentifier ir (ParserState _ scopes) =
  getFirst $ foldMap (First . Map.lookup ir) scopes
