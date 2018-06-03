module Data.Xdr.ParserState
  ( ParserState
  , initialState
  -- , addIdentifier
  , addConstantDef
  , lookupConstantById
  -- , lookupIdentifier
  ) where

import qualified Data.List          as List
import           Data.List.NonEmpty
import qualified Data.Map           as Map
import           Data.Xdr.Types
import           Protolude
import           Text.Megaparsec    (SourcePos)

type Scope = Map Identifier (TypeSpecifier, SourcePos)

data ParserState
  = ParserState [ConstantDef] (NonEmpty Scope)
  deriving Show

initialState :: ParserState
initialState = ParserState mempty (pure emptyScope)

emptyScope :: Scope
emptyScope = Map.empty

addConstantDef :: ConstantDef -> ParserState -> ParserState
addConstantDef cd (ParserState cds scopes) = ParserState (cd : cds) scopes

lookupConstantById :: ParserState -> IdentifierRef -> Maybe Constant
lookupConstantById (ParserState cds _) ir =
  List.find (f ir) cds <&> \(ConstantDef _ c) -> c
  where f (IdentifierRef r) (ConstantDef (Identifier i) _) = r == i

-- addIdentifier :: (Identifier, SourcePos) -> ParserState -> ParserState
-- addIdentifier = notImplemented
--
-- lookupIdentifier :: ParserState -> Identifier -> Maybe (TypeSpecifier, SourcePos)
-- lookupIdentifier st id = notImplemented

-- lookupIdentifierInScope :: Scope -> Identifier -> Maybe (TypeSpecifier, SourcePos)
-- lookupIdentifierInScope = flip Map.lookup
