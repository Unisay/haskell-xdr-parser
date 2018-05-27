{-# LANGUAGE ConstraintKinds #-}

module Data.Xdr.Parsing where

import           Data.Xdr.Spec
import qualified Prelude
import           Protolude
import           Text.Megaparsec

type Input = Text
data CustomError
  = KeywordIdentifier Text
  | ConflictingIdentifier Text SourcePos
  deriving (Eq, Ord)

conflictingIdentifier
  :: Parsing p
  => Text
  -> SourcePos
  -> p a
conflictingIdentifier =
  (customFailure .). ConflictingIdentifier

keywordIdentifier
  :: Parsing p
  => Text
  -> p a
keywordIdentifier =
  customFailure . KeywordIdentifier

type ParserError = ParseError (Token Input) CustomError

instance Prelude.Show CustomError where
  show (KeywordIdentifier i) =
    "Keyword '" <> show i <> "' cannot be used as identifier"
  show (ConflictingIdentifier i pos) =
    "Identifier '" <> show i <> " has been already declared at " <> show pos

type Parser = Parsec CustomError Input

type ParserState = [Map Identifier (TypeSpecifier, SourcePos)]


type StatefulParser = StateT ParserState Parser
type Parsing m = ( MonadState ParserState m
                 , MonadParsec CustomError Input m
                 )
