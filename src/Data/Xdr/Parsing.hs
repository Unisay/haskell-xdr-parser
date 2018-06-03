{-# LANGUAGE ConstraintKinds #-}

module Data.Xdr.Parsing where

import           Data.Xdr.ParserState
import qualified Prelude
import           Protolude
import           Text.Megaparsec

type Input = Text
data CustomError
  = KeywordIdentifier Text
  | ConflictingIdentifier Text SourcePos
  | NegativeArrayLengthConst SourcePos
  | NegativeArrayLengthIdentifier Text SourcePos
  deriving (Eq, Ord)

conflictingIdentifier :: Parsing p => Text -> SourcePos -> p a
conflictingIdentifier = (customFailure .). ConflictingIdentifier

keywordIdentifier :: Parsing p => Text -> p a
keywordIdentifier = customFailure . KeywordIdentifier

negativeArrayLengthConst :: Parsing p => SourcePos -> p a
negativeArrayLengthConst = customFailure . NegativeArrayLengthConst

negativeArrayLengthId :: Parsing p => Text -> SourcePos -> p a
negativeArrayLengthId = (customFailure .). NegativeArrayLengthIdentifier

type ParserError = ParseError (Token Input) CustomError

instance Prelude.Show CustomError where
  show (KeywordIdentifier i) =
    "Keyword '" <> show i <> "' cannot be used as identifier"
  show (ConflictingIdentifier i pos) =
    "Identifier '" <> show i <> " has been already declared at " <> show pos
  show (NegativeArrayLengthConst pos) =
    "Only unsigned constant may be used as array size specification at " <> show pos
  show (NegativeArrayLengthIdentifier i pos) =
    "Identifier '" <> show i <> "' used as array size specification at " <> show pos
    <> " must have been declared previously as an unsigned constant"

type Parser = Parsec CustomError Input
type StatefulParser = StateT ParserState Parser
type Parsing m = ( MonadState ParserState m
                 , MonadParsec CustomError Input m
                 )
