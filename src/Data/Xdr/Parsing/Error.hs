module Data.Xdr.Parsing.Error where

import qualified Prelude
import           Protolude
import           Text.Megaparsec
import Data.Xdr.Types

data CustomError
  = KeywordIdentifier Text
  | ConflictingIdentifier Text SourcePos
  | NegativeArrayLengthConst SourcePos
  | NegativeArrayLengthIdentifier Text SourcePos
  | InvalidDiscriminant SourcePos
  | InvalidDiscriminantType TypeSpecifier SourcePos
  deriving (Eq, Ord)

conflictingIdentifier
  :: MonadParsec CustomError s m
  => Text
  -> SourcePos
  -> m a
conflictingIdentifier =
  (customFailure .). ConflictingIdentifier

keywordIdentifier :: MonadParsec CustomError s m => Text -> m a
keywordIdentifier = customFailure . KeywordIdentifier

negativeArrayLengthConst
  :: MonadParsec CustomError s m
  => SourcePos
  -> m a
negativeArrayLengthConst =
  customFailure . NegativeArrayLengthConst

negativeArrayLengthId
  :: MonadParsec CustomError s m
  => Text
  -> SourcePos
  -> m a
negativeArrayLengthId =
  (customFailure .). NegativeArrayLengthIdentifier

invalidDiscriminant :: MonadParsec CustomError s m => SourcePos -> m a
invalidDiscriminant = customFailure . InvalidDiscriminant

invalidDiscriminantType
  :: MonadParsec CustomError s m
  => TypeSpecifier
  -> SourcePos
  -> m a
invalidDiscriminantType = (customFailure .). InvalidDiscriminantType

instance Prelude.Show CustomError where
  show (KeywordIdentifier i) =
    "Keyword " <> show i <> " cannot be used as identifier"
  show (ConflictingIdentifier i pos) =
    "Identifier " <> show i <> " has been already declared at " <> sourcePosPretty pos
  show (NegativeArrayLengthConst pos) =
    "Only unsigned constant may be used as array size specification at " <> sourcePosPretty pos
  show (NegativeArrayLengthIdentifier i pos) =
    "Identifier " <> show i <> " used as array size specification at " <> sourcePosPretty pos
    <> " must have been declared previously as an unsigned constant"
  show (InvalidDiscriminant pos) =
    "Union descriminant doesn't evaluate to a known type at " <> sourcePosPretty pos
  show (InvalidDiscriminantType _ pos) =
    "Union descriminant doesn't evaluate to an integer type "
    <> "(\"int\", \"unsigned int\", \"bool\", \"enum\") at " <> sourcePosPretty pos

instance ShowErrorComponent CustomError where
  showErrorComponent = Prelude.show