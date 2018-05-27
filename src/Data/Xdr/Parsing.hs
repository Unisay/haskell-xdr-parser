{-# LANGUAGE ConstraintKinds #-}

module Data.Xdr.Parsing where

import           Data.Xdr.Spec
import           Protolude
import           Text.Megaparsec

type Scope = Text
type CustomError = Void
type Input = Text

type ParserState = Map Scope [Identifier]
type Parser = Parsec CustomError Input
type StatefulParser = StateT ParserState Parser
type Parsing = MonadParsec CustomError Input
