{-# LANGUAGE ConstraintKinds #-}

module Data.Xdr.Parsing where

import           Data.Xdr.ParserState
import           Data.Xdr.Parsing.Error
import           Protolude
import           Text.Megaparsec

type Input = Text
type ParserError = ParseError (Token Input) CustomError
type Parser = Parsec CustomError Input
type StatefulParser = StateT ParserState Parser
type Parsing m = ( MonadState ParserState m
                 , MonadParsec CustomError Input m
                 )
