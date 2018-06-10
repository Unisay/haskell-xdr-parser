module Data.Xdr.Types where

import           Data.List.NonEmpty
import           Protolude


newtype Identifier
  = Identifier Text
  deriving (Eq, Ord, Show)

newtype IdentifierRef
  = IdentifierRef Text
  deriving (Eq, Ord, Show)

data Declaration
  = DeclarationSingle TypeSpecifier Identifier
  | DeclarationArrayFixLen TypeSpecifier Identifier Value
  | DeclarationArrayVarLen TypeSpecifier Identifier (Maybe Value)
  | DeclarationOpaqueFixLen Identifier Value
  | DeclarationOpaqueVarLen Identifier (Maybe Value)
  | DeclarationString Identifier (Maybe Value)
  | DeclarationOptional TypeSpecifier Identifier
  | DeclarationVoid
  deriving (Eq, Ord, Show)

data Discriminant
  = DiscriminantInt Identifier
  | DiscriminantUnsignedInt Identifier
  | DiscriminantBool Identifier
  | DiscriminantEnum Identifier
  deriving (Eq, Ord, Show)

type Value = Either Constant IdentifierRef

data Constant
  = DecConstant Integer
  | HexConstant Integer
  | OctConstant Integer
  deriving (Eq, Ord, Show)

data TypeSpecifier
  = TypeInt
  | TypeUnsignedInt
  | TypeHyper
  | TypeUnsignedHyper
  | TypeFloat
  | TypeDouble
  | TypeQuadruple
  | TypeBool
  | TypeEnum EnumBody
  | TypeStruct StructBody
  | TypeUnion UnionBody
  | TypeIdentifier IdentifierRef
  deriving (Eq, Show, Ord)

type EnumBody = NonEmpty (Identifier, Value)

type StructBody = NonEmpty Declaration

data UnionBody = UnionBody
  { unionDiscriminant :: Discriminant
  , unionArms         :: NonEmpty CaseSpec
  , unionDefault      :: Maybe Declaration
  }
  deriving (Eq, Ord, Show)

data CaseSpec
  = CaseSpec
  { caseSpecValues      :: NonEmpty Value
  , caseSpecDeclaration :: Declaration
  }
  deriving (Eq, Ord, Show)

data ConstantDef
  = ConstantDef Identifier Constant
  deriving (Eq, Ord, Show)

data TypeDef
  = TypeDef Declaration
  | TypeDefEnum Identifier EnumBody
  | TypeDefStruct Identifier StructBody
  | TypeDefUnion Identifier UnionBody
  deriving (Eq, Ord, Show)

type Definition = Either TypeDef ConstantDef

type Specification = [Definition]
