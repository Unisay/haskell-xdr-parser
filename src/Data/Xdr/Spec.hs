module Data.Xdr.Spec where

import           Data.List.NonEmpty
import           Protolude


newtype Identifier
  = Identifier Text
  deriving (Eq, Show)

data Declaration
  = DeclarationSingle TypeSpecifier Identifier
  | DeclarationArrayFixLen TypeSpecifier Identifier Value
  | DeclarationArrayVarLen TypeSpecifier Identifier (Maybe Value)
  | DeclarationOpaqueFixLen Identifier Value
  | DeclarationOpaqueVarLen Identifier (Maybe Value)
  | DeclarationString Identifier (Maybe Value)
  | DeclarationOptional TypeSpecifier Identifier
  | DeclarationVoid

type Value = Either Constant Identifier

data Constant
  = DecConstant Integer
  | HexConstant Integer
  | OctConstant Integer

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
  | TypeIdentifier Identifier

type EnumBody = NonEmpty (Identifier, Value)

type StructBody = NonEmpty Declaration

data UnionBody = UnionBody
  { unionDefault      :: Declaration
  , unionDiscriminant :: Declaration
  , unionArms         :: NonEmpty CaseSpec
  }

data CaseSpec
  = CaseSpec
  { caseSpecValues      :: NonEmpty Value
  , caseSpecDeclaration :: Declaration
  }

data ConstantDef = ConstantDef Identifier Constant

data TypeDef
  = TypeDef Declaration
  | TypeDefEnum Identifier EnumBody
  | TypeDefStruct Identifier StructBody
  | TypeDefUnion Identifier UnionBody

type Definition = Either TypeDef ConstantDef

type Specification = [Definition]
