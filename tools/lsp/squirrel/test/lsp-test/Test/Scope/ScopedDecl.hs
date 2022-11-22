module Test.Scope.ScopedDecl
  ( unit_access_field
  ) where

import Prelude hiding (Type)

import Test.Tasty.HUnit (Assertion)

import AST.Scope.ScopedDecl (Type (..), TypeDeclSpecifics (..), TypeField (..), accessField)
import Range (Range, point)

import Test.Common.FixedExpectations (shouldBe)

stubRange :: Range
stubRange = point 0 0

stubTspec :: TypeDeclSpecifics Type
stubTspec = TypeDeclSpecifics stubRange (AliasType "stupid-type")

unit_access_field :: Assertion
unit_access_field = do
  let tspec =
        TypeDeclSpecifics stubRange
          (RecordType [TypeField "stupid-name" $ Just stubTspec])
      accessor = Right "stupid-name"
  accessField tspec accessor `shouldBe` Just stubTspec
