module Language.LIGO.Debugger.CLI.Types.LigoValue.Codegen
  ( module Language.LIGO.Debugger.CLI.Types.LigoValue.Codegen
  ) where

import Data.Singletons (Sing, withSingI)
import Fmt.Buildable (pretty)
import Fmt.Utils (Doc)
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Morley.Michelson.Text (mkMTextCut, mt)
import Morley.Michelson.Typed (SingI (sing), SingT (..))
import Morley.Michelson.Typed qualified as T

import Duplo (Cofree ((:<)), fastMake, layer, match)

import Language.LIGO.AST.Pretty hiding (Doc)
import Language.LIGO.AST.Skeleton
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Debugger.CLI.Types

{-

  Let's describe a pipeline. We have Michelson values annotated with LIGO types (@[(LigoType, SomeValue)]@).
  To get a LIGO value we should create the next contract:

  @
  let () = Test.unset_print_values ()

  let () =
    type t = *place for LIGO type* in
    let val = Test.parse_michelson {| *place for Michelson value* |} in
    let dec : t = Test.decompile val in
    Test.print (Test.to_json dec)
  @

  Then we should run it with "ligo run test". We'll get a JSON-formatted LIGO value.

  Some corner cases:
  1. Michelson doesn't have value representation for operations. Thus they can't be decompiled directly.
     We're doing the next hack: replacing operation values with prettified strings. This way allows us
     to decompile operations like strings.

  2. Addresses in @morley@ have entrypoints but addresses from LIGO don't. It means that
     "addr%ep" couldn't be decompiled into LIGO address value. We handle this by replacing
     @address@ LIGO type with @unit contract@ one. Note that we don't care about inner type
     in this @contract@ because we're decompiling a Michelson value and after this don't
     use this type marker.

  3. Types in the source mapper are monomorphed but sometimes we can see type variables.
     At this moment we noticed them only in inner types of @contract@. This bug is
     present in ligo-0.59.0. As we said in (2), we don't care about inner types, so, we
     can just replace this variable with @unit@ type.

  4. There is no much sense in decompiling function values. We can replace them with string "<fun>"
     like with operations.

-}

-- | Type family that replaces operations and lambdas with strings
type family ReplaceOps (t :: T.T) :: T.T where
  ReplaceOps 'T.TOperation = 'T.TString
  ReplaceOps ('T.TLambda _ _) = 'T.TString
  ReplaceOps ('T.TOption t) = 'T.TOption (ReplaceOps t)
  ReplaceOps ('T.TList t) = 'T.TList (ReplaceOps t)
  ReplaceOps ('T.TPair l r) = 'T.TPair (ReplaceOps l) (ReplaceOps r)
  ReplaceOps ('T.TOr l r) = 'T.TOr (ReplaceOps l) (ReplaceOps r)
  ReplaceOps t = t

replaceOpSing :: forall (t :: T.T). Sing t -> Sing (ReplaceOps t)
replaceOpSing = \case
  STOperation -> STString
  STOption t -> STOption (replaceOpSing t)
  STList t -> STList (replaceOpSing t)
  STPair l r -> STPair (replaceOpSing l) (replaceOpSing r)
  STOr l r -> STOr (replaceOpSing l) (replaceOpSing r)
  STLambda _ _ -> STString
  STKey -> STKey
  STUnit -> STUnit
  STSignature -> STSignature
  STChainId -> STChainId
  STSet t -> STSet t
  STContract t -> STContract t
  STTicket t -> STTicket t
  STMap t t' -> STMap t t'
  STBigMap t t' -> STBigMap t t'
  STInt -> STInt
  STNat -> STNat
  STString -> STString
  STBytes -> STBytes
  STMutez -> STMutez
  STBool -> STBool
  STKeyHash -> STKeyHash
  STBls12381Fr -> STBls12381Fr
  STBls12381G1 -> STBls12381G1
  STBls12381G2 -> STBls12381G2
  STTimestamp -> STTimestamp
  STAddress -> STAddress
  STChest -> STChest
  STChestKey -> STChestKey
  STSaplingState nat -> STSaplingState nat
  STSaplingTransaction nat -> STSaplingTransaction nat
  STNever -> STNever

-- | Replaces operation and lambda values with their string representations
replaceOps :: forall t. Sing t -> T.Value t -> T.Value (ReplaceOps t)
replaceOps sng val = case (val, sng) of
  (T.VOp op, STOperation) -> T.VString (mkMTextCut $ pretty op)
  (T.VOption mValue, STOption t) ->
    withSingI (replaceOpSing t) T.VOption (replaceOps t <$> mValue)
  (T.VList lst, STList t) ->
    withSingI (replaceOpSing t) T.VList (replaceOps t <$> lst)
  (T.VPair (l, r), STPair tl tr) -> T.VPair (replaceOps tl l, replaceOps tr r)
  (T.VOr eLR, STOr tl tr) -> case eLR of
    Left l ->
      withSingI (replaceOpSing tl) $ withSingI (replaceOpSing tr) $ T.VOr (Left $ replaceOps tl l)
    Right r ->
      withSingI (replaceOpSing tl) $ withSingI (replaceOpSing tr) $ T.VOr (Right $ replaceOps tr r)
  (T.VKey key, _) -> T.VKey key
  (T.VUnit, _) -> T.VUnit
  (T.VSignature sig, _) -> T.VSignature sig
  (T.VChainId cId, _) -> T.VChainId cId
  (T.VSet st, _) -> T.VSet st
  (T.VContract addr liftSeq, _) -> T.VContract addr liftSeq
  (T.VTicket addr v nat, _) -> T.VTicket addr v nat
  (T.VLam{}, _) -> T.VString [mt|<fun>|]
  (T.VMap mp, _) -> T.VMap mp
  (T.VBigMap natMb mp, _) -> T.VBigMap natMb mp
  (T.VInt n, _) -> T.VInt n
  (T.VNat n, _) -> T.VNat n
  (T.VString str, _) -> T.VString str
  (T.VBytes bts, _) -> T.VBytes bts
  (T.VMutez mutez, _) -> T.VMutez mutez
  (T.VBool bl, _) -> T.VBool bl
  (T.VKeyHash keyHash, _) -> T.VKeyHash keyHash
  (T.VTimestamp time, _) -> T.VTimestamp time
  (T.VAddress addr, _) -> T.VAddress addr
  (T.VBls12381Fr bts, _) -> T.VBls12381Fr bts
  (T.VBls12381G1 bts, _) -> T.VBls12381G1 bts
  (T.VBls12381G2 bts, _) -> T.VBls12381G2 bts
  (T.VChest ch, _) -> T.VChest ch
  (T.VChestKey chKey, _) -> T.VChestKey chKey

-- | Replace subtypes in the @LIGO@ one.
replaceLigoType :: LIGO Info -> LIGO Info
replaceLigoType (match -> Just (i, TArrow{})) = fastMake i $ TypeName "string"
replaceLigoType typeName@(match -> Just (i, TypeName t)) =
  case toString t of
    "operation" -> fastMake i $ TypeName "string"
    -- Addresses in @morley@ have entrypoints but @LIGO@ ones don't. Let's replace
    -- them with @unit contract@ type which represents like an address with entrypoint.
    "address" -> fastMake i (TApply (fastMake i $ TypeName "contract") [fastMake i $ TypeName "unit"])
    _ -> typeName
replaceLigoType (match -> Just (i, app@(TApply t _))) =
  case layer @TypeName t of
    -- There is a bug in @ligo-0.59.0@ with contract's inner variable.
    -- It's not monomorphed. It was mentioned above that we don't care
    -- about this variable, so, we can replace it with a known one (like @unit)@.
    Just (TypeName (toString -> "contract"))
      -> fastMake i (TApply t [fastMake i $ TypeName "unit"])
    _ -> fastMake i $ fmap replaceLigoType app
replaceLigoType (i :< fs) = (i :< fmap replaceLigoType fs)

-- | Generate a contract that decompiles one Michelson value.
generateDecompilation' :: LigoType -> T.SomeValue -> Doc
generateDecompilation' ligoType (T.SomeValue (val :: T.Value t)) = case ligoType of
  LigoType (Just expr) ->
    let
      replacedTypeNode = LTFResolved expr
        & fromLigoTypeFull
        & replaceLigoType

      isType = isJust (layer @AST.Type replacedTypeNode) || isJust (layer @TypeName replacedTypeNode)
      replacedVal = replaceOps (sing @t) val
      typ = show @Doc $ lppDialect Caml replacedTypeNode
    in
      if not isType
      then noDecompilation
      else
        [int||
        let () =
          type t = #{typ} in
          let v = Test.parse_michelson {| #{replacedVal} |} in
          let dec : t = Test.decompile v in
          Test.print (Test.to_json dec)
        |]
  _ -> noDecompilation
  where
    noDecompilation =
      [int||
      let () = Test.print "null"
      |]

-- | Generate a contract that decompiles a bunch of Michelson values.
-- After executing this contract will return a @JSON@ list with
-- @JSON@ representations of @LIGO@ values.
generateDecompilation :: [(LigoType, T.SomeValue)] -> Doc
generateDecompilation typesAndValues =
  [int||
  let () = Test.unset_print_values ()

  let () = Test.print "["
  #{mconcat $ intersperse comma (uncurry generateDecompilation' <$> typesAndValues)}
  let () = Test.print "]"
  |]
  where
    comma =
      [int||
      let () = Test.print ","
      |]
