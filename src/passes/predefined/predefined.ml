(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Tree_abstraction = struct

  open Ast_imperative

  let some_const c = Some (Const c)

  (*
    Each front-end has its owns constants.

    Constants are special names that have their own case in the AST. E_constant
    for regular constants, and T_constant for type constants. Both types are
    defined in `Ast_core/types.ml`.
    For instance, "2 + 2" in Pascaligo is translated to `E_constant ("ADD" , [
      E_literal (Literal_int 2) ;
      E_literal (Literal_int 2) ;
    ])`.

    They are used to represent what can't expressed in the languages:
    - Primitives. Like "int", "string", "unit" for types. Or "+" for values.
    - Tezos specific stuff. Like "operation" for types. Or "source" for values.
    - What can't be represented in the language yet. Like "list" or "List.fold".

    Each constant is expressed as a pair:
    - The left-hand-side is the reserved name in the given front-end.
    - The right-hand-side is the name that will be used in the AST.
  *)
  let pseudo_modules x =
    match x with
    | "Tezos.self"               -> some_const C_SELF
    | "Tezos.create_contract"    -> some_const C_CREATE_CONTRACT
    | "Tezos.get_entrypoint_opt" -> some_const C_CONTRACT_ENTRYPOINT_OPT
    | "Tezos.get_entrypoint"     -> some_const C_CONTRACT_ENTRYPOINT
    | "Tezos.call_view"          -> some_const C_VIEW

    (* Sapling *)
    | "Tezos.sapling_empty_state" -> some_const C_SAPLING_EMPTY_STATE
    | "Tezos.sapling_verify_update" -> some_const C_SAPLING_VERIFY_UPDATE

    (* Options module *)
    | "Option.map"              -> some_const C_OPTION_MAP

    (* Set module *)
    | "Set.literal"    -> some_const C_SET_LITERAL

    (* Map module *)
    | "Map.literal"  -> some_const C_MAP_LITERAL

    (* Big_map module *)
    | "Big_map.literal"  -> some_const C_BIG_MAP_LITERAL

    (* Bitwise module *)
    | "Bitwise.or"          -> some_const C_OR
    | "Bitwise.and"         -> some_const C_AND

    (* Operator module *)
    | "Operator.neg"   -> some_const C_NEG
    | "Operator.add"   -> some_const C_ADD
    | "Operator.sub"   -> some_const C_POLYMORPHIC_SUB
    | "Operator.sub_mutez" -> some_const C_SUB_MUTEZ
    | "Operator.times" -> some_const C_MUL
    | "Operator.div"   -> some_const C_DIV
    | "Operator.modulus" -> some_const C_MOD
    | "Operator.eq"    -> some_const C_EQ
    | "Operator.not"   -> some_const C_NOT
    | "Operator.and"   -> some_const C_AND
    | "Operator.or"    -> some_const C_OR
    | "Operator.gt"    -> some_const C_GT
    | "Operator.ge"    -> some_const C_GE
    | "Operator.lt"    -> some_const C_LT
    | "Operator.le"    -> some_const C_LE
    | "Operator.cons"  -> some_const C_CONS
    | "Operator.neq"   -> some_const C_NEQ

    | _ -> None


  let pseudo_module_to_string = function
    | C_CHAIN_ID                -> "Tezos.chain_id"
    | C_BALANCE                 -> "Tezos.balance"
    | C_NOW                     -> "Tezos.now"
    | C_AMOUNT                  -> "Tezos.amount"
    | C_SENDER                  -> "Tezos.sender"
    | C_ADDRESS                 -> "Tezos.address"
    | C_SELF                    -> "Tezos.self"
    | C_SELF_ADDRESS            -> "Tezos.self_address"
    | C_IMPLICIT_ACCOUNT        -> "Tezos.implicit_account"
    | C_SOURCE                  -> "Tezos.source"
    | C_FAILWITH                -> "Tezos.failwith"
    | C_CREATE_CONTRACT         -> "Tezos.create_contract"
    | C_CALL                    -> "Tezos.transaction"
    | C_SET_DELEGATE            -> "Tezos.set_delegate"
    | C_CONTRACT_WITH_ERROR     -> "Tezos.get_contract_with_error"
    | C_CONTRACT_OPT            -> "Tezos.get_contract_opt"
    | C_CONTRACT_ENTRYPOINT_OPT -> "Tezos.get_entrypoint_opt"
    | C_CONTRACT                -> "Tezos.get_contract"
    | C_CONTRACT_ENTRYPOINT     -> "Tezos.get_entrypoint"
    | C_NEVER                   -> "Tezos.never"
    | C_OPEN_CHEST              -> "Tezos.open_chest"
    | C_VIEW                    -> "Tezos.call_view"
    | C_GLOBAL_CONSTANT         -> "Tezos.constant"

    (* Operator module *)
    | C_NEG  -> "Operator.neg"
    | C_ADD  -> "Operator.add"
    | C_SUB  -> "Operator.sub"
    | C_SUB_MUTEZ -> "Operator.sub_mutez"
    | C_POLYMORPHIC_SUB -> "Operator.sub"
    | C_MUL  -> "Operator.times"
    | C_DIV  -> "Operator.div"
    | C_MOD  -> "Operator.modulus"
    | C_EQ   -> "Operator.eq"
    | C_NOT  -> "Operator.not"
    | C_AND  -> "Operator.and"
    | C_OR   -> "Operator.or"
    | C_GT   -> "Operator.gt"
    | C_GE   -> "Operator.ge"
    | C_LT   -> "Operator.lt"
    | C_LE   -> "Operator.le"
    | C_CONS -> "Operator.cons"
    | C_NEQ  -> "Operator.neq"

    (* Set module *)
    | C_SET_LITERAL    -> "Set.literal"

    (* Map module *)
    | C_MAP_LITERAL  -> "Map.literal"
    | C_MAP_ADD      -> "Map.add"
    | C_MAP_REMOVE   -> "Map.remove"

    (* Big_map module *)
    | C_BIG_MAP_LITERAL -> "Big_map.literal"

    (* Bitwise module *)
    | C_XOR -> "Bitwise.xor"
    | C_LSL -> "Bitwise.shift_left"
    | C_LSR -> "Bitwise.shift_right"

    | _ as c -> failwith @@ Format.asprintf "Constant not handled : %a" Stage_common.PP.constant' c


  let constants x = pseudo_modules x
  let constant_to_string = function
      | Const x -> pseudo_module_to_string x
end

module Stacking = struct
  (*
    Most constants pass through the Spilling unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Stacking`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
   *)
  type protocol_type = Environment.Protocols.t
  include Helpers.Stacking
  open Tezos_utils.Michelson
  open Stage_common.Types

  let get_operators (protocol_version: protocol_type) c : predicate option =
    match c , protocol_version with
    | C_ADD                , _   -> Some ( simple_binary @@ prim "ADD")
    | C_SUB                , _   -> Some ( simple_binary @@ prim "SUB")
    | C_SUB_MUTEZ          , Ithaca -> Some ( simple_binary @@ prim "SUB_MUTEZ")
    | C_MUL                , _   -> Some ( simple_binary @@ prim "MUL")
    | C_EDIV               , _   -> Some ( simple_binary @@ prim "EDIV")
    | C_DIV                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car])
    | C_MOD                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr])
    | C_NEG                , _   -> Some ( simple_unary @@ prim "NEG")
    | C_OR                 , _   -> Some ( simple_binary @@ prim "OR")
    | C_AND                , _   -> Some ( simple_binary @@ prim "AND")
    | C_XOR                , _   -> Some ( simple_binary @@ prim "XOR")
    | C_LSL                , _   -> Some ( simple_binary @@ prim "LSL")
    | C_LSR                , _   -> Some ( simple_binary @@ prim "LSR")
    | C_NOT                , _   -> Some ( simple_unary @@ prim "NOT")
    | C_PAIR               , _   -> Some ( simple_binary @@ prim "PAIR")
    | C_CAR                , _   -> Some ( simple_unary @@ prim "CAR")
    | C_CDR                , _   -> Some ( simple_unary @@ prim "CDR")
    | C_TRUE               , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "True"))
    | C_FALSE              , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "False"))
    | C_EQ                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "EQ"])
    | C_NEQ                , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "NEQ"])
    | C_LT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LT"])
    | C_LE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LE"])
    | C_GT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GT"])
    | C_GE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GE"])
    | C_UPDATE             , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | C_SOME               , _   -> Some ( simple_unary  @@ prim "SOME")
    | C_MAP_FIND           , _   -> Some ( simple_binary @@ seq [prim "GET" ; i_assert_some_msg (i_push_string "MAP FIND")])
    | C_MAP_MEM            , _   -> Some ( simple_binary @@ prim "MEM")
    | C_MAP_FIND_OPT       , _   -> Some ( simple_binary @@ prim "GET")
    | C_MAP_ADD            , _   -> Some ( simple_ternary @@ seq [dip (i_some) ; prim "UPDATE"])
    | C_MAP_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | (C_MAP_GET_AND_UPDATE|C_BIG_MAP_GET_AND_UPDATE) , _ ->
      Some (simple_ternary @@ seq [prim "GET_AND_UPDATE"; prim "PAIR"])
    | C_SIZE                  , _   -> Some ( simple_unary @@ prim "SIZE")
    | C_FAILWITH              , _   -> Some ( simple_unary @@ prim "FAILWITH")
    | C_NEVER                 , _   -> Some ( simple_unary @@ prim "NEVER")
    | C_UNOPT                 , _   -> Some ( simple_binary @@ i_if_none (seq [i_push_string "option is None"; i_failwith]) (seq []))
    | C_UNOPT_WITH_ERROR      , _   -> Some ( simple_binary @@ i_if_none (i_failwith) (seq [ i_swap; i_drop]))
    | C_ASSERT_SOME           , _   -> Some ( simple_unary @@ i_if_none (seq [i_push_string "failed assert some" ; i_failwith]) (seq [i_drop; i_push_unit]))
    | C_ASSERT_SOME_WITH_ERROR, _   -> Some ( simple_binary @@ i_if_none (i_failwith) (seq [i_dropn 2; i_push_unit]))
    | C_ASSERT_NONE           , _   -> Some ( simple_unary @@ i_if_none (seq [i_push_unit]) (seq [i_push_string "failed assert none" ; i_failwith]))
    | C_ASSERT_NONE_WITH_ERROR, _   -> Some ( simple_binary @@ i_if_none (seq [i_drop; i_push_unit]) (seq[i_drop;i_failwith]))
    | C_ASSERT_INFERRED    , _   -> Some ( simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit]))
    | C_ASSERTION          , _   -> Some ( simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_string "failed assertion" ; i_failwith]))
    | C_ASSERTION_WITH_ERROR, _  -> Some ( simple_binary @@ i_if (seq [i_drop; i_push_unit]) (i_failwith))
    | C_INT                , _   -> Some ( simple_unary @@ prim "INT")
    | C_ABS                , _   -> Some ( simple_unary @@ prim "ABS")
    | C_IS_NAT             , _   -> Some ( simple_unary @@ prim "ISNAT")
    | C_CONS               , _   -> Some ( simple_binary @@ prim "CONS")
    | C_UNIT               , _   -> Some ( simple_constant @@ prim "UNIT")
    | C_BALANCE            , _   -> Some ( simple_constant @@ prim "BALANCE")
    | C_AMOUNT             , _   -> Some ( simple_constant @@ prim "AMOUNT")
    | C_ADDRESS            , _   -> Some ( simple_unary @@ prim "ADDRESS")
    | C_SELF_ADDRESS       , _   -> Some ( simple_constant @@ seq [prim "SELF_ADDRESS"])
    | C_IMPLICIT_ACCOUNT   , _   -> Some ( simple_unary @@ prim "IMPLICIT_ACCOUNT")
    | C_SET_DELEGATE       , _   -> Some ( simple_unary @@ prim "SET_DELEGATE")
    | C_NOW                , _   -> Some ( simple_constant @@ prim "NOW")
    | C_CALL               , _   -> Some ( simple_ternary @@ prim "TRANSFER_TOKENS")
    | C_SOURCE             , _   -> Some ( simple_constant @@ prim "SOURCE")
    | C_SENDER             , _   -> Some ( simple_constant @@ prim "SENDER")
    | C_SET_MEM            , _   -> Some ( simple_binary @@ prim "MEM")
    | C_SET_ADD            , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "True")) ; prim "UPDATE"])
    | C_SET_REMOVE         , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "False")) ; prim "UPDATE"])
    | C_SET_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE" )
    | C_SLICE              , _   -> Some ( simple_ternary @@ seq [prim "SLICE" ; i_assert_some_msg (i_push_string "SLICE")])
    | C_SHA256             , _   -> Some ( simple_unary @@ prim "SHA256")
    | C_SHA512             , _   -> Some ( simple_unary @@ prim "SHA512")
    | C_BLAKE2b            , _   -> Some ( simple_unary @@ prim "BLAKE2B")
    | C_CHECK_SIGNATURE    , _   -> Some ( simple_ternary @@ prim "CHECK_SIGNATURE")
    | C_HASH_KEY           , _   -> Some ( simple_unary @@ prim "HASH_KEY")
    | C_BYTES_PACK         , _   -> Some ( simple_unary @@ prim "PACK")
    | C_CONCAT             , _   -> Some ( simple_binary @@ prim "CONCAT")
    | C_CHAIN_ID           , _   -> Some ( simple_constant @@ prim "CHAIN_ID")
    | C_SHA3               , _   -> Some ( simple_unary @@ prim "SHA3")
    | C_KECCAK             , _   -> Some ( simple_unary @@ prim "KECCAK")
    | C_LEVEL              , _   -> Some ( simple_constant @@ prim "LEVEL")
    | C_VOTING_POWER       , _   -> Some ( simple_unary @@ prim "VOTING_POWER")
    | C_TOTAL_VOTING_POWER , _   -> Some ( simple_unary @@ prim "TOTAL_VOTING_POWER")

    | C_SELF               , _   -> Some (trivial_special "SELF")
    | C_NONE               , _   -> Some (trivial_special "NONE")
    | C_NIL                , _   -> Some (trivial_special "NIL")
    | C_LOOP_CONTINUE      , _   -> Some (trivial_special "LEFT")
    | C_LOOP_STOP          , _   -> Some (trivial_special "RIGHT")
    | C_LIST_EMPTY         , _   -> Some (trivial_special "NIL")
    | C_LIST_HEAD_OPT      , _   -> Some ( special @@ fun with_args -> i_if_cons (seq [i_swap; i_drop; i_some]) (with_args "NONE") )
    | C_LIST_TAIL_OPT      , _   -> Some ( special @@ fun with_args -> i_if_cons (seq [i_drop; i_some])       (with_args "NONE") )
    | C_SET_EMPTY          , _   -> Some (trivial_special "EMPTY_SET")
    | C_MAP_EMPTY          , _   -> Some (trivial_special "EMPTY_MAP")
    | C_BIG_MAP_EMPTY      , _   -> Some (trivial_special "EMPTY_BIG_MAP")
    | C_BYTES_UNPACK       , _   -> Some (trivial_special "UNPACK")
    | C_MAP_REMOVE         , _   -> Some (special (fun with_args -> seq [dip (with_args "NONE"); prim "UPDATE"]))
    | C_LEFT               , _   -> Some (trivial_special "LEFT")
    | C_RIGHT              , _   -> Some (trivial_special "RIGHT")
    | C_TICKET             , _ -> Some ( simple_binary @@ prim "TICKET" )
    | C_READ_TICKET        , _ -> Some ( simple_unary @@ seq [ prim "READ_TICKET" ; prim "PAIR" ] )
    | C_SPLIT_TICKET       , _ -> Some ( simple_binary @@ prim "SPLIT_TICKET" )
    | C_JOIN_TICKET        , _ -> Some ( simple_unary @@ prim "JOIN_TICKETS" )
    | C_SAPLING_EMPTY_STATE, _ -> Some (trivial_special "SAPLING_EMPTY_STATE")
    | C_SAPLING_VERIFY_UPDATE , _ -> Some (simple_binary @@ prim "SAPLING_VERIFY_UPDATE")
    | C_PAIRING_CHECK , _ -> Some (simple_binary @@ prim "PAIRING_CHECK")
    | C_CONTRACT           , _   ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_assert_some_msg (i_push_string "bad address for get_contract")]))
    | C_CONTRACT_WITH_ERROR, _   ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_if_none (i_failwith) (seq [i_swap; i_drop])]))
    | C_CONTRACT_OPT         , _   -> Some (trivial_special "CONTRACT")
    | C_CONTRACT_ENTRYPOINT , _  ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_assert_some_msg (i_push_string "bad address for get_entrypoint")]))
    | C_CONTRACT_ENTRYPOINT_OPT , _ -> Some (trivial_special "CONTRACT")
    | C_CREATE_CONTRACT , _ ->
      Some (special
              (fun with_args ->
                 seq [with_args "CREATE_CONTRACT";
                      i_pair]))
    | C_OPEN_CHEST , _ -> (
      Some (simple_ternary @@ seq [
        prim "OPEN_CHEST" ;
        i_if_left
          ( prim "RIGHT" ~children:[t_or t_unit t_unit])
          ( i_if
            (seq [ i_push_unit ; prim "LEFT" ~children:[t_unit] ; prim "LEFT" ~children:[t_bytes] ])
            (seq [ i_push_unit ; prim "RIGHT" ~children:[t_unit] ; prim "LEFT" ~children:[t_bytes] ])
          )
      ])
    )
    | C_VIEW , _ -> Some (trivial_special "VIEW")
    | C_GLOBAL_CONSTANT , _ ->
      Some (special
        (fun with_args ->  with_args "PUSH")
        )
    | _ -> None

end
