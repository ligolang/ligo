(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Tree_abstraction = struct
  let pseudo_module_to_string (c : Ligo_prim.Constant.constant') =
    match c with
    (* Operator module *)
    | C_NEG -> "Operator.neg"
    | C_ADD -> "Operator.add"
    | C_SUB -> "Operator.sub"
    | C_SUB_MUTEZ -> "Operator.sub_mutez"
    | C_POLYMORPHIC_SUB -> "Operator.sub"
    | C_MUL -> "Operator.times"
    | C_DIV -> "Operator.div"
    | C_MOD -> "Operator.modulus"
    | C_EQ -> "Operator.eq"
    | C_NOT -> "Operator.not"
    | C_AND -> "Operator.and"
    | C_OR -> "Operator.or"
    | C_GT -> "Operator.gt"
    | C_GE -> "Operator.ge"
    | C_LT -> "Operator.lt"
    | C_LE -> "Operator.le"
    | C_CONS -> "Operator.cons"
    | C_NEQ -> "Operator.neq"
    (* Map module *)
    | C_MAP_ADD -> "Map.add"
    | C_MAP_REMOVE -> "Map.remove"
    (* Bitwise module *)
    | C_XOR -> "Bitwise.xor"
    | C_LSL -> "Bitwise.shift_left"
    | C_LSR -> "Bitwise.shift_right"
    | _ as c ->
      failwith
      @@ Format.asprintf "Constant not handled : %a" Ligo_prim.Constant.pp_constant' c
end

module Michelson = struct
  (*
    Most constants pass through the Spilling unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Michelson`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
   *)
  include Helpers.Michelson
  open Tezos_utils.Michelson
  open Ligo_prim.Constant

  let get_operators c : predicate option =
    match c with
    | C_ADD -> Some (simple_binary @@ prim "ADD")
    | C_SUB -> Some (simple_binary @@ prim "SUB")
    | C_SUB_MUTEZ -> Some (simple_binary @@ prim "SUB_MUTEZ")
    | C_MUL -> Some (simple_binary @@ prim "MUL")
    | C_DIV ->
      Some
        (simple_binary
        @@ seq [ prim "EDIV"; i_assert_some_msg (i_push_string "DIV by 0"); i_car ])
    | C_MOD ->
      Some
        (simple_binary
        @@ seq [ prim "EDIV"; i_assert_some_msg (i_push_string "MOD by 0"); i_cdr ])
    | C_NEG -> Some (simple_unary @@ prim "NEG")
    | C_OR -> Some (simple_binary @@ prim "OR")
    | C_AND -> Some (simple_binary @@ prim "AND")
    | C_XOR -> Some (simple_binary @@ prim "XOR")
    | C_LOR -> Some (simple_binary @@ prim "OR")
    | C_LAND -> Some (simple_binary @@ prim "AND")
    | C_LXOR -> Some (simple_binary @@ prim "XOR")
    | C_LSL -> Some (simple_binary @@ prim "LSL")
    | C_LSR -> Some (simple_binary @@ prim "LSR")
    | C_NOT -> Some (simple_unary @@ prim "NOT")
    | C_PAIR -> Some (simple_binary @@ prim "PAIR")
    | C_CAR -> Some (simple_unary @@ prim "CAR")
    | C_CDR -> Some (simple_unary @@ prim "CDR")
    | C_TRUE -> Some (simple_constant @@ i_push (prim "bool") (prim "True"))
    | C_FALSE -> Some (simple_constant @@ i_push (prim "bool") (prim "False"))
    | C_EQ -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "EQ" ])
    | C_NEQ -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "NEQ" ])
    | C_LT -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "LT" ])
    | C_LE -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "LE" ])
    | C_GT -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "GT" ])
    | C_GE -> Some (simple_binary @@ seq [ prim "COMPARE"; prim "GE" ])
    | C_UPDATE -> Some (simple_ternary @@ prim "UPDATE")
    | C_SOME -> Some (simple_unary @@ prim "SOME")
    | C_MAP_FIND ->
      Some
        (simple_binary @@ seq [ prim "GET"; i_assert_some_msg (i_push_string "MAP FIND") ])
    | C_MAP_FIND_OPT -> Some (simple_binary @@ prim "GET")
    | C_MAP_ADD -> Some (simple_ternary @@ seq [ dip i_some; prim "UPDATE" ])
    | C_MAP_UPDATE -> Some (simple_ternary @@ prim "UPDATE")
    | C_MAP_GET_AND_UPDATE | C_BIG_MAP_GET_AND_UPDATE ->
      Some (simple_ternary @@ seq [ prim "GET_AND_UPDATE"; prim "PAIR" ])
    | C_CONS -> Some (simple_binary @@ prim "CONS")
    | C_UNIT -> Some (simple_constant @@ prim "UNIT")
    | C_SET_MEM -> Some (simple_binary @@ prim "MEM")
    | C_SET_ADD ->
      Some
        (simple_binary @@ seq [ dip (i_push (prim "bool") (prim "True")); prim "UPDATE" ])
    | C_SET_REMOVE ->
      Some
        (simple_binary @@ seq [ dip (i_push (prim "bool") (prim "False")); prim "UPDATE" ])
    | C_SET_UPDATE -> Some (simple_ternary @@ prim "UPDATE")
    | C_CONCAT -> Some (simple_binary @@ prim "CONCAT")
    | C_CONCATS -> Some (simple_unary @@ prim "CONCAT")
    | C_SLICE ->
      Some
        (simple_ternary @@ seq [ prim "SLICE"; i_assert_some_msg (i_push_string "SLICE") ])
    | C_NONE -> Some (trivial_special "NONE")
    | C_NIL -> Some (trivial_special "NIL")
    | C_LOOP_CONTINUE -> Some (trivial_special "LEFT")
    | C_LOOP_STOP -> Some (trivial_special "RIGHT")
    | C_LIST_EMPTY -> Some (trivial_special "NIL")
    | C_SET_EMPTY -> Some (trivial_special "EMPTY_SET")
    | C_MAP_EMPTY -> Some (trivial_special "EMPTY_MAP")
    | C_BIG_MAP_EMPTY -> Some (trivial_special "EMPTY_BIG_MAP")
    | C_LIST_SIZE -> Some (trivial_special "SIZE")
    | C_SET_SIZE -> Some (trivial_special "SIZE")
    | C_MAP_SIZE -> Some (trivial_special "SIZE")
    | C_SIZE -> Some (trivial_special "SIZE")
    | C_MAP_MEM -> Some (simple_binary @@ prim "MEM")
    | C_MAP_REMOVE ->
      Some (special (fun with_args -> seq [ dip (with_args "NONE"); prim "UPDATE" ]))
    | C_LEFT -> Some (trivial_special "LEFT")
    | C_RIGHT -> Some (trivial_special "RIGHT")
    | C_CREATE_CONTRACT ->
      Some (special (fun with_args -> seq [ with_args "CREATE_CONTRACT"; i_pair ]))
    | C_GLOBAL_CONSTANT -> Some (special (fun with_args -> with_args "PUSH"))
    | _ -> None
end
