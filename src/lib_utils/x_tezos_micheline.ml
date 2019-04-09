include Tezos_micheline

module Michelson = struct
  open Micheline
  include Memory_proto_alpha.Michelson_v1_primitives

  type michelson = (int, prim) node
  type t = michelson

  let prim ?(annot=[]) ?(children=[]) p : michelson =
    Prim (0, p, children, annot)

  let annotate annot = function
    | Prim (l, p, c, []) -> Prim (l, p, c, [annot])
    | _ -> raise (Failure "annotate")

  let seq s : michelson = Seq (0, s)

  let i_comment s : michelson = prim ~annot:["\"" ^ s ^ "\""] I_NOP

  let int n : michelson = Int (0, n)
  let string s : michelson = String (0, s)
  let bytes s : michelson = Bytes (0, s)

  let t_unit = prim T_unit
  let t_pair a b = prim ~children:[a;b] T_pair
  let t_lambda a b = prim ~children:[a;b] T_lambda

  let d_unit = prim D_Unit
  let d_pair a b = prim ~children:[a;b] D_Pair

  let i_dup = prim I_DUP
  let i_car = prim I_CAR
  let i_cdr = prim I_CDR
  let i_pair = prim I_PAIR
  let i_swap = prim I_SWAP
  let i_piar = seq [ i_swap ; i_pair ]
  let i_push ty code = prim ~children:[ty;code] I_PUSH
  let i_push_unit = i_push t_unit d_unit
  let i_none ty = prim ~children:[ty] I_NONE
  let i_some = prim I_SOME
  let i_lambda arg ret body = prim ~children:[arg;ret;body] I_LAMBDA
  let i_drop = prim I_DROP

  let i_if a b = prim ~children:[a;b] I_IF
  let i_if_none a b = prim ~children:[a;b] I_IF_NONE
  let i_failwith = prim I_FAILWITH
  let i_assert_some = i_if_none (seq [i_failwith]) (seq [])

  let dip code : michelson = prim ~children:[seq [code]] I_DIP
  let i_unpair = seq [i_dup ; i_car ; dip i_cdr]
  let i_unpiar = seq [i_dup ; i_cdr ; dip i_car]

  let rec strip_annots : michelson -> michelson = function
    | Seq(l, s) -> Seq(l, List.map strip_annots s)
    | Prim (l, p, lst, _) -> Prim (l, p, List.map strip_annots lst, [])
    | x -> x

  let rec strip_nops : michelson -> michelson = function
    | Seq(l, s) -> Seq(l, List.map strip_nops s)
    | Prim (l, I_NOP, _, _) -> Seq (l, [])
    | Prim (l, p, lst, a) -> Prim (l, p, List.map strip_nops lst, a)
    | x -> x

  let pp ppf (michelson:michelson) =
    let open Micheline_printer in
    let canonical = strip_locations michelson in
    let node = printable string_of_prim canonical in
    print_expr ppf node

  let pp_naked ppf m =
    let naked = strip_annots m in
    pp ppf naked
end
