open Tezos_micheline
open Micheline

open Memory_proto_alpha.Protocol
include Michelson_v1_primitives

type michelson = (int, prim) node
type t = michelson

let prim ?(annot=[]) ?(children=[]) p : michelson =
  Prim (0, p, children, annot)

let annotate annot = function
  | Prim (l, p, c, []) -> Prim (l, p, c, [annot])
  | _ -> raise (Failure "annotate")

let seq s : michelson = Seq (0, s)


let contract parameter storage code =
  seq [
    prim ~children:[parameter] K_parameter ;
    prim ~children:[storage] K_storage ;
    prim ~children:[code] K_code ;
  ]

let int n : michelson = Int (0, n)
let string s : michelson = String (0, s)
let bytes s : michelson = Bytes (0, s)

let t_unit = prim T_unit
let t_string = prim T_string
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
let i_push_string str = i_push t_string (string str)

let i_apply = prim I_APPLY

let i_comment s : michelson = seq [ i_push_string s ; prim I_DROP ]

let i_none ty = prim ~children:[ty] I_NONE
let i_nil ty = prim ~children:[ty] I_NIL
let i_empty_set ty = prim ~children:[ty] I_EMPTY_SET
let i_iter body = prim ~children:[body] I_ITER
let i_map body = prim ~children:[body] I_MAP
let i_some = prim I_SOME
let i_lambda arg ret body = prim ~children:[arg;ret;body] I_LAMBDA
let i_empty_map src dst = prim ~children:[src;dst] I_EMPTY_MAP
let i_empty_big_map src dst = prim ~children:[src;dst] I_EMPTY_BIG_MAP
let i_drop = prim I_DROP
let i_dropn n = prim I_DROP ~children:[int (Z.of_int n)]
let i_exec = prim I_EXEC

let i_if a b = prim ~children:[seq [a] ; seq[b]] I_IF
let i_if_none a b = prim ~children:[seq [a] ; seq[b]] I_IF_NONE
let i_if_cons a b = prim ~children:[seq [a] ; seq[b]] I_IF_CONS
let i_if_left a b = prim ~children:[seq [a] ; seq[b]] I_IF_LEFT
let i_failwith = prim I_FAILWITH
let i_assert_some = i_if_none (seq [i_push_string "ASSERT_SOME" ; i_failwith]) (seq [])
let i_assert_some_msg msg = i_if_none (seq [msg ; i_failwith]) (seq [])

let dip code : michelson = prim ~children:[seq [code]] I_DIP
let dipn n code = prim ~children:[Int (0 , Z.of_int n) ; seq [code]] I_DIP
let i_dig n : michelson = prim ~children:[Int (0 , Z.of_int n)] I_DIG
let i_dug n : michelson = prim ~children:[Int (0 , Z.of_int n)] I_DUG
let i_unpair = seq [i_dup ; i_car ; dip i_cdr]
let i_unpiar = seq [i_dup ; i_cdr ; dip i_car]

let rec strip_annots : michelson -> michelson = function
  | Seq(l, s) -> Seq(l, List.map strip_annots s)
  | Prim (l, p, lst, _) -> Prim (l, p, List.map strip_annots lst, [])
  | x -> x

let pp ppf (michelson:michelson) =
  let open Micheline_printer in
  let canonical = strip_locations michelson in
  let node = printable string_of_prim canonical in
  print_expr ppf node

let pp_json ppf (michelson : michelson) =
  let open Micheline_printer in
  let canonical = strip_locations michelson in
  let node = printable string_of_prim canonical in
  let json = Tezos_data_encoding.(
      Json.construct
        (Micheline.erased_encoding ~variant:"???" {comment = None} Data_encoding.string)
        node
    )
  in
  Format.fprintf ppf "%a" Tezos_data_encoding.Json.pp json

let pp_hex ppf (michelson : michelson) =
  let canonical = strip_locations michelson in
  let bytes = Tezos_data_encoding.Binary_writer.to_bytes_exn Script_repr.expr_encoding canonical in
  let hex = Hex.of_bytes bytes in
  Format.fprintf ppf "%a" Hex.pp hex
