open Trace
open Tezos_micheline.Micheline
open Memory_proto_alpha.Protocol.Michelson_v1_primitives

let strip_annots = fun e ->
  match e with
  | Prim (l , p , lst , _) -> ok @@ Prim (l , p , lst , [])
  | x -> ok x

let strip_nops = fun e ->
  match e with
  | Seq(l, [Prim (_, I_UNIT, _, _) ; Prim(_, I_DROP, _, _)]) -> ok @@ Seq (l, [])
  | x -> ok x


let all = [
  strip_annots ;
  strip_nops ;
]

let rec bind_chain : ('a -> 'a result) list -> 'a -> 'a result = fun fs x ->
  match fs with
  | [] -> ok x
  | hd :: tl -> (
      let aux : 'a -> 'a result = fun x -> bind (bind_chain tl) (hd x) in
      bind aux (ok x)
    )

let all_expression =
  let all_expr = List.map Helpers.map_expression all in
  bind_chain all_expr
