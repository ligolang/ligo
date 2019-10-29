open Trace

let all = [
  Tezos_type_annotation.peephole_expression ;
  None_variant.peephole_expression ;
  Literals.peephole_expression ;
]

let rec bind_chain : ('a -> 'a result) list -> 'a -> 'a result = fun fs x ->
  match fs with
  | [] -> ok x
  | hd :: tl -> (
      let aux : 'a -> 'a result = fun x -> bind (bind_chain tl) (hd x) in
      bind aux (ok x)
    )

let all_program =
  let all_p = List.map Helpers.map_program all in
  bind_chain all_p

let all_expression =
  let all_p = List.map Helpers.map_expression all in
  bind_chain all_p

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression
