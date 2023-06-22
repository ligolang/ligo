module Errors = Errors
open Errors
module Signature = Context.Signature
module O = Ast_typed
module C = Computation
open O.Combinators
open Ligo_prim

let default_built_entrypoint = "$main"

let default_built_entrypoint_var =
  Value_var.of_input_var ~loc:Location.generated default_built_entrypoint


let default_views = "$views"
let default_views_var = Value_var.of_input_var ~loc:Location.generated default_views
let default_contract = "$contract"
let default_contract_var = Value_var.of_input_var ~loc:Location.generated default_contract
let default_parameter = "$parameter"

let default_parameter_var =
  Type_var.of_input_var ~loc:Location.generated default_parameter


(* [check_entries s e] returns true if all entries from signature s are present in e *)
let check_entries
    : Signature.t -> Value_var.t list -> [ `All_found | `Not_found of Value_var.t ]
  =
 fun sig_ e ->
  let f s =
    match s with
    | Signature.S_value (var, _, attr) when attr.entry ->
      if List.mem ~equal:Value_var.equal e var then None else Some var
    | _ -> None
  in
  match List.find_map ~f sig_ with
  | None -> `All_found
  | Some e -> `Not_found e


let program_sig_ : Signature.t -> (Signature.item list, _, _) C.t =
 fun sig_ ->
  let is_entry s =
    match s with
    | Signature.S_value (var, ty, attr) when attr.entry -> Some (var, ty)
    | _ -> None
  in
  let open C.Let_syntax in
  match List.Ne.of_list_opt @@ List.filter_map ~f:is_entry sig_ with
  | None -> return []
  | Some entries ->
    let%bind parameter_type, storage_type =
      match Type.parameter_from_entrypoints entries with
      | Error (`Not_entry_point_form ep_type) ->
        C.raise
          (corner_case @@ Format.asprintf "Not an entrypoint form: %a" Type.pp ep_type)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        C.raise
          (corner_case
          @@ Format.asprintf
               "@[<hv>Storage types do not match for different entrypoints:@.- %a : \
                %a@.- %a : %a@]"
               Value_var.pp
               ep_1
               Type.pp
               storage_1
               Value_var.pp
               ep_2
               Type.pp
               storage_2)
      | Ok (p, s) -> return (p, s)
    in
    let type_binder = default_parameter_var in
    let parameter_type_decl = Signature.S_type (type_binder, parameter_type) in
    let contract_type = Type.build_entry_type parameter_type storage_type in
    let contract_decl =
      Signature.S_value (default_built_entrypoint_var, contract_type, Context.Attr.default)
    in
    let views_type = Type.t_views ~loc:Location.generated storage_type () in
    let views_decl =
      Signature.S_value (default_views_var, views_type, Context.Attr.default)
    in
    let mcontract_type =
      Type.t_pair ~loc:Location.generated contract_type views_type ()
    in
    let mcontract_decl =
      Signature.S_value (default_contract_var, mcontract_type, Context.Attr.default)
    in
    return [ parameter_type_decl; contract_decl; views_decl; mcontract_decl ]


let make_main_signature (sig_ : Signature.t) =
  let open C.Let_syntax in
  let%bind postfix = program_sig_ sig_ in
  return @@ sig_ @ postfix
