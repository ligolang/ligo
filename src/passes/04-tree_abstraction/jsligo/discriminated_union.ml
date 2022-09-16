open Errors
open Simple_utils.Trace

module CST = Cst.Jsligo
module Region = Simple_utils.Region
module Utils = Simple_utils.Utils

type disc_variant = {
  constructor: string;
  constructor_field: string;
  has_payload: bool;
  fields: string list;
}

let disc_unions: disc_variant list list ref = ref []

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let has_constructor (variants: disc_variant list) (variant_name: string) : bool = 
  let rec aux = function
    {constructor; _} :: _ when String.equal constructor variant_name -> true
  | _ :: tl -> aux tl
  | [] -> false
  in
  aux variants

let find_disc_union (variants: string list) = 
  let variants_length = List.length variants in
  let rec aux = function
    item :: tl when List.length item = variants_length ->
      let is_disc_union = List.fold_left ~f:(fun a i -> a && has_constructor item i) ~init:true variants in
      if is_disc_union then 
        Some item
      else 
        aux tl
  | _ :: tl -> aux tl
  | [] -> None
  in
  aux !disc_unions

let has_field (variant: disc_variant) (field_name, field_value) : bool = 
  List.mem variant.fields field_name ~equal:String.equal || (String.equal variant.constructor_field field_name && String.equal variant.constructor field_value)

let is_disc_union (a: disc_variant list) (fields: (string * string) list) =
  let rec aux = function
    variant :: tl when (List.length variant.fields + 1) = List.length fields -> (
      let result = List.fold_left ~f:(fun a i -> a && has_field variant i) ~init:true fields in
      if result then
        Some variant
      else aux tl
    )
  | _ :: tl -> aux tl
  | [] -> None
  in 
  aux a

let find_variant (fields: (string * string) list) = 
  let rec aux = function 
    disc_union :: tl -> 
      let result = is_disc_union disc_union fields in
      (match result with 
        Some s -> Some s
      | None -> aux tl)
  | [] -> None
  in
  aux !disc_unions

let is_discriminated_union (a: CST.switch Region.reg) =
  if Poly.(!disc_unions = []) then 
    None
  else (
    let rec switch_fields (check, result) = function 
      (CST.Switch_case {expr = EString (String s); _}) :: remaining ->
        switch_fields (check, s.value :: result) remaining
    | _ :: _ -> false, []
    | [] -> (check, List.rev result)
    in 
    let (check1, values) = switch_fields (true, []) (nseq_to_list a.value.cases) in
    if not(check1) then 
      None
    else
      (* check if length is the same and if one disc_union type is correct *)
      find_disc_union values
  )

let find_disc_obj (a: CST.object_expr) =
  let rec aux fields = function
    CST.Property {value = {name = EVar v; value; _}; _} :: remaining ->
    aux ((v.value, match value with EString (String {value = s; _}) -> s | _ -> "" ) :: fields) remaining
  | [] -> true, List.rev fields
  | _ -> false, []
  in
  let check, fields = aux [] (npseq_to_list a.value.inside) in
  if check then 
    find_variant fields
  else
    None

let get_shared_field ~raise (n: (CST.obj_type, CST.vbar) Utils.nsepseq) = 
  let shared_fields = Utils.nsepseq_foldl (fun shared_fields (obj: CST.obj_type) -> 
    let fields = obj.value.ne_elements in
    Utils.nsepseq_foldl (fun shared_fields ({value; _}: CST.field_decl Region.reg)  -> 
      let field_type = value.field_type in
      let field_name = value.field_name.value in
      match shared_fields, field_type with 
        [],      TString s -> (field_name, s.value) :: shared_fields
      | _ :: _, TString s 
          when List.mem shared_fields (field_name, s.value) ~equal:(fun (a, a_value) (b, b_value) -> String.equal a b && not (String.equal a_value b_value)) ->
            (field_name, s.value) :: shared_fields
      | _             -> shared_fields
    ) shared_fields fields  
  ) [] n
  in
  let shared_fields = List.rev shared_fields in
  let shared_field = match shared_fields with 
    [] -> raise.error @@ no_shared_fields (CST.nsepseq_to_region (fun (r: CST.obj_type) -> r.region) n)
  | (hd, _) :: _ -> hd
  in
  shared_field

let add disc_union =
  disc_unions := disc_union :: !disc_unions
        