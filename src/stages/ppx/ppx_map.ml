open Ppxlib
open Simple_utils

let rec extract_ident = function
  | Lident id -> id
  | Ldot (_, id) -> id
  | Lapply (_, lid) -> extract_ident lid


let extract_payload = function
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_desc =
                    Pexp_tuple
                      [ { pexp_desc = Pexp_ident id; _ }
                      ; { pexp_desc = Pexp_constant (Pconst_string (rid, _, _)); _ }
                      ]
                ; _
                }
              , _ )
        ; _
        }
      ] -> Some (extract_ident id.txt, [ rid ])
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_desc =
                    Pexp_tuple
                      [ { pexp_desc = Pexp_ident id; _ }
                      ; { pexp_desc = Pexp_tuple l; _ }
                      ]
                ; _
                }
              , _ )
        ; _
        }
      ] ->
    let f = function
      | { pexp_desc = Pexp_constant (Pconst_string (id, _, _)); _ } -> Some id
      | _ -> None
    in
    let l = List.filter_map l ~f in
    Some (extract_ident id.txt, l)
  | _ -> None


let replace_exprs id new_id =
  let replace l =
    let l = Str.global_replace (Str.regexp_string id) new_id l in
    let l =
      Str.global_replace
        (Str.regexp_string (String.uppercase id))
        (String.uppercase new_id)
        l
    in
    l
  in
  object
    inherit Ast_traverse.map as super

    method! pattern_desc pat =
      let pat = super#pattern_desc pat in
      match pat with
      | Ppat_var v -> Ppat_var { v with txt = replace v.txt }
      | _ -> pat

    method! longident lid =
      let lid = super#longident lid in
      let rec aux = function
        | Lident l -> Lident (replace l)
        | Ldot (lid, label) -> Ldot (aux lid, replace label)
        | Lapply (lid, lid') -> Lapply (aux lid, aux lid')
      in
      aux lid
  end


let map_exprs =
  object
    inherit Ast_traverse.map as super

    method! structure_item_desc =
      function
      | Pstr_value (flg, exprs) ->
        let f expr =
          let expr = super#value_binding expr in
          let attr =
            List.find_map expr.pvb_attributes ~f:(function
                | { attr_name; attr_payload; _ } when String.equal attr_name.txt "map" ->
                  Some attr_payload
                | _ -> None)
          in
          let attr = Option.bind attr ~f:extract_payload in
          Option.value_map attr ~default:[ expr ] ~f:(fun (id, ids) ->
              List.map ids ~f:(fun new_id -> (replace_exprs id new_id)#value_binding expr))
        in
        let exprs = List.concat_map exprs ~f in
        Pstr_value (flg, exprs)
      | sid -> sid
  end


let () =
  Ppxlib.Driver.register_transformation
    "ppx_map"
    ~impl:map_exprs#structure
    ~intf:map_exprs#signature
