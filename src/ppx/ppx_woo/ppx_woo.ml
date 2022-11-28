module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module Parse = Woo_parse

module Make (Params : Woo_helpers.PARAMS) = struct
  module Generate = struct
    module Matching = Woo_matching.Make (Params)
    module Cases = Woo_cases.Make (Params)
    module Constructors = Woo_constructors.Make (Params)
    module Properties = Woo_properties.Make (Params)
    module Record = Woo_record.Make (Params)
    module Variant = Woo_variant.Make (Params)
    module Helpers = Woo_helpers.Generate (Params)

    let declaration_of_prefix (name, body) = Helpers.declaration ~name ~body

    let str ~prefixes ?wrap_constructor ?wrap_get ?wrap_map ?default_get
        : P.rec_flag * P.type_declaration list -> P.structure_item list
      =
     fun (rec_flag, tds) ->
      let non_recursive = Parse.non_recursive rec_flag in
      let tds' = Parse.type_declarations ~non_recursive tds in
      let aux : W.type_declaration -> P.structure_item list =
       fun td ->
        let W.{ labelled_type = label, ty; non_recursive = _ } = td in
        let prefix = if String.(label = "t") then "" else label ^ "_" in
        match ty with
        | W.T_variant variant ->
          let wrap_constructor =
            match wrap_constructor with
            | Some (name, body) when String.(name = label) -> Some body (* once told me *)
            | _ -> None
          in
          let wrap_get =
            match wrap_get with
            | Some (name, body) when String.(name = label) -> Some body (* once told me *)
            | _ -> None
          in
          let wrap_map =
            match wrap_map with
            | Some (name, body) when String.(name = label) -> Some body (* once told me *)
            | _ -> None
          in
          let cons_types = Constructors.constructor_types variant in
          let is = Matching.matching_is_full ?wrap_get variant in
          let exn = Matching.matching_exn_full ?wrap_get variant in
          let opt = Matching.matching_opt_full ?wrap_get variant in
          let def = Matching.matching_default ?default_get variant in
          let cons = Constructors.constructors ?wrap_constructor variant in
          let mappers = Cases.mappers ?wrap_map variant in
          let setters = Cases.setters ?wrap_map variant in
          let destruct = Variant.destruct prefix variant in
          let destruct_tpl = Variant.destruct_tpl prefix variant in
          []
          @ cons_types
          @ cons
          @ mappers
          @ setters
          @ is
          @ exn
          @ opt
          @ def
          @ destruct
          @ destruct_tpl
        | W.T_record record ->
          let prop_types = Properties.property_types record in
          let props = Properties.properties record in
          let mappers = Properties.mappers record in
          let setters = Properties.setters record in
          let maker = Record.make prefix record in
          let maker_tpl = Record.make_tpl prefix record in
          let destruct_tpl = Record.destruct_tpl prefix record in
          [] @ prop_types @ props @ mappers @ setters @ maker @ maker_tpl @ destruct_tpl
        | W.T_core _ -> []
      in
      let itemss = List.map ~f:aux tds' in
      let items = List.concat itemss in
      let items = List.map ~f:declaration_of_prefix prefixes @ items in
      let items =
        List.filter ~f:(fun x -> not @@ Helpers.is_tauto_type_declaration x) items
      in
      items
  end
end

let make_PARAMS loc : (module Woo_helpers.PARAMS) =
  (module struct
    let location = loc
  end : Woo_helpers.PARAMS)


let args =
  P.Deriving.Args.(
    empty
    +> arg "prefix" (pack2 (pexp_tuple (estring __ ^:: __ ^:: nil)))
    +> arg "prefixes" (elist @@ pack2 (pexp_tuple (estring __ ^:: __ ^:: nil)))
    +> arg "wrap_constructor" (pack2 (pexp_tuple (estring __ ^:: __ ^:: nil)))
    +> arg "wrap_get" (pack2 (pexp_tuple (estring __ ^:: __ ^:: nil)))
    +> arg "wrap_map" (pack2 (pexp_tuple (estring __ ^:: __ ^:: nil)))
    +> arg "default_get" (pexp_variant __ none))


let deriver =
  let str_type_decl =
    let generate
        ~loc
        ~path:_
        input
        prefix
        prefixes
        wrap_constructor
        wrap_get
        wrap_map
        default_get
      =
      let prefixes =
        let prefix =
          match prefix with
          | Some x -> [ x ]
          | None -> []
        in
        let prefixes = Option.value prefixes ~default:[] in
        prefix @ prefixes
      in
      let module Full = Make ((val make_PARAMS loc)) in
      Full.Generate.str ~prefixes ?wrap_constructor ?wrap_get ?wrap_map ?default_get input
    in
    P.Deriving.Generator.make args generate
  in
  P.Deriving.add "ez" ~str_type_decl
