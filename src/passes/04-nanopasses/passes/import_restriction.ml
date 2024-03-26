open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let is_mangled mvar =
  String.is_prefix ~prefix:"Mangled_module_" (Mod_variable.to_name_exn mvar)


let compile ~raise =
  (* given a declaration, we extract all visibility attributes (in reverse order) *)
  let rec extract_visibility : _ -> _ =
   fun d ->
    match d with
    | D_attr ((Attribute.{ key = "public" | "private"; value = _ } as attr), d) ->
      let d = get_d d in
      let vattrs, d = extract_visibility d in
      vattrs @ [ attr ], d
    | D_attr (attr, d) ->
      let loc = get_d_loc d in
      let d = get_d d in
      let vattrs, d = extract_visibility d in
      vattrs, D_attr (attr, make_d ~loc @@ d)
    | _ -> [], d
  in
  let declaration : _ declaration_ -> declaration =
   fun d ->
    let loc = Location.get_location d in
    match Location.unwrap d with
    | D_import i as d ->
      let ret = make_d ~loc d in
      let open Import in
      (match i with
      | Import_all_as _ | Import_selected _ -> raise.error (unsupported_import ret)
      | Import_rename { module_path = imported_module, []; alias }
        when is_mangled imported_module ->
        (* Only keep the import around if it is a module alias introduced by #import *)
        d_import
          ~loc
          (Import_rename { alias; module_path = List.Ne.singleton imported_module })
      | Import_rename { alias; module_path } ->
        let mod_expr =
          let loc =
            List.Ne.fold
              (fun acc x -> Location.cover acc (Mod_variable.get_location x))
              Location.generated
              module_path
          in
          m_path ~loc module_path
        in
        let wrap d = make_d ~loc @@ D_attr ({ key = "public"; value = None }, d) in
        wrap
        @@ d_module
             ~loc
             { name = alias; mod_expr; annotation = { signatures = []; filter = false } })
    | D_module { name; mod_expr; annotation = _ } as d ->
      (match get_m mod_expr with
      | M_var alias when is_mangled alias ->
        (* Translate [module <Name> = <Mangled module>] to [import <Name> = <Mangled module>] *)
        d_import
          ~loc
          (Import_rename { alias = name; module_path = List.Ne.singleton alias })
      | _ -> make_d ~loc d)
    | d -> make_d ~loc d
  in
  let program_entry
      : (program_entry, declaration, instruction) program_entry_ -> program_entry
    =
   fun e ->
    match e with
    | PE_declaration d ->
      let loc = get_d_loc d in
      (match extract_visibility (get_d @@ d) with
      | [], dc -> pe_declaration @@ make_d ~loc dc
      | attr :: _, dc -> pe_declaration @@ make_d ~loc @@ D_attr (attr, make_d ~loc dc))
    | e -> make_pe e
  in
  Fold { idle_fold with declaration; program_entry }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_import (Import_rename { module_path = mvar, _; _ }); _ }
        when not (is_mangled mvar) -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
