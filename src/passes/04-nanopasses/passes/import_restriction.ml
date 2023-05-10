open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise =
  let declaration : _ declaration_ -> declaration =
   fun d ->
    let loc = Location.get_location d in
    match Location.unwrap d with
    | D_import i as d ->
      let ret = make_d ~loc d in
      let open Import in
      (match i with
      | Import_all_as _ | Import_selected _ -> raise.error (unsupported_import ret)
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
        d_module ~loc { name = alias; mod_expr })
    | d -> make_d ~loc d
  in
  Fold { idle_fold with declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_import _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
